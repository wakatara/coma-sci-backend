
;; combine flat images

(in-package imred)


(defun %flatcombine-image-median-function (im median-nsample statsec)
  (compute-image-median-in-statsec im median-nsample statsec))


;; simple function to nuke stars in an input flat by eliminating pixels brighter than 10 sigma over the
;; image median - FIXME - should use local sigmas
;; NOTE - this doesn't seem to work well, so is disabled below
(defun  %flatcombine-nuke-stars (im)
  (declare (type imutils:image im))
  (multiple-value-bind (median sigma)
      (imutils:compute-sampled-image-median-and-sigma im 50000 :allow-nan t)
    (loop with xcutoff = (+ median (* 10.0 sigma))
	  for i below (array-total-size im)
	  for x of-type single-float = (row-major-aref im i)
	  when (and (not (float-utils:single-float-nan-or-infinity-p x))
		    (> x xcutoff))
	    do (setf (row-major-aref im i) float-utils:*single-float-nan*))))
  


;; FIXME - enforce dithering if possible - maybe put ra,dec into i-med list and
;; hash on them

;; return those image sections whose median pixel value is
;; within min-counts and max-counts, and rescale the images to have a
;; value of 1.  If too many images, then take the ones with most counts.
(defun %flatcombine-imsec-filter-function 
    (imsec-generator statsec  reduction-plan median-nsample
     &key
       ;; option to NaN out bright pixels.  This doesn't work well in practice.
       ;; the bad regions of sky flats are still bad, but with NaN donuts.
       (flat-type :flat) ;; or :object
       (nuke-stars nil))

  (let* ((min-counts (reduction-plan-min-flat-counts reduction-plan))
	 (max-counts (reduction-plan-max-flat-counts reduction-plan))
	 (nflat-max  (if (eq flat-type :flat)
			 (reduction-plan-max-flat-frames reduction-plan)
			 (reduction-plan-max-objflat-counts reduction-plan)))
	 (logger (reduction-plan-logger reduction-plan))
	 (ext-stuff-list ;; list of (image-number median-pix-value fits extension)
	   (loop for i from 0
		 for imsec = (funcall imsec-generator i)
		 until (not imsec)
		 for ext = (cf:fits-file-current-hdu-num
			    (cf:image-section-orig-fits-file imsec))
		 for fits = (file-io:file-minus-dir
			     (cf:fits-file-filename
			      (cf:image-section-fits-file imsec)))
		 ;; compute median, but log error if it fails
		 for median = (multiple-value-bind (med err)
				  (ignore-errors
				   (%flatcombine-image-median-function
				    (cf:image-section-data imsec)
				    median-nsample statsec))
				(cond
				  ((not med)
				   (logger:writelog
				    logger
				    (format
				     nil
				     "ERROR: in computing median of input flat ~A:~A - ~A"
				     fits ext err))
				   nil) ;; return median=nil so it isn't collected
				  (t
				   med)))
		 when median
		 collect (list i median fits ext ))))

    (let ((final-imsec-list
	    (loop with ngood = 0
		  ;; sort by brightest flux so we take best ones
		  for ext-stuff in (sort ext-stuff-list '> :key 'second) 
		  for i = (first ext-stuff) and med = (second ext-stuff)
		  and fits = (file-io:file-minus-dir (third ext-stuff))
		  and ext = (1- (fourth ext-stuff))
		  for counts-hi = (> med max-counts)
		  for counts-lo = (< med min-counts)
		  until (= ngood nflat-max)
		  do (when counts-hi
		       (logger:writelog
			logger
			(format nil "ERROR: Input flat ~A:~A rejected: counts ~A>~A"
				fits ext  med max-counts)))
		     (when counts-lo
		       (logger:writelog
			logger
			(format nil "Input flat ~A:~A rejected: counts ~A<~A"
				fits ext med min-counts)))
		     ;;
		  when (not (or counts-lo counts-hi))
		    do (incf ngood)
		       (logger:writelog
			logger
		(format nil "Input flat ~A:~A accepted: counts = ~A"
			fits ext med))
	    and
	      ;; scale the image to a median of 1, and return
	      collect  (let* ((imsec (funcall imsec-generator i))
			      (im (cf:image-section-data imsec)))
			 (imutils:im-scale im (/ 1.0 med) :image-out im)
			 imsec))))

      (when nuke-stars
	(dolist (imsec final-imsec-list)
	  (%flatcombine-nuke-stars (cf:image-section-data imsec))))

      final-imsec-list)))
      
			     

;; a clipping-function for imutils:image-stack used for stacking darksky flats
;; to try to suppress stars - for now just clip top 30% of frames when n>7
(defun %darksky-top-fraction-reject-pixel-filter (v vscr n)
  (declare (type (simple-array single-float (*)) v vscr)
	   (ignore vscr)
	   (type (integer 1 1000000) n)
	   (optimize speed))
   (shellsort-jk:shellsort-macro 
	    (<= (aref v i) (aref v j))
	    (rotatef (aref v i) (aref v j))
	    0 (1- n) i j)
  (if (< n 8)
      n ;; no 
      (round (* 0.7 n)))) ;; keep 80%

 
 
(defun flatcombine (fits-list fits-out
		    &key
		      (reduction-plan *default-reduction-plan*)
		      (stack-type :median)
		      (template-fits nil)
		      (flat-type :flat) ;; :flat or :object
		      (if-exists :error)
		      (median-nsample 30000))
  "Median combine flat fields, removing those with a median
that is not in range [MIN-COUNTS, MAX-COUNTS]."

  (declare (type list fits-list)
	   (type (or string pathname) fits-out)
	   (type (unsigned-byte 28) median-nsample)
	   (type (member :flat :object) flat-type))
  (when (not fits-list)
    (error "No fits images given to FLATCOMBINE."))
  (let ((minval
	  (float (reduction-plan-min-final-flat-value reduction-plan) 1.0))
	(maxval
	  (float (reduction-plan-max-final-flat-value reduction-plan) 1.0))
	(nullval
	  (float (reduction-plan-output-null-pixel-value reduction-plan) 1.0)))

    ;;
    (flet ((final-array-function (im) ;; fix the final flat to be
				      ;; within the range specified
	     (declare (type (simple-array single-float (* *)) im))
	     (loop for i of-type fixnum below (array-total-size im)
		for x of-type single-float = (row-major-aref im i)
		when (or (float-nan-p x) (< x minval) (> x maxval))
		do (setf (row-major-aref im i) nullval)))
	   ;;
	   (imsec-filter-function (imsec-generator extdesc)
	      (%flatcombine-imsec-filter-function 
	       imsec-generator
	       (extdesc-statsec extdesc)
	       reduction-plan
	       median-nsample
	       :flat-type flat-type)))
	         
      (stack-images fits-list
		    fits-out   
		    :reduction-plan reduction-plan
		    :template-fits template-fits
		    :clipping-function (if (eq flat-type :object)
					   #'%darksky-top-fraction-reject-pixel-filter)
		    :stack-type stack-type
		    :count-header "IMRED.NFLAT"
		    :if-exists if-exists		 
		    :final-array-function #'final-array-function
		    :imsec-filter-function #'imsec-filter-function)))
  ;;
  (cf:write-fits-header fits-out "IMRED.CAL" T
			:comment "This is an IMRED calib file"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if we are using images as the flats, we have to clean up the list

;; go through a flat file, and find the biggest image section, and
;; return its median counts (and sigma)
(defun %roughly-estimate-flat-counts (fits-file)
  (let ((biggest-ed 
	 (loop
	    with bigsize = 1
	    with edbig = nil
	    for ed in (build-extdesc-list-for-fits fits-file)
	    for image-size = (extdesc-image-size ed)
	    when  (extdesc-reduce-p ed)
	    do
	      (let ((size (* (aref image-size 0) (aref image-size 1))))
		(when (> size bigsize)
		  (setf bigsize size)
		  (setf edbig ed)))
	    finally (return edbig))))
    (cf:with-open-fits-file (fits-file ff)
      (cf:move-to-extension ff (extdesc-n-ext biggest-ed))
      (let ((imsec (cf:read-image-section ff)))
	(imutils:compute-sampled-image-median-and-sigma 
	 (cf:image-section-data imsec) 3000)))))

;; go through and return a subset of images that are bright enough,
;; spacing them through the list (assumed temporal)
(defun %clean-up-object-flat-list (reduction-plan flat-list)
  ;; unmark any of these flats as being used as object flat, in case this is a repeat run
  (dolist (fits flat-list)
    (cf:delete-fits-header fits "OBJFLAT"))
  ;;
  (let*
      ;; most flats allowed
      ((nmax  (reduction-plan-maximum-number-objflat-images reduction-plan))
       ;; counts in each flat image
       (count-list (mapcar '%roughly-estimate-flat-counts flat-list))
       (good-fits
	(loop 
	   for counts in count-list
	   for fits in flat-list
	   when (<= (reduction-plan-min-objflat-counts reduction-plan)
		    counts
		    (reduction-plan-max-objflat-counts reduction-plan))
	   collect fits))
       ;;
       (ngood (length good-fits)))

    
    (when (< ngood (reduction-plan-min-flat-frames reduction-plan))
      (imred-log-note 
       (format nil "WARNING: When cleaning up object flat (skyflat) list, only ~A inputs remain but require ~A to build flat."
	       ngood (reduction-plan-min-flat-frames reduction-plan))))
       ;;
    (let*
	((nskip (max 1 (round (/ ngood nmax))))
	 (selected-fits
	   (loop 
	     with ipicked = 0
	     for fits in good-fits
	     for i from 0
	     when (and (zerop (mod i nskip))
		       (< ipicked nmax))
	       collect fits
	       and
		 do (incf ipicked))))

      (imred-log-note
       (format nil "Picked ~A input object (darksky) inputs from ~A total, ~A satisfying counts, and maximum allowed ~A"
	       (length selected-fits)
	       (length flat-list)
	       (length good-fits)
	       nmax))
      
      selected-fits)))
	  
       
       

