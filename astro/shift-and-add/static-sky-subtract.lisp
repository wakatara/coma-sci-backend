#|
  background sky subtraction - this is an instance of an image-preproc

  This has the special option pre-subtract-fits and pre-subtract-radius
  that pre-subtracts an image from the input files before they used to 
  construct a background sky.

  This allows an initial estimate of the object to be deleted 
  from the images before constructing a cleaner sky, useful
when the object doesn't move enough to generate a clean sky.


|#

(in-package shift-and-add)

(defclass static-sky-subtract-preproc (image-preproc)
  ((cleanup :initarg :cleanup :initform t
	    :accessor static-sky-subtract-preproc-cleanup)
   ;; delete the individual sky files (projected to parent fits wcs)
   (delete-sky-files :initarg :delete-sky-files :initform nil
		     :accessor static-sky-subtract-preproc-delete-sky-files)
   ;; do we mask the object itself to a radius of MASK-RADIUS (in pix)?
   (mask-object :initarg :mask-object :initform nil
		:accessor static-sky-subtract-preproc-mask-object)
   (mask-radius :initarg :mask-radius :initform 20 ;; pixels
		:accessor static-sky-subtract-preproc-mask-radius)
   ;; an image to shift and pre-subtract before constructing the sky - this might be
   ;; a first estimate of the object, to create a better sky; ie, do a two-stage
   ;; sky subtraction.
   (pre-subtract-fits :initarg :pre-subtract-fits
		      :accessor static-sky-subtract-preproc-pre-subtract-fits
		      :initform nil)
   ;; what radius (pixels) to do the fit on when subtracting
   (pre-subtract-radius  :initarg :pre-subtract-radius
			 :accessor static-sky-subtract-preproc-pre-subtract-radius
			 :initform 50.0)
   ;; filter the average object before subtracting to avoid building up coherent noise
   (pre-subtract-filter-scale  :initarg :pre-subtract-filter-scale
			 :accessor static-sky-subtract-preproc-pre-subtract-filter-scale
			 :initform 9)))
    

(defmethod run-image-preproc ((image-preproc static-sky-subtract-preproc)
			      (saaplan saaplan)
			      fits-working-list)
  (subtract-static-sky-from-fits-list
   saaplan image-preproc fits-working-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; for a fits file FITS, generate a background sky file and return its name
;;
(defun generate-static-sky-file-for-fits (saaplan statsky-preproc
					  fits full-sky-fits)
  (declare (ignorable saaplan))
  (let* ((skyname ;; name the sky differently if we're doing a pre-subtract
	   (if (static-sky-subtract-preproc-pre-subtract-fits statsky-preproc)
	       "skyPS" "sky"))
	 (backd-fits-base (format nil "~A_~A" (file-io:file-basename fits) skyname))
	 (backd-fits-dir (format nil "~A_DIR/" backd-fits-base))
	 (n-ext (get-image-extension fits))
	 (headfile   (format nil "~A_~A.bhead" (file-io:file-basename fits) skyname))
	 (naxis1 (cf:read-fits-header fits "NAXIS1" :extension n-ext))
	 (naxis2 (cf:read-fits-header fits "NAXIS2" :extension n-ext))
	 (wcs (cf:read-wcs fits  :extension n-ext )))
    (unwind-protect
	 (progn
	   (make-head-file-from-wcs headfile wcs)
	   (terapix:run-swarp
	    full-sky-fits backd-fits-base
	    :image-size (vector naxis1 naxis2)
	    :header-suffix "bhead"
	    :resampling-type (saaplan-swarp-resampling-type saaplan)
	    :copy-keywords
	    (cons (saaplan-mjd-keyword saaplan)
		  (instrument-id:get-critical-headers-for-fits
		   fits))
	    :image-size (list naxis1 naxis2)
	    :combine-type "MAX")
	   (format nil "~A.fits" backd-fits-base)) ;; return the name

      ;; cleanup if asked
      (when (static-sky-subtract-preproc-cleanup statsky-preproc)
	(uiop:delete-directory-tree
	 (pathname backd-fits-dir) ;; requires pathname, not namestring
	 :validate t ;; safety feature
	 :if-does-not-exist :ignore)
	(let ((weight-file (format nil "~A.weight.fits" backd-fits-base)))
	  (when (probe-file weight-file)
		(delete-file weight-file)))
	(delete-file headfile)))))

#+nil
(defun %hashify-headers (header-list)
   (loop with hash = (make-hash-table :test 'equal)
	 for header in header-list
	 for key = (first header) and val = (second header)
	 do (setf (gethash key hash) header)
	    finally (return hash)))

(defun subtract-static-sky-from-fits-file (saaplan statsky-preproc
					   fits sky-stack-fits outfits)
  (let* ((sky-fits  (generate-static-sky-file-for-fits 
		     saaplan statsky-preproc fits sky-stack-fits))
	 (imsec-sky (cf:read-image-section  sky-fits))
	 (data-sky  (cf:image-section-data imsec-sky))
	 (n-ext (get-image-extension fits)))

    (cf:with-open-fits-file (fits ff :mode :input)
      (cf:move-to-extension ff  n-ext)
      (let* ((naxis1 (cf:read-fits-header ff "NAXIS1")) ;; x
	     (naxis2 (cf:read-fits-header ff "NAXIS2")) ;; y
	     ;(wcs (cf:read-wcs ff))
	     ;; buffer around edges to ignore (edge messiness)
	     (nbuf1 (round (* naxis1 0.15)))
	     (nbuf2 (round (* naxis2 0.15)))
	     (imsec-fits (cf:read-image-section ff))
	     (data-fits  (cf:image-section-data imsec-fits)))


	;; modify array data-fits in place
	(multiple-value-bind (image-out sky-scale im-backd imsky-backd neval)
	    (imutils:subtract-static-sky 
	     data-fits data-sky
	     :auto-backd t
	     ;; we want background left in these images to get photometric
	     ;; errors if we want to use them for photometry
	     :subtract-backd-from-final-result nil 
	     :imout data-fits ;; overwrite
	     :ix0 nbuf1 :ix1  (- naxis1 nbuf1)
	     :iy0 nbuf2 :iy1  (- naxis2 nbuf2))
	  (declare (ignorable image-out sky-scale im-backd imsky-backd neval)
		   (type imutils:image image-out))
	  #+nil
	  (format *error-output*
		  "SKY-SCALE: ~A IM-BACKD: ~A  IMSKY-BACKD: ~A NEVAL: ~A~%"
		  sky-scale im-backd imsky-backd neval)
	  ;; clean up any out of range pixels
	  (loop for i below (array-total-size image-out)
		for x of-type single-float = (row-major-aref image-out i)
		when (and (not (float-utils:single-float-nan-p x))
			  (or (float-utils:single-float-infinity-p x)
			      (> (abs x) (* 0.1 most-positive-single-float))))
		  do (setf (row-major-aref image-out i) 
			   float-utils:*single-float-nan*)))



	(file-io:copy-file fits outfits :overwrite t)

	;; first clobber any headers that might disagree with
	;; the data we're about to write, and close file
	(cf:with-open-fits-file (outfits ffout :mode :io)
	  (cf:move-to-extension ffout n-ext)
	  (cf:write-fits-header ffout "BITPIX" -32) ;; single float
	  (cf:delete-fits-header ffout "BZERO")
	  (cf:delete-fits-header ffout "BSCALE"))
	;; reopen, and write data
	(cf:with-open-fits-file (outfits ffout :mode :io)
	  (cf:move-to-extension ffout n-ext)
	  (cf:write-image-section  ffout imsec-fits :reckless t))
	  
	  
	
	(when (static-sky-subtract-preproc-delete-sky-files statsky-preproc)
	  (delete-file sky-fits))
	
	outfits))))
	
#+nil
(defun subtract-static-sky-from-fits-file (saaplan statsky-preproc
					   fits sky-stack-fits outfits)
  (let* ((sky-fits  (generate-static-sky-file-for-fits 
		     saaplan statsky-preproc fits sky-stack-fits))
	 (imsec-sky (cf:read-image-section  sky-fits))
	 (hlist0 nil)
	 (data-sky  (cf:image-section-data imsec-sky))
	 (n-ext (instrument-id:get-image-extension-for-onechip-fits fits)))

    (cf:with-open-fits-file (fits ff :mode :input)
      (setf hlist0 (cf:read-fits-header-list fits)) ;; the zeroth extension headers
      (cf:move-to-extension ff  n-ext)
      (let* ((naxis1 (cf:read-fits-header ff "NAXIS1")) ;; x
	     (naxis2 (cf:read-fits-header ff "NAXIS2")) ;; y
	     (wcs (cf:read-wcs fits))
	     (hlist-im (cf:read-fits-header-list ff)) ;; the image headers
	     (hlist nil) ;; the final cleaned headers
	     ;; buffer around edges to ignore (edge messiness)
	     (nbuf1 (round (* naxis1 0.15)))
	     (nbuf2 (round (* naxis2 0.15)))
	     (imsec-fits (cf:read-image-section ff))
	     (data-fits  (cf:image-section-data imsec-fits)))


	;; modify array data-fits in place
	(imutils:subtract-static-sky 
	 data-fits data-sky
	 :auto-backd t
	 ;; we want background left in these images to get photometric
	 ;; errors if we want to use them for photometry
	 :subtract-backd-from-final-result nil 
	 :imout data-fits ;; overwrite
	 :ix0 nbuf1 :ix1  (- naxis1 nbuf1)
	 :iy0 nbuf2 :iy1  (- naxis2 nbuf2))

	;; now combine the header lists - headers from image extension take precedence
	(if (= n-ext 1)
	    (setf hlist hlist0) ;; just use hlist 0
	    ;; else combine
	    (setf hlist
		  (let ((hash-im (%hashify-headers hlist-im))
			(outlist nil))
		    (loop for header in hlist0
			  for key0 = (first header)
			  if  (not (gethash key0 hash-im))
			    do
			       (push header outlist) ;; use the main header
			  else ;; substitute the image ext header, and remove it
			  do (progn (push (gethash key0 hash-im) outlist)
				    (remhash key0 hash-im)))
		    ;; now collect the remaining headers in the image ext
		    (loop for header in hlist-im
			  for key1 = (first header)
			  when (gethash key1 hash-im) ;; still in hash-im
			    do (push header outlist))
		    (nreverse outlist))))


	(file-io:copy-file fits outfits :overwrite t)
	(cf:with-new-fits-file (outfits ffout :overwrite t :make-primary-headers nil)
	  (loop for head in hlist
		for key = (first head) and val = (second head) and comment = (third head)
		;; modify some headers
		for realvalue =  (cond ((equalp key "BITPIX") -32) ;; output is float
				       (t val))
		do
		   (cf:write-fits-header ffout key realvalue :comment comment))
	  (cf:write-image-section  ffout imsec-fits :reckless t)
	  (cf:write-wcs wcs ffout))
	  
	  
	
	(when (static-sky-subtract-preproc-delete-sky-files statsky-preproc)
	  (delete-file sky-fits))
	
	outfits))))
	
	
	


(defun subtract-static-sky-from-fits-list (saaplan statsky-preproc
					   fits-list)
  (let* ((fits-list-2 ;; if we are pre-subtracting the object itself we build a new set of fits files
	   (if (not (static-sky-subtract-preproc-pre-subtract-fits statsky-preproc))
	       fits-list
	       (build-sssp-presub-fits-files saaplan statsky-preproc
					     fits-list)))
	       
	 (sky-stack-fits (if (static-sky-subtract-preproc-mask-object statsky-preproc)
			     ;; need to build a special stationary stack
			     (build-masked-stationary-stack saaplan
							    statsky-preproc
							    fits-list-2)
			     ;; use the default one
			     (progn
			       (format t "Building stationary stack from ~A~%~%" fits-list-2)
			     (build-stationary-stack saaplan fits-list-2
						     :force-rebuild t)))))
    
    ;; now use the ORIGINAL fits list to make the object beacause fits-list-2 has the object
    ;; subtracted out
    (loop for fits in fits-list
	  for outfits = (format nil "~A_skysub.fits" (file-io:file-basename fits))
	  do (subtract-static-sky-from-fits-file
	      saaplan statsky-preproc fits sky-stack-fits outfits)
	  collect outfits)))
					  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make a stationary stack, but with masking of the object
(defun build-masked-stationary-stack (saaplan statsky-preproc fits-list)
  (let* ((stack-title "maskedbackdsky")
	 (stack-name (make-stationary-stack-name saaplan
						 :name stack-title
						 :append-suffix t))
	 (weight-suffix ".BMweight.fits")) ;; backd-masked


    (loop for fits in fits-list
	  do (%make-masked-stationary-sky-input-weight
	      fits saaplan statsky-preproc weight-suffix))
    
    (terapix:run-swarp  
     fits-list
     (make-stationary-stack-name saaplan
				 :name stack-title
				 :append-suffix nil)
     :verbose-type (saaplan-verbose-type saaplan)
     :output nil 
     :weight-type "MAP_WEIGHT"
     :weight-suffix weight-suffix
     :combine-type "median"
     :resampling-type (saaplan-swarp-resampling-type saaplan)
     :copy-keywords (cons (saaplan-mjd-keyword saaplan)
			  (instrument-id:get-critical-headers-for-fits
			   (first fits-list)))
     :header-suffix "head")
    ;;
    stack-name))


;; make one input weight file that masks the object location
(defun %make-masked-stationary-sky-input-weight
    (fits saaplan statsky-preproc weight-suffix)

  (let (ra dec xpix ypix
	   (wcs (cf:read-wcs
		 fits
		 :extension (instrument-id:get-image-extension-for-onechip-fits fits))))

    (multiple-value-setq (ra dec)
      (get-object-location-in-fits-file (saaplan-locator saaplan) saaplan fits))
    
    (multiple-value-setq (xpix ypix)
      (wcs:wcs-convert-ra-dec-to-pix-xy wcs ra dec))

    
    (let* ((maskfits (concatenate 'string (file-io:file-basename fits) weight-suffix))
	   (nx (cf:read-fits-header fits "NAXIS1"))
	   (ny (cf:read-fits-header fits "NAXIS2"))
	   (im (make-array (list ny nx) :element-type '(unsigned-byte 16)
					:initial-element 2))
	   (r (float (static-sky-subtract-preproc-mask-radius statsky-preproc) 1.0)))

      (mask-circle-in-int16-image (round xpix) (round ypix) im r 0)

      (cf:write-2d-image-to-new-fits-file im maskfits :type :short :overwrite t))))

      

      

      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; build and return a new fits-list with the
;; static-sky-subtract-preproc-pre-subtract-fits file subtracted
;; from the fits files (after shifting)
(defun build-sssp-presub-fits-files (saaplan statsky-preproc
				     fits-list)
  (loop for fits in fits-list
	for outfits = (format nil "~A_presub.fits" (file-io:file-basename fits))
	do (build-one-sspp-presub-fits
	    saaplan statsky-preproc fits outfits)
	collect outfits))
 
;; subtract the object in (static-sky-subtract-preproc-pre-subtract-fits statsky-preproc)
;; and put result in outfits
(defun build-one-sspp-presub-fits (saaplan statsky-preproc fits-file outfits)
  (let* ((pfits (static-sky-subtract-preproc-pre-subtract-fits statsky-preproc))
	 (pfits-name (format nil "presub_~A" (file-io:file-basename (file-io:file-minus-dir fits-file))))
	 (shifted-pfits-base
	   (concatenate 'string (file-io:file-basename pfits) "." pfits-name))
	 (shifted-pfits
	   (shift-fits-relative-to-fits
	    fits-file pfits shifted-pfits-base saaplan
	    :sign +1
	    :header-suffix "presubhead"
	    :delete-headerfiles (static-sky-subtract-preproc-cleanup statsky-preproc)
	    :delete-weight+dir (static-sky-subtract-preproc-cleanup statsky-preproc)
	    :where "build-one-sspp-presub-fits")))
    
    (cf:with-open-fits-file (fits-file ff :mode :input)
      (let* ((wcs (cf:read-wcs fits-file))
	     (hlist (cf:read-fits-header-list ff))
	     (imsec-fits (cf:read-image-section ff))
	     (data-fits  (cf:image-section-data imsec-fits))
	     (imsec-pre  (cf:read-image-section shifted-pfits))
	     (data-pre   (cf:image-section-data imsec-pre)))
	(declare (type imutils:image data-fits data-pre))
	
	(multiple-value-bind (xpixd ypixd)
	    (get-object-location-in-fits-file
	     (saaplan-locator saaplan) saaplan fits-file :coords :pix)


	  (let* ((xpix (float xpixd 1.0))
		 (ypix (float ypixd 1.0))
		 (rp (float (static-sky-subtract-preproc-pre-subtract-radius statsky-preproc)
			    1.0))
		 (ix0 (round (- xpix rp)))
		 (ix1 (round (+ xpix rp)))
		 (iy0 (round (- ypix rp)))
		 (iy1 (round (+ ypix rp))))
		 
	    
	    ;; subtract local image median, and tophat-filter image to nuke stuff
	    ;; outside rp region
	    (loop with backd = (imutils:image-fraction-in-annulus 
				data-pre 0.5 xpix ypix 
				rp (* 1.5 rp))
		  for iy below (array-dimension data-pre 0)
		  do (loop
		       with rc = (* rp 2.0) ;; rclamp - radius at which backd is clamped
		       for ix below (array-dimension data-pre 1)
		       for r = (sqrt (the (single-float 0.0)
					   (+ (expt (- xpix ix) 2)
					      (expt (- ypix iy) 2))))
		       for val = (aref data-pre iy ix)
		       ;; x is parameter for clamping function - so that claimping
		       ;; function goes from 1 to 0 between rp and rclamp 
		       for x = (+ (/ r (- rp rc)) (/ rc (- rc rp)))
		       for weight = (cond ((> x 1) 1.0)
					  ((< x 0) 0.0)
					  (t ;; see wikipedia 'smoothstep'
					   (- (* 3 x x) (* 2 x x x)))) 
		       do  (setf (aref data-pre iy ix)
				 (max 0.0 ;; unphysical
				      (* (- val backd) weight)))))
		  

	    ;; now filter the image to avoid building up coherent noise in the iteration
	    (let ((nfilt (round
			  (static-sky-subtract-preproc-pre-subtract-filter-scale statsky-preproc))))
	      (setf data-pre
		    ;; FIXME - change imutils:median-filter-image to take :ix0..
		    (imutils:median-filter-image
		     data-pre
		     :image-out data-pre
		     :nx nfilt
		     :ny nfilt)))
	    
	    #+nil ;; this code blindly subtracts
	    (loop for ix from ix0 to ix1
		  do (loop for iy from iy0 to iy1
			   do (decf (aref data-fits iy ix)
				    (aref data-pre  iy ix))))

	    ;; FIXME - what if we were to pre-subtract the backd from data-pre
	    ;; and then gaussian-squish it beyond the radius?
	    
	    (imutils:subtract-static-sky
	     data-fits data-pre
	     :auto-backd t
	     ;; we want background left in these images to get photometric
	     ;; errors if we want to use them for photometry
	     :subtract-backd-from-final-result nil 
	     :imout data-fits ;; overwrite
	     :ix0 ix0
	     :ix1 ix1
	     :iy0 iy0
	     :iy1 iy1)
	    )
	  
	  ;; a two-extension file
	  (cf:with-new-fits-file (outfits ffout :overwrite t :make-primary-headers nil)
	    (loop for head in hlist
		for key = (first head) and val = (second head) and comment = (third head)
		  ;; modify some headers
		  for realvalue =  (cond ((equalp key "BITPIX") -32) ;; output is float
					 (t val))
		  do
		     (cf:write-fits-header ffout key realvalue :comment comment))
	    (cf:write-image-section  ffout imsec-fits :reckless t)
	    (cf:write-wcs wcs ffout)))))
    
    (when (static-sky-subtract-preproc-cleanup statsky-preproc)
      (delete-file shifted-pfits))
    
    outfits))
