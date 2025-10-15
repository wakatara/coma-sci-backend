

(in-package  gmos-proc)

;; for an extension in FF, get a list of the data section
;; and bias section.  It not PROCESSED, use DATASEC and TRIMSEC,
;; otherwise if PROCESSED don't get a trimsec, but get the whole image
(defun get-datasec-and-biassec (ff ext &key (processed nil))
  (cf:move-to-extension ff ext)
  (let* ((datasec (when (not processed)
		    (cf:parse-image-section-string
		     (cf:read-fits-header ff "DATASEC"))))
	 (biassec (when (not processed)
		    (cf:parse-image-section-string
		     (cf:read-fits-header ff "BIASSEC"))))
	 (imsec-data
	   (cf:read-image-section ff :type :single-float
				     :fp (when datasec
					   (vector (aref datasec 0)
						   (aref datasec 2)))
				     :lp (when datasec
					   (vector (aref datasec 1)
						   (aref datasec 3)))))
	 (imsec-bias
	  (when (not processed)
	    (cf:read-image-section ff :type :single-float
				      :fp (vector (aref biassec 0)
						  (aref biassec 2))
				       :lp (vector (aref biassec 1)
						   (aref biassec 3)))))
	 (gain (or (ignore-errors
		    (float (cf:read-fits-header ff "GAIN") 1.0))
		   (error "GAIN keyword not found"))))
	 ;;
    (list imsec-data imsec-bias gain)))
	  

(defun debias-with-biassec (imsec-data imsec-bias)
  (let* ((adata (cf:image-section-data imsec-data))
	 (abias (cf:image-section-data imsec-bias))
	 (nxd (array-dimension adata 1))
	 (nyd (array-dimension adata 0))
	 (nxb (array-dimension abias 1))
	 (nyb (array-dimension abias 0))
	 (vb  (make-array nxb :element-type 'single-float)))
    (declare (type imutils:image adata abias)
	     (type jtypes:fast-unsigned-int nxd nyd nxb nyb)
	     (optimize speed))
    (when (not (= nyd nyb))
      (error "DATASEC and BIASSEC don't have the same Y dimension."))
    (loop
      for iy of-type jtypes:fast-unsigned-int below nyd
      do (loop for ibx of-type jtypes:fast-unsigned-int below nxb
	       do (setf (aref vb ibx) (aref abias iy ibx)))
	 (let ((med (fastmedian:fast-single-float-1d-array-median vb)))
	   (declare (type single-float med))
	   (loop for idx of-type jtypes:fast-unsigned-int below nxd
		 do (decf (aref adata iy idx) med))))))
	  


;; compute the median of the ratio of the boundary between left and
;; right amp regions, using a band of nwidth.  We expect roughly
;; 0.1%/sqrt(NWIDTH) accuracy, where NWIDTH is the width of the strip
;; in each image used to compute the ratio of flux.  However, it is
;; best to use NWIDTH=1 because even tiny gradients can introduce a
;; systematic effect.
;;
;; IMPROVEMENT: compute a median slope on each side of the boundary
;; using median(x_i - x_(i-1)), use slope to flatten the vector of
;; points on each side of boundary, take median of flattened vector,
;; and compute the flux ratio from that.  With a 100 pix strip, we
;; could get 0.01% precision rather than 0.1% (but systematics might
;; become an issue with a wide band)
;;
(defun fix-relative-gain-between-amps-using-boundary
    (arl arr &key (nwidth 1) (rescale-side :right)
	 (frac-vignetted 0.12))
  "Use NWIDTH pixels in left array ARL and right array ARL to compute
median offset between the two sides.  Then rescale the RESCALE-SIDE
array to match the other side.

FRAC-VIGNETTED is the fraction of the image (from bottom and again
from top) that is vignetted, and cannot be used for the statistics."
  (declare (type imutils:image arl arr)
	   (type (member :right :left) rescale-side)
	   (type (single-float 0.0 1.0) frac-vignetted)
	   (type (unsigned-byte 16) nwidth))
  (let* ((ny (array-dimension arl 0))
	 (ymin (round (* frac-vignetted ny))) ;; unvignetted region in edge chps
	 (ymax (round (* (- 1.0 frac-vignetted) ny)))
	 (jx (1- (array-dimension arl 1)))
	 (rlist
	   (loop with outlist = nil
		 for iw below nwidth
		 do
		    (loop for iy from ymin to ymax
			  ;; note that pixels are paired as Al Bl Cl | Ar Br Cr
			  ;; so the ratios are Al/Ar, Bl/Br, Cl/Cr
			  ;; this accounts at least a little
			  ;;  for a gradient across the boundary
			  for xl = (aref arl iy (+ jx (- nwidth) iw 1))
			  for xr = (aref arr iy (+ 0 iw))
			  when (and
				(not (float-utils:single-float-nan-or-infinity-p xl))
				(not (float-utils:single-float-nan-or-infinity-p xr))
				(plusp xl)
				(plusp xr))
			    do (push (/ xl xr) outlist))
		 finally (return outlist)))
	 (rvec (coerce rlist '(simple-array single-float (*))))
	 (ratio (fastmedian:fast-single-float-1d-array-median rvec))
	 (scaling (if (eq rescale-side :right) ratio (/ 1.0 ratio))))
    
    (declare (type single-float ratio scaling)
	     (type (unsigned-byte 16) ymin ymax ny)
	     (type list rlist)
	     (optimize speed))

    ;; normalize the correct one of ARR, ARL by SCALING
    (loop with a of-type imutils:image = (if (eq rescale-side :right) arr arl)
	  for i of-type jtypes:fast-unsigned-int below (array-total-size arr)
	  do (setf (row-major-aref a i)
		   (* scaling (row-major-aref a i))))
    scaling))
    
	 
(defparameter *exclude-headers*
  '("BZERO" "BSCALE" "BITPIX" "SIMPLE" "EXTEND"
    ;;"PCOUNT" "GCOUNT" ;; keep these - these are still valid for each img
    "NAXIS" "NAXIS1" "NAXIS2" "CCDSEC" "BIASSEC" "DETSEC" "DATASEC"))
  

;; Copy headers from primary plus EXT into FF-OUT.  This ends up
;; transferring WCS correctly
(defun %copy-headers (ff-in ff-out ext
		      &key
			(normalize-gain t)
			(exclude-headers *exclude-headers*))
  (flet ((do-one-ext (k)
	   (cf:move-to-extension ff-in k)
	   (loop
	     with hlist =  (cf:read-fits-header-list ff-in)
	     for header in hlist
	     for key = (first header) and val = (second header)
	     and comment = (third header)
	     do (cond
		  ;; we changed gain to FINAL-GAIN
		  ((equalp key "GAIN")
		   (cf:write-fits-header ff-out "GAIN"
					 (if normalize-gain 1.0 val)
					 :comment "Gain")
		   (when normalize-gain
		     (cf:write-fits-comment
		      ff-out
		      (format nil "Original left amp gain was ~,4F" val))))
		  ;; ignore these headers because the image will put them in
		  ((member key exclude-headers :test 'equalp)
		   nil)
		  ;;
		  (t
		   (cf:write-fits-header ff-out key val :comment comment))))))
    (do-one-ext 1) ;; copy primary into each image
    (when ext
      (do-one-ext ext)))) ;; copy image extension
	 


    

;; combine extensions ext-left and ext-right from ff-in to ff-out
;; producing float output image - this is for RAW images that haven't been processed
;; and trimmed
(defun combine-chip-amps (ff-in ff-out ext-list
			  &key (processed t)
			    (debias t)
			    (fix-relative-gain nil)
			    (normalize-gain t))

  (when (and processed
	     (or normalize-gain debias))
    (error "COMBINE-CHIP-AMPS should not be called with NORMALIZE-GAIN or DEBIAS when PROCESSED=T. These are options for raw images only."))
  
  (let* (;; pairs of (imsec-image imsec-bias)
	 (imsec-list (mapcar (lambda (ext) (get-datasec-and-biassec
					    ff-in ext
					    :processed processed))
			     ext-list))
	 ;; total width of output image
	 (nxtot  (loop for imsec-pair in imsec-list
		       for imsec = (first imsec-pair) ;; image section
		       sum (array-dimension (cf:image-section-data imsec) 1)))
	 (imsec0 (caar imsec-list)) ;; first imsec
	 ;; total height of output image
	 (ny (array-dimension (cf:image-section-data imsec0) 0))
	 ;; output image
	 (arrout (imutils:make-image ny nxtot))
	 ;; adjustment ratios used to correct across boundaries
	 (amp-ratios (list 1.0)))

    (declare (type imutils:image arrout))
	 

    ;; if requested, bias correct
    (when debias
      (loop for imsec-pair in imsec-list
	    for imsec = (first imsec-pair)
	    for bsec = (second imsec-pair)
	    do (debias-with-biassec imsec bsec)))

    ;; if requested, normalize gain to 1:  (e/adu * adu = e)
    (when normalize-gain
      (loop for imsec-pair in imsec-list
	    for imsec = (first imsec-pair)
	    for arr = (cf:image-section-data imsec)
	    for gain = (third imsec-pair)
	    do (imutils:im-scale arr gain :image-out arr)))

    ;; if requested, fix the relative gains, each time using the image on the
    ;; left as the normalizer
    (when fix-relative-gain
       (loop for imsec-pair-l in imsec-list ;; left
	     for imsec-l = (first imsec-pair-l)
	     for arl = (cf:image-section-data imsec-l)
	     for imsec-pair-r in (cdr imsec-list) ;; right
	     for imsec-r = (first imsec-pair-r)
	     for arr = (cf:image-section-data imsec-r)
	     do
		(push  (fix-relative-gain-between-amps-using-boundary arl arr)
		       amp-ratios)))

    ;; now put the the scaled sections into the output array
    (locally (declare (optimize (speed 3)))
      (loop with nx0 of-type (unsigned-byte 28) = 0 ;; x offset for this imsec
	    for imsec-pair in imsec-list
	    for imsec = (first imsec-pair)
	    for arr of-type imutils:image = (cf:image-section-data imsec)
	    for nx of-type (unsigned-byte 28) = (array-dimension arr 1)
	    do
	       (loop
		 for iy of-type (unsigned-byte 28) below ny
		 do  (loop for ix of-type (unsigned-byte 28) below nx
			   for jx of-type (unsigned-byte 28) from nx0
			   do (setf (aref arrout iy jx)
				    (aref arr    iy ix))))
	       (incf nx0 nx)))

	 ;;
    (cf:add-image-to-fits-file ff-out :float (vector nxtot ny)
				      :create-data arrout)
    (cf:write-fits-header
     ff-out
     "DATASEC"
     (format nil "[~A:~A,~A:~A]" 1 nxtot 1 ny))
			  

    (%copy-headers ff-in ff-out (first ext-list))


    (loop with nx0 of-type (unsigned-byte 28) = 0 ;; x offset for this imsec
	  for iamp from 1
	  for imsec-pair in imsec-list
	  for imsec = (first imsec-pair)
	  for arr = (cf:image-section-data imsec)
	  for nx of-type (unsigned-byte 28) = (array-dimension arr 1)
	  do (cf:write-fits-comment
	      ff-out
	      (format nil "Amp[~D]  spans x=[~A,~A]"
		      iamp nx0 (+ nx0 nx -1)))
	     (incf nx0 nx))
    
    (cf:write-fits-header ff-out "IMRED.GMOSCOMBINED" t :comment "combined amps")

     (when amp-ratios
      (cf:write-fits-header
       ff-out "IMRED.FIXAMPGAIN"
       (format nil "~{~,3F~^, ~}" amp-ratios) ;; print as comma-delim list
       :comment "rescalings of amp regions"))))
    



    

;; combine extensions ext-left and ext-right from ff-in to ff-out
;; producing float output image - this is for images that HAVE been
;; processed and trimmed
#+nil 
(defun combine-chip-amps-processed (ff-in ff-out ext-left ext-right
				    &key (fix-relative-gain nil))

  ;; FIXME - we're now here
  (let* ((imsecl (progn (cf:move-to-extension ff-in ext-left)
			(cf:read-image-section ff-in))) ;; whole thing
	 (imsecr (progn (cf:move-to-extension ff-in ext-right)
			(cf:read-image-section ff-in))) ;; whole thing
	 (arl (cf:image-section-data imsecl))
	 (arr (cf:image-section-data imsecr))
	 (nx (array-dimension arl 1))
	 (ny (array-dimension arl 0))
	 (nx2 (* 2 nx))
	 (arrout (imutils:make-image ny nx2))
	 (amp-ratio nil))
    (declare (type imutils:image arl arr arrout)
	     (type (unsigned-byte 28) nx ny nx2)
	     (optimize speed))
    (when (not (equalp (array-dimensions arl)
		       (array-dimensions arr)))
      (error "Chips ~A,~A not of same dimensions." ext-left ext-right))


    (when fix-relative-gain
       (setf amp-ratio (fix-relative-gain-between-amps-using-boundary arl arr)))
    
    (loop for ixl of-type jtypes:fast-unsigned-int below nx
	  for ixr of-type jtypes:fast-unsigned-int from nx
	  do (loop for iy of-type jtypes:fast-unsigned-int below ny
		   do (setf (aref arrout iy ixl) (aref arl iy ixl))
		      (setf (aref arrout iy ixr) (aref arr iy ixl))))

      ;;
    (cf:add-image-to-fits-file ff-out :float (vector nx2 ny)
				      :create-data arrout)
    (cf:write-fits-header
     ff-out
     "DATASEC"
     (format nil "[~A:~A,~A:~A]" 1 nx2 1 ny))

    (%copy-headers ff-in ff-out ext-left)
    
    (cf:write-fits-comment
     ff-out  
     (format nil "amp1 spans x=[~A:~A]; amp2 spans x=[~A,~A]"
	     1 nx (+ 1 nx) (* 2 nx)))
    (cf:write-fits-header ff-out "IMRED.GMOSCOMBINED" t :comment "combined amps")
    (when amp-ratios
      (cf:write-fits-header
       ff-out "IMRED.FIXAMPGAIN"
       (format nil "~{~,3F~^, ~}" amp-ratios) ;; print as comma-delim list
       :comment "rescalings of amp regions"))))

    
 



;; create a name for the output file
(defun %make-gmos-combined-name (gmos-fits)
  (let* ((str (namestring gmos-fits))
	 (ndot (position #\. str  :from-end t))
	 (base (if ndot
		   (subseq str 0 ndot)
		   str)))
    (concatenate 'string base "c.fits")))






(defun combine-gmos-chips (gmos-fits &key (fits-out nil)
				       (overwrite nil)
				       (debias t)
				       (fix-relative-gain nil)
				       (normalize-gain t)
				       (processed nil)
				       (only-central-chip nil))
  "Combine the half-chip amplifier images of a GMOS image, creating an output image
 (OR FITS-OUT NYYYYMMSNNNc.fits) with 3 image extensions.

Keywords: 

 DEBIAS  means to use the BIASSEC to remove the zero point.
 FIX-RELATIVE-GAIN means to use the step across the boundary between
    amplifiers to rescale the right amplifier (expect about 0.1% error).
 ONLY-CENTRAL-CHIP means just do the full central chip, not the 
    two vignetted side chips.

PROCESSED means that this is for chips that have been processed already,
   meaning that they have been trimmed, bias-subtracted, etc.

This should be run with FIX-RELATIVE-GAIN when doing a quick
reduction, but NORMALIZE-GAIN and (in theory) FIX-RELATIVE-GAIN could
be turned off before a full reduction, because proper flatfielding
will take care of gain issues.

In practice, it seems that FIX-RELATIVE-GAIN might still be needed when
flats are dubious."

  (when processed
    (when normalize-gain
      (error "combine-chip-amps: if PROCESSED then NORMALIZE-GAIN not allowed"))
    (when debias
      (error "combine-chip-amps-processed: if PROCESSED then DEBIAS not allowed")))

  
  (let* ((id (instrument-id:identify-instrument gmos-fits))
	 (fits-out (or fits-out (%make-gmos-combined-name gmos-fits))))

    (when (not (typep id 'instrument-id::%gmos-frac-chip-array))
      (error "Can't assemble chips in ~A if image is not of type instrument-id::%gmos-frac-chip-array" gmos-fits))

    ;; figure out the list of extensions for each chip, based on whether this is
    ;; e2v or hamamatsu
    ;;
    ;; chip layouts here: http://www.gemini.edu/sciops/instruments/gmos/data-format-and-reduction
    ;;
    (let* ((chip-type
	     (cond ((typep id 'instrument-id:%gmos-e2v) 'e2v)
		   ((typep id 'instrument-id:%gmos-hamamatsu) 'hamamatsu)
		   (t (error "Fits ~A is neither e2v nor Hamamatsu" gmos-fits))))
	   (chip-1-exts (cond ((eq chip-type 'e2v)
			       '(3 2))
			      ((eq chip-type 'hamamatsu)
			       '(2 3 4 5))))
	   (chip-2-exts (cond ((eq chip-type 'e2v)
			       '(5 4))
			      ((eq chip-type 'hamamatsu)
			       '(6 7 8 9))))
	   (chip-3-exts (cond ((eq chip-type 'e2v)
			       '(6 7))
			      ((eq chip-type 'hamamatsu)
			       '(10 11 12 13)))))
			
    
      (cf:with-open-fits-file (gmos-fits ff-in)
	(cf:with-new-fits-file (fits-out ff-out :overwrite overwrite)
	  (when (not only-central-chip)
	    (cf:add-image-to-fits-file ff-out :float #()) ;; primary header
	    (%copy-headers ff-in ff-out  nil)
	    (cf:write-fits-header ff-out "IMRED.GMOSCOMBINED" T
				  :comment "GMOS image w/chips combined"))
      
	  (when (not only-central-chip)
	    (combine-chip-amps ff-in ff-out chip-1-exts
			       :debias debias
			       :fix-relative-gain fix-relative-gain
			       :processed processed
			       :normalize-gain normalize-gain)
	    (cf:write-fits-header ff-out "EXTNAME" "CHIP1"))
	  ;;
	  (combine-chip-amps ff-in ff-out chip-2-exts
			     :debias debias
			     :fix-relative-gain fix-relative-gain
			     :processed processed
			     :normalize-gain normalize-gain)
	  (cf:write-fits-header ff-out "EXTNAME" "CHIP2")
	  ;;
	  (when (not only-central-chip)
	    (combine-chip-amps ff-in ff-out chip-3-exts
			       :debias debias
			       :fix-relative-gain fix-relative-gain
			       :processed processed
			       :normalize-gain normalize-gain)
	    (cf:write-fits-header ff-out "EXTNAME" "CHIP3"))))
    
      fits-out)))

  
  
