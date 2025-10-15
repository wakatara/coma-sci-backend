#|
   
    Make a weight image for sextractor

|#

(in-package terapix)

(defun make-sextractor-badpix-weight-image (fits-file weight-fits-file
					    &key
					      extension
					      input-bitflag-image
					      (min-allowed-value nil)
					      (max-allowed-value nil)
					      filter-bad-rowcol
					      ;; mark-cosmics ;; FIXME - not implemented
					      overwrite)
  "Make a bad pixel map fits file for an input fits file, with 0 at
the bad pixels and 1 at the good pixels.

INPUT-BITFLAG-IMAGE is an optional bitmap with 1 the BAD pixels and 0
at the good ones (the inverse of the final bitmap).

MIN-ALLOWED-VALUE and MAX-ALLOWED-VALUE are the cutoff values flagged
as bad.  If they are NIL, no cutoff is used. If they are the keyword
:AUTO then they are set to cut off a low value of 10 sigma below 0.9x
the image median for filtering of severe drops-outs, and a high value
of 0.97 times the image max, to get saturated pixels.

FILTER-BAD-ROWCOL uses IMUTILS:BAD-ROWCOL-FILTER-IMAGE to find anomalous columns.

OVERWRITE enables overwriting of existing WEIGHT-FITS-FILE."
  ;;
  (let* ((imsec (cf:read-image-section fits-file :extension extension :type :single-float))
	 (im (cf:image-section-data imsec))
	 (npix (array-total-size im))
	 (minval 0.0)
	 (maxval 0.0)
	 (flag-image
	   (progn
	     (when (and input-bitflag-image
			(not (and (eq (array-element-type input-bitflag-image) 'bit)
				  (equalp (array-dimensions input-bitflag-image)
					  (array-dimensions im))))
			(error "An invalid INPUT-BITFLAG-IMAGE was given")))
	     (or input-bitflag-image
		 (imutils:make-same-size-bit-image im :initial-value 0)))))
    ;;
    (declare (type imutils:image im)
	     (type fixnum npix)
	     (type imutils:bit-image flag-image)
	     (type single-float minval maxval))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; section to set cutoff values if not specified, using :AUTO
    ;; keyword
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (flet ((%get-auto-min-allowed-value ()
	     (multiple-value-bind (median sigma)
		 (imutils:compute-sampled-image-median-and-sigma im 10000 :allow-nan nil)
	       ;; cut off at 90% of median minus 10 sigma
	       (- (* 0.9 median) (* 10 sigma))))
	   (%get-auto-max-allowed-value ()
	     (loop with xmax of-type single-float = most-negative-single-float
		   for i of-type fixnum below npix
		   for x of-type single-float = (row-major-aref im i)
		   when (and (not (float-utils:single-float-nan-or-infinity-p x))
			     (> x xmax))
		     do (setf xmax x)
		   finally
		      ;; now check that we won't delete too many pixels
		      (return
			(loop
			  with cutoff of-type single-float = (* 0.95 xmax)
			  with ncutoff of-type fixnum = 0
			  for i of-type fixnum below npix
			  for x of-type single-float = (row-major-aref im i)
			  when (and (not (float-utils:single-float-nan-or-infinity-p x))
				    (>= x cutoff))
			    do (incf ncutoff)
			  finally
			     ;; if we're cutting off more than 10% of pixels, make the cutoff infinite
			     (when (> ncutoff (* 0.1 npix))
			       (setf cutoff most-positive-single-float))
			     (return cutoff))))))
      (setf minval
	    (cond ((eq min-allowed-value :auto)
		   (%get-auto-min-allowed-value))
		  ((not min-allowed-value)
		   most-negative-single-float)
		  ((realp min-allowed-value)
		   (float min-allowed-value 1.0))
		  (t
		   (error "Invalid MIN-ALLOWED-VALUE ~A" min-allowed-value))))
      (setf maxval
	    (cond ((eq max-allowed-value :auto)
		   (%get-auto-max-allowed-value))
		  ((not max-allowed-value)
		   most-negative-single-float)
		  ((realp max-allowed-value)
		   (float max-allowed-value 1.0))
		  (t
		   (error "Invalid MAX-ALLOWED-VALUE ~A" max-allowed-value)))))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    (when filter-bad-rowcol
      (imutils:bad-rowcol-filter-image
       im :modify-input-image nil :input-bitflag-image flag-image))
    ;;
    (loop for i of-type fixnum below npix
	  for x of-type single-float = (row-major-aref im i)
	  when (or (float-utils:single-float-nan-or-infinity-p x)
		   (< x minval)
		   (> x maxval))
	    do (setf (row-major-aref flag-image i) 1))
    ;; now invert the 1 to 0 for the weight map
    (loop for i of-type fixnum below (array-total-size im)
	  for f = (row-major-aref flag-image i)
	  for finv = (if (zerop f) 1 0)
	  do (setf (row-major-aref flag-image i) finv))
    ;; fits does not support bit-images so we use :UINT8
    (cf:write-2d-image-to-new-fits-file flag-image weight-fits-file :overwrite overwrite
					:type :byte)))
    
						      
						      
