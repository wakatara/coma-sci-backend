
;; combine bias images

(in-package imred)


;; return those image sections whose median pixel value is
;; within max-fractional-deviation of the sample as a whole, to discard outliers
;;
;; imsec-generator-list is a function of I=0,1... that returns imsec[i] or nil.
;; we do this so we don't read too many imsecs at once, if many were given
;;
(defun %zerocombine-imsec-filter-function 
    (imsec-generator reduction-plan median-nsample)

  (let* ((max-fractional-deviation
	   (reduction-plan-max-zero-fractional-deviation reduction-plan))
	 (nzero-max
	   (reduction-plan-max-zero-frames reduction-plan))
	 (logger (reduction-plan-logger reduction-plan))
	 (med-info-list
	   (loop for i from 0
		 for imsec = (funcall imsec-generator i)
		 until (not imsec)
		 for ext = (cf:fits-file-current-hdu-num
			    (cf:image-section-orig-fits-file imsec))
		 for fits = (file-io:file-minus-dir
			     (cf:fits-file-filename
			      (cf:image-section-fits-file imsec)))
		 for median
		   = (multiple-value-bind (med err)
			 (ignore-errors
			  (randomly-sampled-image-median
			   (cf:image-section-data imsec)
			   median-nsample))
		       (cond
			 ((not med)
			  (logger:writelog
			   logger
			   (format
			    nil
			    "Error computing median of input bias ~A:~A - ~A"
			    fits ext err))
			  nil) ;; return median=nil so it isn't collected
			 (t
			  med)))
		 when median
		 collect (list median fits ext)))
	 (med-med (stats:median-of-elements (mapcar 'first med-info-list))))
  
    (loop
      with ngood = 0
      for i from 0
      for med-info in med-info-list
      for med = (first med-info) and fits = (second med-info) and ext = (third med-info)
      for is-good = (<= (abs (- med med-med))
			;; 1.0 is to address possibility that med-med is zero,
			;; as in an overscan subtracted image
			(+ 1.0 (* max-fractional-deviation med-med)))
      when (not is-good)
	do
	   (logger:writelog
	    logger
	    (format nil "ERROR: Input bias ~A:~A rejected as outlier: counts = ~A"
		    fits ext  med))
	   
      when (and (< ngood nzero-max) is-good)
	do
	   (incf ngood)
	   (logger:writelog
	    logger
	    (format nil "Accepted input bias ~A:~A"  fits ext))
	and
	  collect (funcall imsec-generator i))))


 
(defun zerocombine (fits-list fits-out &key
		    (reduction-plan *default-reduction-plan*)
		    (stack-type :median)
		    (template-fits nil)
		    (if-exists :error)
		    (median-nsample 30000))
  "Does a median combine of bias frames, removing those with a median
that is more than MAX-FRACTIONAL-DEVIATION fractional distance from the
median-median of the sample."

  (declare (type list fits-list)
	   (type (or string pathname) fits-out)
	   (type (unsigned-byte 28) median-nsample))
	   
  (stack-images fits-list fits-out   
		:reduction-plan reduction-plan
		:template-fits template-fits
		:stack-type stack-type
		:if-exists if-exists
		:final-array-function nil
		:count-header "IMRED.NBIAS"
		:imsec-filter-function
		;; an imsec-generator is a function of one function I=0,1....
		;; that returns IMSEC[i]
		(lambda (imsec-generator extdesc)
		  (declare (ignore extdesc))
		  (%zerocombine-imsec-filter-function 
		   imsec-generator 
		   reduction-plan
		   median-nsample)))
  ;;
  (cf:write-fits-header fits-out "IMRED.CAL" T :comment "This is an IMRED calib file"))
