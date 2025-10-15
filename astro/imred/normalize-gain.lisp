

(in-package imred)



;; this is run on a completed image, so that pixels with
;; (reduction-plan-output-null-pixel-value reduction-plan) are assumed to be 
;; NULL and are not rescaled
(defun normalize-gain-of-reduced-image
    (fits &key (reduction-plan *default-reduction-plan*))
  
  "DESTRUCTIVELY normalized the gain of an image to 1.0, which it
 resets to 1.  Output will be in single-float."
  ;;(declare (optimize debug))
  (let ((extdesc-list (build-extdesc-list-for-fits 
		       fits :reduction-plan reduction-plan)))
    
    (loop for extdesc in extdesc-list
	 when (and (extdesc-reduce-p extdesc) 
		   (not (and (extdesc-gain extdesc)
			     (realp (extdesc-gain extdesc)))))
       do (error "GAIN not found by INSTRUMENT-ID in ~A[~A]" 
		 fits (extdesc-n-ext extdesc)))

    (cf:with-open-fits-file (fits ff :mode :io)
    ;;
      (loop 
	 with null-pixel-value 
	   = (reduction-plan-output-null-pixel-value reduction-plan) 
	 for extdesc in extdesc-list
	 for ihdu = (extdesc-n-ext extdesc)
	 for gain = (extdesc-gain extdesc)
	 when (not (cf:read-fits-header ff "IMRED.NORMGAIN"))
	 do 
	   (cf:move-to-extension ff ihdu)
	   (cond
	     ;; ignore any non-image extension
	     ((not (extdesc-reduce-p extdesc))
	      nil)
	     ;; if gain=1, then do nothing
	     ((= gain 1)
	      nil)
	     ;;
	     (t ;; else we know that gain is a valid number
	      (let* ((imsec (cf:read-image-section ff :type :single-float))
		     (im (cf:image-section-data imsec))
		     (fgain (float gain 1.0)))
		
		(locally
		    (declare (type (simple-array single-float (* *)) im)
			     (optimize speed))
		  ;; it's unclear why we need to mask here - some pixels can sometimes be
		  ;; close to infinite?
		  (float-utils:with-float-traps-masked (:inexact t :invalid t)

		    (loop with npix = (array-total-size im)
			  for i of-type fixnum below npix
			  for pixval of-type single-float = (row-major-aref im i)
			  when (float-utils:float-nan-or-infinity-p pixval)
			    ;; note that testing against NaN with ordinary '='
			    ;; can throw a float exception
			    when (not (float-utils:safe-single-float=
				       pixval null-pixel-value))
			    do
			       (setf (row-major-aref im i) (* pixval fgain)))))
		  
		(cf:write-back-image-section  imsec))
	      (cf:write-fits-header ff "IMRED.OLDGAIN" gain :comment "Old gain value")
	      (cf:write-fits-header ff "IMRED.NORMGAIN" 
				    (astro-time:ut-to-date-string (get-universal-time))
				    :comment "Gain normalized by imred")
	      (instrument-id:write-gain-for-fits ff gain :extension nil)))))))




	 
