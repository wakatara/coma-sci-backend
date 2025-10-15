#|

Fix the aperture magnitude using MAG_AUTO

|#

(in-package phot-calib)

;; return FWHM or NIL if not enough info
(defun %estimate-fwhm-pix-from-a-img (a-image magerr-auto)
  (loop for a across a-image
	for magerr across magerr-auto
	when (< 0.0001 magerr 0.02)
	  collect a into a-image-list
	finally
	   (let ((fwhm
		   (cond
		     ;; if lots of points, try the mode, and fall back on median
		     ((> (length a-image-list) 30) ;; enough points
		      (let* ((a-best (or (ignore-errors (stats:mode-of-elements a-image-list))
					 (stats:median-of-elements a-image-list)))
			     (fwhm (imutils:gaussian-fwhm a-best)))
			fwhm))
		     ;; if few points, try the median
		     ((> (length a-image-list) 6)
		      (stats:median-of-elements a-image-list))
		     ;; else give up - can't compute fwhm
		     (t
		      nil))))
	     ;;(format t "FWHM of ~A  points is ~A~%" (length a-image-list) fwhm)
	     (return fwhm))))
	     

;; compute the incompleteness of each aperture (ie, the mag to add
;; to each aperture to make it match MAG_APER)
;;
;; returns
;;  (VALUES FWHM/PIX
;;          N-MAGS-USED
;;          #(DMAG-AP-1 DMAG-AP-2 ...))  ;; this vector may have NIL elements
(defun compute-aperture-fix-vector (catalog &key (good-mag-err 0.01))
  (let* ((mags-aper (gethash "MAG_APER" catalog))	
	 ;;(magerrs-aper (gethash "MAGERR_APER" catalog))
	 (apertures (coerce (gethash "%APERTURES" catalog) 'vector)) ;; pix
	 (mag-auto  (gethash "MAG_AUTO" catalog))
	 (magerr-auto (gethash "MAGERR_AUTO" catalog))
	 (a-img (gethash "A_IMAGE" catalog)) ;; semi-major axis
	 ;; gaussian fwhm
	 (fwhm/pix (%estimate-fwhm-pix-from-a-img 
		    a-img magerr-auto))
	 (n-mags-used 0)) ;; number of good mags we used - same for every i-ap below

    
    (flet ((find-dmag-for-aperture (i-ap)
	     (loop for i from 0
		   for m-auto across mag-auto
		   for merr-auto across magerr-auto
		   for m-ap = (aref mags-aper i i-ap)
		   when (< merr-auto good-mag-err) ;; use high S/N dets only
		     collect (- m-auto m-ap) into dm-list
		   finally
		      (return
			(if (> (length dm-list) 3)
			    (progn
			      (setf n-mags-used (length dm-list))
			      (return 
				;; mode is much more stable to changes in good-mag-err
				(or (ignore-errors (stats:mode-of-elements dm-list))
				    (stats:median-of-elements dm-list)) )))))))
      (let* ((mag-cor-vec
	       (coerce
		(loop for i-ap from 0
		      for ap across apertures
		      collect (find-dmag-for-aperture i-ap))
		'vector))
	     ;; is the result usable?
	     (is-ok (and fwhm/pix (every 'realp mag-cor-vec))))
	(values
	 is-ok
	 fwhm/pix
	 n-mags-used
	 mag-cor-vec)))))

#|
Return a vector of corrected magnitudes and errors:
 (VALUES MAG-FIXED MAGERR-FIXED)
where the fixed mags are based on correcting MAG_APER
using MAG_AUTO, demanding that at least npts-min-for-correction
stars were available to compute the offset.

The smallest aperture with at least 90% of the light is used for the
correction.

APP-DMAG is the maximum correction we allow

Only mags with magnitude errors smaller than GOOD-MAG-ERR are used
to compute the offset.

|#
(defun compute-corrected-mag-auto-vector (catalog &key (good-mag-err 0.01)
						    (npts-min-for-correction 10)
						    (app-dmag 0.10)) ;; seems to be a good value
  (multiple-value-bind (is-ok fwhm/pix n-mags-used-for-correction mag-cor-vec)
      (compute-aperture-fix-vector catalog :good-mag-err good-mag-err)

    #+nil
    (format t "n-mags-used-for-correction: ~A   npts-min-for-correction: ~a  is-ok: ~A~%"
	    n-mags-used-for-correction npts-min-for-correction is-ok)

    (let* ((mag-auto  (gethash "MAG_AUTO" catalog))
	   (magerr-auto  (gethash "MAGERR_AUTO" catalog))
	   (a-img (gethash "A_IMAGE" catalog)) ;; semi-major axis
	   (mags-aper (gethash "MAG_APER" catalog))
	   (magerrs-aper (gethash "MAGERR_APER" catalog))
	   ;;(apertures (coerce (gethash "%APERTURES" catalog) 'vector)) ;; pix
	   (flags (gethash "FLAGS" catalog))
	   ;; (npts (length mag-auto))
	   (mag-out (copy-seq mag-auto))
	   (magerr-out (copy-seq magerr-auto))
	   (kap nil) ;; the number of the aperture to choose
	   (acor nil)
	   (noriginal 0)
	   (ncorrected 0))

      (when is-ok ;; was the result of compute-aperture-fix-vector OK?
	;; first find KAP, the aperture used for the fixed magnitude
	(loop for mc across mag-cor-vec
	      for iap from 0
	      ;; pick the first aperture index where the correction is less than 0.08 (arbitrary)
	      when (< (abs mc) app-dmag) ;; better than 90% of flux
		do
		   (setf kap iap)
		   (setf acor mc)
		   (return))
	
	
	;; go through mags and for those that have suitable FWHM, substitute
	;; the ACOR-corrected aperture mag for MAG-AUTO
	(when (and kap ;; perhaps we found no good aperture
		   (> n-mags-used-for-correction npts-min-for-correction))
	  (loop for i from 0
		for a across a-img
		for flag across flags
		for magerr across magerr-auto
		for fwhm = (imutils:gaussian-fwhm a)
		if (and
		    ;; brights objects must be close to FWHM of image,
		    ;; and faint ones must be kinda close
		    (if (< magerr 0.05)
			(<= (* 0.7 fwhm/pix) fwhm  (* 1.5 fwhm/pix))
			(<= (* 0.5 fwhm/pix) fwhm  (* 2.0 fwhm/pix)))
		    ;; don't allow some flags
		    (not (terapix:sextractor-flag-aperture-data-bad flag))
		    (not (terapix:sextractor-flag-blended flag))
		    (not (terapix:sextractor-flag-truncated flag)))
		  do (incf ncorrected 1)
		     (setf (aref mag-out i)
			   (+ acor (aref mags-aper i kap)))
		     (setf (aref magerr-out i)
			   (aref magerrs-aper i kap))
		else
		  do (incf noriginal 1)))) ;; end 'when is-ok'
	
      (values mag-out magerr-out noriginal ncorrected))))
      
	     
	   
		   
		   
		   
		   
		   

    
