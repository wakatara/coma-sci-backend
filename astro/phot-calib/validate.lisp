

(in-package phot-calib)

;; download a catalog again, and compute median calibrated mag from actual mag
(defun validate-phot-calib (fits-file catalog-type filter
			    &key (aperture 20) (max-mag 22))
  (terapix:run-sextractor fits-file 
			  :output-catalog "sex_validate_phot_calib.cat"
			  :phot-apertures (list aperture))
  
  (multiple-value-bind (ra0 dec0 radius)
      (%get-fits-bounds fits-file)
    (let* ((catalog (astro-catalog:get-cached-catalog-object 
		     ra0 dec0 radius catalog-type))
	   (mag-translation-function 
	     (or (get-mag-trans-func-for-catalog catalog filter)
		 (error "Cannot translate catalog of type ~A to magnitude ~A" 
			(type-of catalog) filter)))
	   (shash (terapix:read-sextractor-catalog 
		   (concatenate 'string (terapix:get-fits-directory fits-file)
				"/sex_validate_phot_calib.cat")))
	   (ra-vec (gethash "ALPHA_J2000" shash))
	   (dec-vec (gethash "DELTA_J2000" shash))
	   (num-vec (gethash "NUMBER" shash))
	   (flag-vec (gethash "FLAGS" shash))
	   (mag-ap-array (gethash "MAG_APER" shash))
	   (magerr-ap-array (gethash "MAGERR_APER" shash))
	   (zp (cf:read-fits-header fits-file "PHOTCALIB.ZPMAG"))
	   (scog (if (equalp (cf:read-fits-header fits-file "PHOTCALIB.MAGTYPE")
			     "MAG_AP")
		     (terapix:read-curve-of-growth-from-fits-file fits-file)))
	   (pcobj-list
	     (loop for ra across ra-vec and dec across dec-vec
		   for flag across flag-vec
		   for i from 0
		   for mag = (aref mag-ap-array i)
		   for mag-cor = (if (not scog) mag
				     (terapix:correct-aperture-mag-using-curve-of-growth 
				      scog mag aperture))
		   for mag-err = (aref magerr-ap-array i)
		   when (and (zerop flag) (< mag-cor max-mag))
		     collect (make-pcobj :alpha ra :delta dec 
					 :mag (float (+ zp mag-cor) 1d0)
					 :mag-err (float mag-err 1d0)
					 :id i)))
	   (matching-pairs (nth-value 3 (phot-calib-obj-list-using-catalog 
			    pcobj-list catalog
			    mag-translation-function)))
	   (dmag-list (mapcar (lambda (pair) 
				(- (pcobj-mag (first pair)) (pcobj-mag (second pair))))
			      matching-pairs))
	   (median-mag (stats:median-of-elements dmag-list)))
      (if scog (format t "USING COG correction of ~A mag~%" 
		       (terapix:correct-aperture-mag-using-curve-of-growth 
			scog 0d0 aperture) ))
      (values median-mag (length dmag-list)))))
	     
      
	     

      
      
      
  
