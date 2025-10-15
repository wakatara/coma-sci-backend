

#|

Use one image to calibrate another


|#

(in-package phot-calib)

;; given two sextractor shash'es compute the magnitude to add to 
;; the 2nd one to make it match the first one.
(defun %compute-im-im-mag-offset-from-shashes
  (shash-calibrator shash-to-calibrate ;; sextractor catalog hashes
   &key 
     (mag-keyword "MAG_AUTO")
     (dmag-keyword "MAGERR_AUTO")
     (flux-keyword "FLUX_AUTO")
     (flags-keyword "FLAGS")
     (min-obj-flux 10000)
     (min-calib-stars 20)
     (sextractor-reject-flags #xFFFF)
     (tol/arcsec 1.0)
     (matches-file nil)
     (extra-error 0.01d0)) ;; extra sigma to add to dmag-error
  (let* ((shash1  shash-calibrator)
	 (shash2  shash-to-calibrate)
	 (idvec1  (gethash "NUMBER" shash1))
	 (ravec1   (gethash "ALPHA_J2000" shash1))
	 (decvec1  (gethash "DELTA_J2000" shash1))
	 (magvec1  (gethash mag-keyword shash1))
	 (dmagvec1 (gethash dmag-keyword shash1))
	 (fluxvec1 (gethash flux-keyword shash1))
	 (flagvec1 (gethash flags-keyword shash1))
	 (xpixvec1    (gethash "X_IMAGE" shash1))
	 (ypixvec1    (gethash "Y_IMAGE" shash1))
	 (idvec2  (gethash "NUMBER" shash2))
	 (ravec2   (gethash "ALPHA_J2000" shash2))
	 (decvec2  (gethash "DELTA_J2000" shash2))
	 (magvec2  (gethash mag-keyword shash2))
	 (dmagvec2 (gethash dmag-keyword shash2))
	 (fluxvec2 (gethash flux-keyword shash2))
	 (flagvec2 (gethash flags-keyword shash2))
	 ;; indices in vec2 parallel to vec1
	 (match-indices
	   (aobj:match-ra-dec-by-index ravec1 decvec1 
				       ravec2 decvec2 
				       :tol/arcsec tol/arcsec))
	 dmag-vec dmag-err-vec
	 matches-list)
    
    (loop with dmag-list = nil and dmag-err-list = nil
	  for i1 from 0
	  for i2 across match-indices
	  when (and (plusp i2)  ;; ie, there's a match
		    (zerop (logand sextractor-reject-flags (aref flagvec1 i1)))
		    (zerop (logand sextractor-reject-flags (aref flagvec2 i2)))
		    (>= (aref fluxvec1 i1) min-obj-flux)
		    (>= (aref fluxvec2 i2) min-obj-flux))
	    do
	       (let ((dmag (* 1d0 (- (aref magvec1 i1)
				     (aref magvec2 i2))))
		     (dmag-err (* 1d0 (sqrt (+ (expt (aref dmagvec1 i1) 2)
					       (expt (aref dmagvec2 i2) 2)
					       (expt extra-error 2))))))
		 (when matches-file 
		   (push (vector 
			  (aref idvec2 i2) (aref magvec2 i2) (aref dmagvec2 i2)
			  (aref idvec1 i1) (aref magvec1 i1) (aref dmagvec1 i1)
			  dmag dmag-err 
			  (aref ravec1 i1) (aref decvec1 i1)
			  (aref xpixvec1 i1) (aref ypixvec1 i1)
			  ;; last element is the logical OR of flags 1,2
			  (logior (aref flagvec1 i1) (aref flagvec2 i2))
			  )
			 matches-list))
		 (push dmag dmag-list)
		 (push dmag-err dmag-err-list))
	    finally
	       (setf dmag-vec (coerce dmag-list '(simple-array double-float (*))))
	       (setf dmag-err-vec (coerce dmag-err-list '(simple-array double-float (*)))))

    (when matches-file
      (with-open-file (sout matches-file 
				    :direction :output 
				    :if-does-not-exist :create
				    :if-exists :supersede)
	(format sout "# Matches from image/image calib
# 2 is calibrator, 1 is image being calibrated~%#~%")
	(format 
	 sout
"# ObjN1  ObjMag1  ObjMagErr1 FlagOR12 ObjN2   ObjMag2  ObjMagErr2     Dmag     DmagErr   RA       DEC         Xpix1   Ypix1~%")
	(loop for vec in (reverse matches-list)
	      do (format 
		  sout 
 "~4A   ~7,3F   ~7,3F    ~3D        ~4A   ~7,3F   ~7,3F      ~7,3F   ~7,3F ~9,5F ~9,5F  ~7,1F ~7,1F~%"
                  (aref vec 0) (aref vec 1) (aref vec 2)
		  (aref vec 12) ;; logical OR of flag is at end
		  (aref vec 3) (aref vec 4) (aref vec 5)
		  (aref vec 6) (aref vec 7) (aref vec 8) (aref vec 9)
		  (aref vec 10) (aref vec 11)
		  ))))

    ;;
    (when (< (length dmag-vec) min-calib-stars)
      (error "Too few calib stars - found ~A, but require ~A"
	     (length dmag-vec) min-calib-stars))
    ;;
    (bayes-outlier:bayes-outlier-estimate-mean 
     dmag-vec dmag-err-vec 
     ;; hardwire these values for the background; it shouldn't be sensitive
     :s 0.15 :f 0.15)))
  
  
  
					 
(defun calibrate-image-with-image/mag-auto 
  (fits-calibrator fits-to-calibrate 
		   &key
		     (filter nil)
		     (min-calib-stars 20)
		     (min-obj-flux 10000)
		     (sextractor-reject-flags #xFFFF)
		     (md5-avoid-rerun t)
		     (phot-autoparams '(5.5 5.5)) ;; wide phot-autoparams
		     (tol/arcsec 1.0)
		     (satur-level 20000)
		     (extra-error 0.005d0)
		     (write-headers t))
  "Calibrate one image using an image with MAGZP already defined, using MAG-AUTO
sextractor phtometry."  
  (when
      (and (not filter)
	   (not (eq (instrument-id:get-standard-filter-for-fits fits-calibrator)
		    (instrument-id:get-standard-filter-for-fits fits-to-calibrate))))
    (error 
 "Filters don't match in the two images using instrument-id:get-standard-filter-for-fits"))

  (when (not (and (cf:read-fits-header fits-calibrator "PHOTCALIB.ZPMAG")
		  (cf:read-fits-header fits-calibrator "PHOTCALIB.ZPMAGERR")))
    (error "Image ~A is not calibrated with PHOTCALIB.ZMAG." fits-calibrator))

  (let*  ((fits1 fits-calibrator)
	  (std-filter 
	    (or filter (instrument-id:get-standard-filter-for-fits fits-calibrator)))
	  (fits2 fits-to-calibrate)
	  (phot-calib-catalog-file "sex_phot_calib_mag_auto.cat")
	  (working-dir1 (terapix:get-fits-directory 
			 fits1 :if-does-not-exist t))
	  (phot-calib-catalog-file-fullpath1
	    (format nil "~A/~A" working-dir1 phot-calib-catalog-file))
	  (working-dir2 (terapix:get-fits-directory 
			 fits2 :if-does-not-exist t))
	  (phot-calib-catalog-file-fullpath2
	    (format nil "~A/~A" working-dir2 phot-calib-catalog-file))
	  (matches-file 
	    (format nil "~A/sex_phot_im_im_calib_mag_best_matches.txt"
		    working-dir2)))

    (loop for fits-file in (list fits1 fits2)
	  do   (terapix:run-sextractor 
		fits-file
		:display-errors nil ;; too much output
		:output-catalog phot-calib-catalog-file
		:md5-avoid-rerun md5-avoid-rerun
		:satur-level satur-level
		:deblend-mincont 1.0 ;; don't deblend (eg trails)
		:phot-autoparams phot-autoparams
		:phot-apertures '(5 10 20 30 40)))

    (let* ((shash1 (terapix:read-sextractor-catalog 
		    phot-calib-catalog-file-fullpath1))
	   (shash2 (terapix:read-sextractor-catalog 
		    phot-calib-catalog-file-fullpath2))
	   (outlier-result
	     (%compute-im-im-mag-offset-from-shashes 
	      shash1 shash2
	      :mag-keyword "MAG_AUTO" 
	      :dmag-keyword "MAGERR_AUTO"
	      :flux-keyword "FLUX_AUTO"
	      :min-calib-stars min-calib-stars
	      :sextractor-reject-flags sextractor-reject-flags
	      :min-obj-flux min-obj-flux 
	      :tol/arcsec tol/arcsec
	      :extra-error extra-error
	      :matches-file matches-file))
	   (nstars  (bayes-outlier:outlier-result-n outlier-result))
	   (zp1 (or (cf:read-fits-header fits1 "PHOTCALIB.ZPMAG")
		    (error  "PHOTCALIB.ZPMAG not found in ~A" fits1)))
	   (zperr1 (or (cf:read-fits-header fits1 "PHOTCALIB.ZPMAGERR")
		    (error  "PHOTCALIB.ZPMAGERR not found in ~A" fits1)))
	   (zp2 (+ zp1 (bayes-outlier:outlier-result-xm outlier-result)))
	   (zperr2 
	     (sqrt (+ (expt zperr1 2)
		      (expt (bayes-outlier:outlier-result-xm-err outlier-result) 2)))))
      ;;
      (when write-headers
	(delete-all-photcalib-headers fits2)
	(cf:write-fits-header 
	 fits2 "PHOTCALIB.MAGTYPE" "MAG_AUTO")
	(cf:write-fits-header
	 fits2 "PHOTCALIB.MAGAUTOCOR" 0.0
	 :comment "Addit. cor. for MAG_AUTO")
	(cf:write-fits-header
	 fits2 "PHOTCALIB.ZPMAG" zp2
	 :comment "Mag to add to ADU-flux mag")
	(cf:write-fits-header 
       fits2 "PHOTCALIB.ZPMAGERR" zperr2
       :comment "Error on PHOTCALIB.ZPMAG")
	(cf:write-fits-header
	 fits2 "PCZPMAG" zp2
	 :comment "Synonym for PHOTCALIB.ZPMAG")
	(cf:write-fits-header
	 fits2 "PCEZPMAG" zperr2 
	 :comment "Synonym for PHOTCALIB.ZPMAGERR")
	(cf:write-fits-header 
	 fits2 "PHOTCALIB.NSTARS" nstars 
	 :comment "No. of stars used for PHOTCALIB.ZPMAG")
	(cf:write-fits-header 
	 fits2 "PHOTCALIB.FILTER" std-filter 
	 :comment "Assumed filter in PHOTCALIB package")
	(cf:write-fits-header 
	 fits2  "PHOTCALIB.CATALOGTYPE" "NONE"
	 :comment "image-image calibration")
	(cf:write-fits-header 
	 fits2  "PHOTCALIB.CALIBRATOR" 
	 (file-io:file-minus-dir fits1)))
	;;
	(values zp2 zperr2 nstars))))
					
      
	   
      
		   
