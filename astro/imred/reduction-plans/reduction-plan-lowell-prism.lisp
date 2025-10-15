


(in-package imred)


;; reduction plan suitable for Lowell 
;;
(defclass reduction-plan-lowell-prism (reduction-plan)
  ((inst-id-type :initform 'instrument-id:lowell-prism)
   (trimsec :initform "TRIMSEC.IMRED")
   (zero-name :initform "BIAS")  ;
   (flat-basename :initform "FLAT")
   ;; filter wheel 3 has the normal broadband filtes
   (flat-name   :initform "FLAT")
   (max-flat-counts :initform 40000) ;; non-linear region
   (input-fits-patch-function :initform
			      'lowell-prism-input-fits-patch-function)
   (output-fits-patch-function :initform
			       'lowell-prism-output-fits-patch-function)))



(defmethod get-reduction-plan-for-instrument ((inst instrument-id:lowell-prism))
  (declare (ignorable inst))
  'reduction-plan-lowell-prism)

(defun lowell-prism-input-fits-patch-function (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
    ;; insert best guess at TRIMSEC.IMRED because Prism has various array sizes
  (let ((trimsec-vec (instrument-id:get-trimsec-for-fits fits-file)))
    (cf:write-fits-header fits-file "TRIMSEC.IMRED"
			  (format nil "[~D:~D,~D:~D]"
				  (aref trimsec-vec 0)
				  (aref trimsec-vec 1)
				  (aref trimsec-vec 2)
				  (aref trimsec-vec 3))))
  (cf:write-fits-header fits-file "GAIN" 2.72
			:comment "On whiteboard at Lowell - may change :("))

  

  
(defun lowell-prism-output-fits-patch-function (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
  ;; create the MJD headers
  (let* ((date-string  (%gethead-or-error fits-file "DATE-OBS"))
	 (exptime      (%gethead-or-error fits-file "EXPTIME"))
	 (full-ut-string  date-string)
	 (mjd-start
	   (multiple-value-bind (year month day hour min sec)
	       (astro-time:parse-ut-date-and-time-string full-ut-string)
	     (astro-time:calendar-date-to-mjd year month day hour min sec)))
	 (mjd-midpt (+ mjd-start (/ exptime (* 24d0 3600)))))
    
    
    (cf:write-fits-header  fits-file  "UTDATE" full-ut-string
			   :comment "UT of start of observation")
    (cf:write-fits-header  fits-file  "MJD" mjd-start 
			   :comment "start MJD of obs")
    (cf:write-fits-header  fits-file "MJDSTART" mjd-start 
			   :comment "start MJD of obs")
    (cf:write-fits-header  fits-file "MJDMID" mjd-midpt 
			   :comment "midpoint MJD of obs"))

  
  ;;
  ;; insert a WCS - lowell WCS is bogus
   (let* 
       ((ra (or (ra-dec:hms-string->deg (%gethead-or-error fits-file "TELRA"))
		(error "TELRA header not found in ~A" fits-file)))
	(dec (or (ra-dec:dms-string->deg (%gethead-or-error fits-file "TELDEC"))
		 (error "TELDEC header not found in ~A" fits-file)))
	(pixscale/arcsec (or (%gethead-or-error fits-file "PIXSCAL")
			     (error "PIXSCAL header not found in ~A" fits-file)))
	(pixscale (/ pixscale/arcsec 3600))
	(wcs
	  (wcs:make-wcs-radec-tan 
	   :crval1 ra :crval2 dec
	   :crpix1 1024d0 :crpix2 1024d0 
	   :cd1_1 (- pixscale)
	   :cd2_2 pixscale
	   :equinox 2000d0
	    )))
     (cf:delete-fits-header fits-file "CRDELT1")
     (cf:delete-fits-header fits-file "CRDELT2")
     (cf:write-wcs wcs fits-file)

  ))
	 
  
