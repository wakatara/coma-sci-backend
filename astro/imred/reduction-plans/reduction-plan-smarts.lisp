
;; WARNING - THIS IS FOR THE 7X7' single amp mode only!

(in-package imred)


;; reduction plan suitable for smarts
;;
(defclass reduction-plan-smarts-7x7-1amp (reduction-plan)
  ((inst-id-type :initform 'instrument-id::smarts-FIXME)
   (trimsec :initform #(31 1054 1 1024))
   (zero-name :initform "BIAS")  
   (flat-name :initform "SKY FLAT")
   (flat-basename :initform "FLAT")
   (input-fits-patch-function :initform
			      'smarts-input-fits-patch-function-7x7-1amp)
   (output-fits-patch-function :initform
			       'smarts-final-fits-patch-function-7x7-1amp)))



;; FIXME - there is no SMARTS in INSTRUMENT-ID
#+nil
(defmethod get-reduction-plan-for-instrument ((inst instrument-id:smarts-7x7-1amp))
  (declare (ignorable inst))
  'reduction-plan-smarts-7x7-1amp)

;; smarts data are missing gain
(defun smarts-input-fits-patch-function-7x7-1amp (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
  (cf:write-fits-header fits-file "GAIN" 3.5))


;; the function to patch an output fits file
;; * create MJD header

(defun smarts-final-fits-patch-function-7x7-1amp (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
  ;; create the MJD headers
  (let* ((full-ut-string (%gethead-or-error fits-file "DATE-OBS"))
	 (exptime (%gethead-or-error fits-file "EXPTIME"))
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

  ;; put in fake wcs, assuming N-S alignment
  (let* ((ra  (or (ra-dec:hms-string->deg (cf:read-fits-header fits-file "RA"))
		     (error "No RA header in ~A" fits-file)))
	 (dec (or (ra-dec:dms-string->deg (cf:read-fits-header fits-file "DEC"))
		     (error "No DEC header in ~A" fits-file)))
	 ;; adjustments for not being centered on chip 
	 (ra-adj  (/ (/ 0.0 60d0) (cos (* (/ pi 180) dec))))
	 (dec-adj (/ 0.0 60d0))
	 ;;
	 (crval1 (+ ra ra-adj))
	 (crval2 (+ dec dec-adj))
	 ;;
	 (crpix1 (* 0.5 1027d0))
	 (crpix2 (* 0.5 1012d0))
	 (pixsize (/ 0.396d0 3600d0)))
    
    (let ((wcs 
	   (wcs:make-wcs-radec-tan 
	    :crval1 crval1 :crval2 crval2
	    :crpix1 crpix1 :crpix2 crpix2
	    :cd1_1 (- pixsize)  ;; west right
	    :cd2_2 (- pixsize)  ;; north down
	    :cd1_2 0d0 :cd2_1 0d0
	    :equinox 2000d0)))
      (cf:write-wcs wcs fits-file))
    
    ))
	 
  
