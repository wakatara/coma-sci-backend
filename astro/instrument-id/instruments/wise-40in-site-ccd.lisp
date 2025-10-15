#|

Camera on 40 inch Wise Telescope, Israel - vignetted field, some distortion.

|#

(in-package instrument-id)


(defclass/inst wise-40in-site-ccd (imaging-instrument onechip)
  ((name :initform "WISE-40IN-SITE-CCD") ;; a string
   (observatory :initform "Wise")
   (aperture :initform 1.016))) ;; 40in





(defun %wise-40in-site-ccd-identify-instrument (fits-file)
  (let ((telescop (cf:read-fits-header fits-file "TELESCOP"))
	(observat (cf:read-fits-header fits-file "OBSERVAT"))
	(instrume (cf:read-fits-header fits-file "INSTRUME")))
    (when (and (equal telescop "40 INCH")
	       (equal observat  "WISE")
	       (equal instrume  "site-ccd"))
      (make-instance 'wise-40in-site-ccd))))
	

(%add-instrument-id-function '%wise-40in-site-ccd-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst wise-40in-site-ccd) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("TELESCOP" "OBSERVAT" "INSTRUME" "DATE-OBS" "UT" "SECPIX"
      "RA" "DEC" "EPOCH")
    (call-next-method))
   :test 'equalp))

;; R,B are OK - not sure abot others
(defmethod get-standard-filter-for-instrument ((inst wise-40in-site-ccd) fits-file)
  (let ((filter (%gethead-or-error fits-file "FILTER")))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not inserted into WISE-40IN-SITE-CCD image ~A" fits-file))
    (cond ((equalp filter "U") :uj)
	  ((equalp filter "B") :bj)
	  ((equalp filter "V") :vj)
	  ((equalp filter "R") :rc)
	  ((equalp filter "I") :ic)
	  ((equalp filter "Blank") :open)
	  ;;;
	  (t NIL))))

;; exposures seem to be negative?!
(defmethod get-exptime-for-instrument ((inst wise-40in-site-ccd) fits-file)
  (declare (ignore inst))
  (%gethead-or-error fits-file "EXPTIME"))
  

(defmethod get-object-for-instrument ((inst wise-40in-site-ccd) fits-file)
  (let ((obj (%gethead-or-error fits-file "OBJECT")))
    (if (not (equal obj "Unknown"))
	obj
	nil)))

(defmethod get-object-type-for-instrument ((inst wise-40in-site-ccd) fits-file)
  (let ((obj (%gethead-or-error  fits-file  "OBJECT")))
    (cond ((search obj "BIAS" :test 'equalp) :BIAS)
	  ((search obj "FLAT" :test 'equalp) :FLAT)
	  ((search obj "Unknown" :test 'equalp) nil)
	  (T :OBJECT))))


(defmethod get-mjd-start-for-instrument ((inst wise-40in-site-ccd) fits-file)
  (astro-time:parse-ut-date-and-time-string-to-mjd
   (concatenate 'string
		(%gethead-or-error fits-file "DATE-OBS")
		"T"
		(%gethead-or-error fits-file "UT"))))

(defmethod get-mjd-mid-for-instrument ((inst wise-40in-site-ccd) fits-file)
 (+ (get-mjd-start-for-instrument inst fits-file)
     (* 0.5d0 (/ (get-exptime-for-instrument inst fits-file) #.(* 24d0 3600)))))

(defmethod get-gain-for-instrument ((inst wise-40in-site-ccd) fits-file  &key extension)
  (declare (ignorable inst fits-file extension))
  (%gethead-or-error fits-file "GAIN")) ;; from the PDS web site (see top of this file)
  

(defmethod get-pixel-scale-for-instrument ((inst wise-40in-site-ccd) fits-file &key extension)
  (declare (ignorable inst fits-file extension))
  (%gethead-or-error fits-file "SECPIX"))


;;(defmethod get-datasec-for-instrument ((inst wise-40in-site-ccd) fits-file &key extension) )



;; See if a WCS was inserted; if not, try an RA,DEC, either decimal degrees or xx:mm:ss
(defmethod get-initial-wcs-for-instrument ((inst wise-40in-site-ccd) fits-file
					   &key extension)
  (declare (ignore extension))
  (or (cf:read-wcs fits-file) ;; if WCS was inserted use that
      ;; else try ra,dec as degrees or HH:MM:SS DD:MM:SS
      (let* ((ra-raw (cf:read-fits-header fits-file "RA"))
	     (dec-raw (cf:read-fits-header fits-file "DEC"))
	     (equinox (cf:read-fits-header fits-file "EPOCH"))
	     (ra (ra-dec:hms-string->deg ra-raw))
	     (dec (ra-dec:dms-string->deg dec-raw)))

	
	(cond ((not (and ra dec equinox))
	       nil) ;; no WCS
	      (t
	       (let ((pixel-scale (* 1/3600 (get-pixel-scale-for-instrument inst fits-file)))
		     (naxis1 (%gethead-or-error fits-file "NAXIS1"))
		     (naxis2 (%gethead-or-error fits-file "NAXIS2")))
		 ;; precess to J2000
		 (multiple-value-setq (ra dec)
		     (precess-ra-dec-to-j2000 equinox ra dec))
		 
		 (wcs:make-wcs-radec-tan
		  :crval1 ra
		  :crval2 dec
		  :crpix1 (* 0.5d0 naxis1)
		  :crpix2 (* 0.5d0 naxis2)
		  :cd1_1  0d0
		  :cd2_2  0d0
		  :cd1_2  (* -1 pixel-scale) 
		  :cd2_1  (* +1 pixel-scale) 
		  :equinox 2000d0))))))) 
		    
(defmethod insert-initial-wcs-for-instrument ((inst wise-40in-site-ccd) fits-file &key extension)
  (declare (ignore extension))
  (when (not (cf:read-wcs fits-file))
    (let ((wcs (get-initial-wcs-for-instrument inst fits-file)))
      (when (not wcs)
	(error "No initial WCS to write"))
      (cf:write-wcs wcs fits-file))))
					      
      





