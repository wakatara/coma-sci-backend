#|

OPTIC camera on UH88 - 2x 2x4K chips, but read out as 2 amps per
chip, so we pretend it is is 1 chip.

The chips/amps don't appear to be read in a physically consistent manner, so
there is no WCS.

https://www.ifa.hawaii.edu/88inch/2.2-meter-technical.htm

https://www.ifa.hawaii.edu/88inch/instruments/optic/optic.pdf


|#

(in-package instrument-id)


(defclass/inst uh88-optic (imaging-instrument onechip)
  ((name :initform "UH88-OPTIC") ;; a string
   (aperture :initform 1.22)
   (observatory :initform "MKO")))





(defun %uh88-optic-identify-instrument (fits-file)
  (let ((instrume (cf:read-fits-header fits-file "INSTRUME"))
	(telescop (cf:read-fits-header fits-file "TELESCOP")))
    (when (and (equalp telescop "2.2m UH")
	       (string-utils:string-starts-with instrume "OPTIC"))
      (make-instance 'uh88-optic))))
	

(%add-instrument-id-function '%uh88-optic-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst uh88-optic) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("INSTRUME" "TELESCOP" "BASENAME" "DATE-OBS" 
      "SECPIX1" "SECPIX2")
      (call-next-method))
   :test 'equalp))

(defmethod get-standard-filter-for-instrument ((inst uh88-optic) fits-file)
  (let ((filter (%gethead-or-error fits-file "FILTER")))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not inserted into UH88-OPTIC image ~A" fits-file))
    (cond ((equalp filter "U") :uj)
	  ((equalp filter "B") :bj)
	  ((equalp filter "V") :vj)
	  ((equalp filter "R") :rc)
	  ((equalp filter "I") :ic)
	  ;;;
	  (t NIL))))

;; exposures seem to be negative?!
(defmethod get-exptime-for-instrument ((inst uh88-optic) fits-file)
  (declare (ignorable inst))
  (%gethead-or-error fits-file "EXPTIME"))
  

(defmethod get-object-for-instrument ((inst uh88-optic) fits-file)
  (declare (ignorable inst))
  (%gethead-or-error fits-file "BASENAME"))


;; this is a wild guess
(defmethod get-object-type-for-instrument ((inst uh88-optic) fits-file)
  (let ((obj (%gethead-or-error  fits-file  "BASENAME")))
    (cond ((search obj "BIAS" :test 'equalp) :BIAS)
	  ((search obj "FLAT" :test 'equalp) :FLAT)
	  ((search obj "Unknown" :test 'equalp) nil)
	  (T :OBJECT))))


(defmethod get-mjd-start-for-instrument ((inst uh88-optic) fits-file)
  (astro-time:parse-ut-date-and-time-string-to-mjd
   (concatenate 'string
		(%gethead-or-error fits-file "DATE-OBS")
		"T"
		(%gethead-or-error fits-file "UT"))))


(defmethod get-mjd-mid-for-instrument ((inst uh88-optic) fits-file)
  (+ (get-mjd-start-for-instrument inst fits-file)
     (* 0.5d0 (/ (get-exptime-for-instrument inst fits-file) #.(* 24d0 3600)))))


  

(defmethod get-gain-for-instrument ((inst uh88-optic) fits-file  &key extension)
  (declare (ignorable inst extension))
  (%gethead-or-error fits-file "GAIN"))
  

(defmethod get-pixel-scale-for-instrument ((inst uh88-optic) fits-file &key extension)
  (declare (ignorable inst extension))
  (let ((s1 (%gethead-or-error fits-file "SECPIX1"))
	(s2 (%gethead-or-error fits-file "SECPIX2")))
    (if (= s1 s2)
	s1
	(error "Pixels scales in two directions not same for UH88-OPTIC"))))
    



;; (defmethod get-datasec-for-instrument ((inst uh88-optic) fits-file &key extension) ..)

;; the four amps are not arranged consistently, so no WCS
(defmethod get-initial-wcs-for-instrument ((inst uh88-optic) fits-file
					   &key extension)
  (declare (ignore inst fits-file extension))
  nil)

;; this wcs WOULD work for upper right quadrant, sometimes (?)
#+nil
(defmethod get-initial-wcs-for-instrument ((inst uh88-optic) fits-file
					   &key extension)
  (declare (ignore extension))
  (or (cf:read-wcs fits-file) ;; if WCS was inserted use that
      ;; else try ra,dec as degrees or HH:MM:SS DD:MM:SS
      (let* ((ra-raw (cf:read-fits-header fits-file "RA"))
	     (dec-raw (cf:read-fits-header fits-file "DEC"))
	     (equinox (cf:read-fits-header fits-file "EQUINOX"))
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
		  :cd1_1  (* -1 pixel-scale) 
		  :cd2_2  (* +1 pixel-scale) 
		  :cd1_2  0d0
		  :cd2_1  0d0
		  :equinox 2000d0))))))) 
		    
(defmethod insert-initial-wcs-for-instrument ((inst uh88-optic) fits-file &key extension)
  (declare (ignore extension))
  (when (not (cf:read-wcs fits-file))
    (let ((wcs (get-initial-wcs-for-instrument inst fits-file)))
      (when (not wcs)
	(error "No initial WCS to write"))
      (cf:write-wcs wcs fits-file)))) 
					      
      





