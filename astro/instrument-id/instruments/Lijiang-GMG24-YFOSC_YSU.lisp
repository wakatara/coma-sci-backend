
;; YUNNAN LIJIANG YFOSC imager - warning - a lot of things like
;; filters and image type are uncertain.   Need more images to
;; validate.

(in-package instrument-id)


(defclass/inst lijiang-gmg24-yfosc_ysu (imaging-instrument onechip)
  ((name :initform "LIJIANG-GMG24-YFOSC_YSU")
   (observatory :initform "yunnan")
   (saturation-level :initform 65535)
   (non-linear-level :initform 40000)))


  

(defun %lijiang-gmg24-yfosc_ysu-identify-instrument (fits-file)
  (when (and
	 (equalp (cf:read-fits-header fits-file "INSTRUME") "YFOSC_YSU")
	 (equalp (cf:read-fits-header fits-file "TELESCOP") "GMG2.4"))
    (make-instance 'lijiang-gmg24-yfosc_ysu)))

(%add-instrument-id-function '%lijiang-gmg24-yfosc_ysu-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument
    ((inst lijiang-gmg24-yfosc_ysu) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("INSTRUME" "TELESCOP" "YFLTNM" "IMAGETYP" "DATE-OBS"
      "CCDSUM" "RA" "DEC")
    (call-next-method))
   :test 'equalp))

;; FIXME - don't know if these are correct
(defmethod get-standard-filter-for-instrument
    ((inst lijiang-gmg24-yfosc_ysu) fits-file)
  (let* ((filter (%gethead-or-error fits-file "YFLTNM")))
    (cond ((equalp filter "JR") :rc)
	  ((equalp filter "JI") :ic)
	  ((equalp filter "JV") :vj)
	  ((equalp filter "JB") :bj)
	  ((equalp filter "UB") :uj)
	  ;;;
	  (t NIL))))

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst lijiang-gmg24-yfosc_ysu) fits-file) ... )

(defmethod get-object-for-instrument ((inst lijiang-gmg24-yfosc_ysu) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

;; FIXME - we have no idea here
(defmethod get-object-type-for-instrument
    ((inst lijiang-gmg24-yfosc_ysu) fits-file)
  (let ((obj-type (%gethead-or-error fits-file  "IMAGETYP")))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "FLAT") :FLAT)
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OBJECT)))) ;; this is a GUESS

(defmethod get-mjd-start-for-instrument ((inst lijiang-gmg24-yfosc_ysu) fits-file)
  (let* ((date-string  (%gethead-or-error fits-file "DATE-OBS"))
	 (mjd-start
	   (multiple-value-bind (year month day hour min sec)
	       (astro-time:parse-ut-date-and-time-string date-string)
	     (astro-time:calendar-date-to-mjd year month day hour min sec))))
    mjd-start))


;; generic version OK
;(defmethod get-mjd-mid-for-instrument ((inst lijiang-gmg24-yfosc_ysu) fits-file) ...  )

(defmethod get-gain-for-instrument ((inst lijiang-gmg24-yfosc_ysu) fits-file
				    &key (extension nil))
  (declare (ignore extension))
  ;; this is on the whiteboard at Lowell obs room - the header
  ;; AGAIN_01 is unreliable
  2.72)

;(defmethod get-chip-id-for-instrument ((inst lijiang-gmg24-yfosc_ysu) fits-file) ..)

(defmethod get-datasec-for-instrument
    ((inst lijiang-gmg24-yfosc_ysu) fits-file &key extension)
  (declare (ignore extension))
  (vector 54 2101 2 2063))



;; this telescope has invali RA and dec - non-standard compliant
(defun %parse-bogus-yfosc-ra (ra-string)
  (let ((h (parse-integer (subseq ra-string 0 2)))
	(m (parse-integer (subseq ra-string 2 4)))
	(s (jk-parse-float:parse-float (subseq ra-string 4))))
    (ra-dec:hms-seq->deg (list h m s))))


(defun %parse-bogus-yfosc-dec (dec-string)
  (let ((sign (subseq dec-string 0 1))
	(d (parse-integer (subseq dec-string 1 3)))
	(m (parse-integer (subseq dec-string 3 5)))
	(s (jk-parse-float:parse-float (subseq dec-string 5))))
    (ra-dec:dms-seq->deg
     (list (if (equalp sign "-") -1 +1)
	    d m s))))




;; fixme - how do we decide if we have a valid wcs?
(defmethod get-initial-wcs-for-instrument
    ((inst lijiang-gmg24-yfosc_ysu) fits-file &key (extension))
  (declare (ignore extension))
  (or
   ;; try to read a fixed wcs, because we might have tried
   ;; to make one
   (ignore-errors (cf:read-wcs fits-file))
   ;;
   ;; or kludge one together from the broken headers
   (let* ((binning-string (%gethead-or-error fits-file "CCDSUM"))
	  (pix-cent
	    (cond ((equalp binning-string "1 1") 1023d0)
		  ((equalp binning-string "2 2") 511d0)
		  (t
		   (error "lijiang-gmg24-yfosc_ysu - get-initial-wcs - Don't know binning ~A" binning-string))))
	  ;; the correct pixel scale
	  (pix-scale
	    (cond ((equalp binning-string "1 1") 7.861111d-5)
		  ((equalp binning-string "2 2")  1.5722222d-4))))
     ;;
     (wcs:make-wcs-radec-tan	   ;; their equinox is bogus :(
      :crval1 (%parse-bogus-yfosc-ra ;; totally broken WCS
	       (%gethead-or-error fits-file "RA"))
      :crval2 (%parse-bogus-yfosc-dec
	       (%gethead-or-error fits-file "DEC"))
      :cd1_1 pix-scale ;; E right
      :cd1_2 0d0
      :cd2_1 0d0
      :cd2_2 pix-scale
      :crpix1 pix-cent :crpix2 pix-cent
      :equinox (let* ((equinox (%gethead-or-error fits-file "EQUINOX")))
		 (cond ((realp equinox) ;; managed to parse dbl
			(float equinox 1d0))
		       ((equalp equinox "J2000")
			2000d0)
		       (t
			(error "Don't know how to parse equinox=~A"
			       equinox))))))))

;; this will insert a semi-decent wcs
(defmethod insert-initial-wcs-for-instrument
    ((inst lijiang-gmg24-yfosc_ysu) fits-file
     &key extension)
  (declare (ignore extension))
  (cf:write-wcs (get-initial-wcs-for-instrument inst fits-file) fits-file))



