#|

TNG LRS in imaging mode, called DOLORES in full

http://www.tng.iac.es/instruments/lrs/

UBVRI filter set


|#

(in-package instrument-id)

;; emphasize "IMAGER" in name; instrument is mainly a spectrometer
(defclass/inst tng-dolores-imager (imaging-instrument onechip)
  ((name :initform "TNG DOLORES-IMAGER, La Palma")
   (observatory :initform "LaPalma")
   (aperture  :initform 3.58)
   (saturation-level :initform 65535)
   (non-linear-level :initform 40000)))


  

(defun %tng-dolores-imager-identify-instrument (fits-file)
  (when (and
	 (equalp (cf:read-fits-header fits-file "TELESCOP") "TNG")
	 (equalp (cf:read-fits-header fits-file "INSTRUME") "LRS"))
    (make-instance 'tng-dolores-imager)))

(%add-instrument-id-function '%tng-dolores-imager-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst tng-dolores-imager) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("TELESCOP" "FLT_ID" "OBS-TYP" "DATE-OBS" "GAIN_1"
      "RA-RAD" "DEC-RAD")
    (call-next-method))
   :test 'equalp))

(defmethod get-standard-filter-for-instrument
    ((inst tng-dolores-imager) fits-file)
  (let* ((filter-string (%gethead-or-error fits-file "FLT_ID"))
	 (filter (string (aref filter-string 0))))
    (cond ((equalp filter "U") :uj)
	  ((equalp filter "B") :bj)
	  ((equalp filter "V") :vj)
	  ((equalp filter "R") :rc)
	  ((equalp filter "I") :ic)
	  ;;;
	  (t NIL))))

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst tng-dolores-imager) fits-file) ... )

(defmethod get-object-for-instrument ((inst tng-dolores-imager) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

;; seems to have FLATS listed as OBJECTS
(defmethod get-object-type-for-instrument
    ((inst tng-dolores-imager) fits-file)
  (let ((obj-type (%gethead-or-error fits-file  "OBS-TYPE"))
	(object   (%gethead-or-error fits-file  "OBJECT")))
    (cond ((equalp obj-type "BIAS") :BIAS) ;; just guesses so far
	  ((or (equalp obj-type "FLAT")
	       (equalp "flat" object)
	       ;; the next two are guesses
	       (search "skyflat" (remove #\space object) :test 'equalp)
	       (search "domeflat" (remove #\space object) :test 'equalp))
	       :FLAT)
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))

(defmethod get-mjd-start-for-instrument
    ((inst tng-dolores-imager) fits-file)
  (let* ((date-string ;; yyyy/mm/dd
	   (%gethead-or-error fits-file "DATE-OBS"))
	 (ut-string (%gethead-or-error fits-file "EXPSTART"))
	 (full-date-string
	   (concatenate 'string 
			(substitute #\- #\/ date-string)
			"T" ut-string))
	 (mjd-start
	   (multiple-value-bind (year month day hour min sec)
	       (astro-time:parse-ut-date-and-time-string full-date-string)
	     (astro-time:calendar-date-to-mjd year month day hour min sec))))
    mjd-start))


;; generic version OK
;(defmethod get-mjd-mid-for-instrument ((inst tng-dolores-imager) fits-file) ...  )

(defmethod get-gain-for-instrument ((inst tng-dolores-imager) fits-file
				    &key (extension nil))
  (declare (ignore extension))
  (%gethead-or-error fits-file "GAIN_1"))

;(defmethod get-chip-id-for-instrument ((inst tng-dolores-imager) fits-file) ..)

;; no datasec DATASEC header is present
;;(defmethod get-datasec-for-instrument


(defmethod  get-pixel-scale-for-instrument
    ((inst tng-dolores-imager) fits-file
     &key extension)
  (declare (ignore inst extension))
  (let ((scale 0.276 #+nil 0.252d0) ;; web page value of 0.252 is off vs fit
	;; we get rid of CRDELT 1,2 because they seem toxic to other
	;; WCS users, like scamp
	(bin1 (or
	       (cf:read-fits-header fits-file "XCRDELT1")
	       (%gethead-or-error fits-file "CRDELT1")))
	(bin2 (or
	       (cf:read-fits-header fits-file "XCRDELT2")
	       (%gethead-or-error fits-file "CRDELT2"))))
    (when (not (= bin1 bin2))
      (error "X,Y binning not equal for TNG DOLORES - no pixel scale"))
    (* scale bin1)))


(defmethod get-initial-wcs-for-instrument
    ((inst tng-dolores-imager) fits-file &key (extension))
  (declare (ignore extension))
  ;; there seems to be a bug in header where RA-DEG is really hours,
  ;; so we use the radians version
  (let* ((ra-rad (%gethead-or-error fits-file "RA-RAD")) ;; BUG in headers
	 (dec-rad (%gethead-or-error fits-file "DEC-RAD"))
	 (ra  (* ra-rad (/ 180 pi)))
	 (dec (* dec-rad (/ 180 pi)))
	 (naxis1 (%gethead-or-error fits-file "NAXIS1"))
	 (naxis2 (%gethead-or-error fits-file "NAXIS2"))
	 (pixel-scale (/ (get-pixel-scale-for-instrument inst fits-file)
			 3600d0))
	 (equinox (%gethead-or-error fits-file "EQUINOX")))

    
    ;; convert to 2000 if necessary
    (when (not (= equinox 2000d0))
      (multiple-value-setq (ra dec)
	(precess-ra-dec-to-j2000 equinox ra dec)))

     (wcs:make-wcs-radec-tan
		:crval1 ra
		:crval2 dec
		:crpix1 (* 0.5d0 naxis1)
		:crpix2 (* 0.5d0 naxis2)
		:cd1_1  (* +1 pixel-scale)
		:cd2_2  (* +1 pixel-scale)
		:cd1_2  0d0
		:cd2_1  0d0
		:equinox 2000d0)))






;; nothing to do - initial WCS exists
(defmethod insert-initial-wcs-for-instrument
    ((inst tng-dolores-imager) fits-file
     &key extension)
  (declare (ignore extension))
  (let ((crdelt1 (cf:read-fits-header fits-file "CRDELT1"))	
	(crdelt2 (cf:read-fits-header fits-file "CRDELT2")))
    (when crdelt1 
      (cf:write-fits-header
       fits-file "XCRDELT1" crdelt1
       :comment "INSTRUMENT-ID rmvd CRDELT1 to avoid confusion")
      (cf:delete-fits-header fits-file "CRDELT1"))
    (when crdelt2
      (cf:write-fits-header
       fits-file "XCRDELT2" crdelt2
       :comment "INSTRUMENT-ID rmvd CRDELT2 to avoid confusion")
      (cf:delete-fits-header fits-file "CRDELT2")))
  (cf:write-wcs (get-initial-wcs-for-instrument inst fits-file) fits-file))




