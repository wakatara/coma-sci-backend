

(in-package instrument-id)


(defclass/inst lowell-31in-direct-ccd (imaging-instrument onechip)
  ((name :initform "LOWELL 31in DIRECT CCD")
   (observatory :initform "lowell")
   (aperture :initform 0.7874) ;; 31 inches
   (saturation-level :initform 65535)
   (non-linear-level :initform 40000)))


  

(defun %lowell-31in-direct-ccd-identify-instrument (fits-file)
  (when (and
	 (equalp (cf:read-fits-header fits-file "TELESCOP") "31-in")
	 (equalp (cf:read-fits-header fits-file "DETECTOR") "TH7883")
	 (search "Lowell" (cf:read-fits-header fits-file "OBSERVAT")
		 :test 'equalp))
    (make-instance 'lowell-31in-direct-ccd)))

(%add-instrument-id-function '%lowell-31in-direct-ccd-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst lowell-31in-direct-ccd)
						fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("TELESCOP" "DETECTOR" "OBSERVAT" "IMAGETYP" "DATE-OBS" "UT" "SCALE"
      "TELRA" "TELDEC" "EPOCH")
    (call-next-method))
   :test 'equalp))

(defmethod get-standard-filter-for-instrument
    ((inst lowell-31in-direct-ccd) fits-file)
  (let* ((filter (%gethead-or-error fits-file "FILTER")))
    (cond ((equalp filter "U") :uj)
	  ((equalp filter "B") :bj)
	  ((equalp filter "V") :vj)
	  ((equalp filter "R") :rc)
	  ((equalp filter "I") :ic)
	  ((equalp filter "none") :empty)
	  ;;;
	  (t NIL))))

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst lowell-31in-direct-ccd) fits-file) ... )

(defmethod get-object-for-instrument ((inst lowell-31in-direct-ccd) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument
    ((inst lowell-31in-direct-ccd) fits-file)
  (let ((obj-type (%gethead-or-error fits-file  "IMAGETYP")))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "FLAT") :FLAT)
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))

(defmethod get-mjd-start-for-instrument
    ((inst lowell-31in-direct-ccd) fits-file)
  (let* ((date-string ;; yyyy/mm/dd
	   (%gethead-or-error fits-file "DATE-OBS"))
	 (ut-string (%gethead-or-error fits-file "UT"))
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
;(defmethod get-mjd-mid-for-instrument ((inst lowell-31in-direct-ccd) fits-file) ...  )

(defmethod get-gain-for-instrument ((inst lowell-31in-direct-ccd) fits-file
				    &key (extension nil))
  (declare (ignore extension))
  (%gethead-or-error fits-file "GAIN"))

;(defmethod get-chip-id-for-instrument ((inst lowell-31in-direct-ccd) fits-file) ..)

;; DATASEC header is present
;;(defmethod get-datasec-for-instrument
;;    ((inst lowell-31in-direct-ccd) fits-file &key extension)
;;  (declare (ignore extension))
;;  (vector 33 1056 1 2304))

;; note that the WCS inside lowell prism is actually a bogus linear one (at least
;; in some old images)
(defmethod  get-pixel-scale-for-instrument
    ((inst lowell-31in-direct-ccd) fits-file
     &key extension)
  (declare (ignore inst extension))
  (let ((scale  (%gethead-or-error fits-file "SCALE")))
    ;; when a scale of 1.25 is reported, we measure 1.22, so
    ;; we substitute our better value if given 1.25
    (if (< (abs (- scale 1.25)) 0.001)
	1.22
	scale)))


(defmethod get-initial-wcs-for-instrument
    ((inst lowell-31in-direct-ccd) fits-file &key (extension))
  (declare (ignore extension))
  (let* ((rastr (%gethead-or-error fits-file "TELRA"))
	 (decstr (%gethead-or-error fits-file "TELDEC"))
	 (naxis1 (%gethead-or-error fits-file "NAXIS1"))
	 (naxis2 (%gethead-or-error fits-file "NAXIS2"))
	 (pixel-scale (/ (get-pixel-scale-for-instrument inst fits-file)
			 3600d0))
	 (ra (ra-dec:hms-string->deg rastr))
	 (dec (ra-dec:dms-string->deg decstr))
	 (epoch (%gethead-or-error fits-file "EPOCH")))

    
    ;; convert to 2000 if necessary
    (when (not (= epoch 2000d0))
      (multiple-value-setq (ra dec)
	(precess-ra-dec-to-j2000 epoch ra dec)))

     (wcs:make-wcs-radec-tan
		:crval1 ra
		:crval2 dec
		:crpix1 (* 0.5d0 naxis1)
		:crpix2 (* 0.5d0 naxis2)
		:cd1_1  0d0
		:cd2_2  0d0
		:cd1_2  (* +1 pixel-scale)
		:cd2_1  (* -1 pixel-scale)
		:equinox 2000d0)))






;; nothing to do - initial WCS exists
(defmethod insert-initial-wcs-for-instrument
    ((inst lowell-31in-direct-ccd) fits-file
     &key extension)
  (declare (ignore extension))
  (cf:write-wcs (get-initial-wcs-for-instrument inst fits-file) fits-file))




