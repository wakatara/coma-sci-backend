

;; Discover Channel Telescope Large Monolithic Imager

;; currently called the Lowell Discovery Telescope

(in-package instrument-id)


(defclass/inst dct-lmi (imaging-instrument onechip)
  ((name :initarg :name :initform "DCT-LMI"
	 :accessor instrument-name) ;; a string
   (observatory :initarg :observatory  :initform "DCT"
		:accessor instrument-observatory)
   (aperture :initform 4.3)
   ;; 
   (saturation-level :initform 65535)
   (non-linear-level :initform 40000))) ;; THIS IS AN UNEMEDJUCATED GUESS


  

(defun %dct-lmi-identify-instrument (fits-file)
  (when (and (equalp (cf:read-fits-header fits-file "TELESCOP") "DCT")
	     (equalp (cf:read-fits-header fits-file "DETECTOR")
		     "CCD231-C6-F08_SN-10413-07-01")
	     (equalp (cf:read-fits-header fits-file "TELESCOP") "DCT"))
    (make-instance 'dct-lmi)))

(%add-instrument-id-function '%dct-lmi-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst dct-lmi) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("TELESCOP" "DETECTOR" "FILTERS" "OBSTYPE" "DATE-OBS" "TELRA" "TELDEC"
      "CTYPE1U" "CRPIX1U" "CRVAL1U" "CD1_1U"
      "CFINT1" "CTYPE2U" "CRPIX2U" "CRVAL2U"
      "CD2_2U" "CD1_2U" "CD2_1U" "CFINT2")
    (call-next-method))
   :test 'equalp))

(defmethod get-standard-filter-for-instrument ((inst dct-lmi) fits-file)
  (let* ((filter (%gethead-or-error fits-file "FILTERS"))) ;; composite filt
    (cond ((equalp filter "U") :uj)
	  ((equalp filter "B") :bj)
	  ((equalp filter "V") :vj)
	  ((equalp filter "R") :rc)
	  ((equalp filter "I") :ic)
	  ((equalp filter "VR") :vr)
	  ((equalp filter "none") :empty)
	  ;;;
	  (t NIL))))

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst dct-lmi) fits-file) ... )

(defmethod get-object-for-instrument ((inst dct-lmi) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst dct-lmi) fits-file)
  (let ((obj-type (%gethead-or-error fits-file  "OBSTYPE")))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "FLAT") :FLAT)
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))

(defmethod get-mjd-start-for-instrument ((inst dct-lmi) fits-file)
  (let* ((date-string  (%gethead-or-error fits-file "DATE-OBS"))
	 (mjd-start
	   (multiple-value-bind (year month day hour min sec)
	       (astro-time:parse-ut-date-and-time-string date-string)
	     (astro-time:calendar-date-to-mjd year month day hour min sec))))
    mjd-start))


;; generic version OK
;(defmethod get-mjd-mid-for-instrument ((inst dct-lmi) fits-file) ...  )

;; generic verison is OK
;;(defmethod get-gain-for-instrument ((inst dct-lmi) fits-file &key extension)


;(defmethod get-chip-id-for-instrument ((inst dct-lmi) fits-file) ..)

(defmethod get-datasec-for-instrument ((inst dct-lmi) fits-file &key extension)
  (declare (ignore extension))
  (vector 27 3095 3 3078)) ;; not too sure - this was TRIMSEC in reduc

;; Has INVALID WCS to start - no CRVAL - if IPA is zero, all bets are
;; off and we need to verify
(defmethod get-initial-wcs-for-instrument ((inst dct-lmi) fits-file &key extension)
  (declare (ignore extension))
  (let* ((cd1_1    (%gethead-or-error fits-file "CD1_1"))
	 (cd1_2    (%gethead-or-error fits-file "CD1_2"))
	 (cd2_2    (%gethead-or-error fits-file "CD2_2"))
	 (cd2_1    (%gethead-or-error fits-file "CD2_1"))
	 (crpix1   (%gethead-or-error fits-file "CRPIX1"))
	 (crpix2   (%gethead-or-error fits-file "CRPIX2"))
	 (equinox  (%gethead-or-error fits-file "EQUINOX"))
	 (ra       (ra-dec:hms-string->deg
		    (%gethead-or-error fits-file "TELRA")))
	 (dec      (ra-dec:dms-string->deg
		    (%gethead-or-error fits-file "TELDEC")))
	 ;; Instrument Position Angle
	 (ipa      (%gethead-or-error fits-file "IPA")))
    ;;
    (when (not (zerop ipa))
      (error "Modify instrument-id:get-initial-wcs-for-instrument to
handle IPA=0 case.  If WCS reflects rotator angle, nothing need be
done. Otherwise, change code to rotate WCS CD matrix."))
	 ;;
    (wcs:make-wcs-radec-tan
     :crpix1 crpix1
     :crpix2 crpix2
     :crval1 ra
     :crval2 dec
     :cd1_1 cd1_1 :cd1_2 cd1_2
     :cd2_2 cd2_2 :cd2_1 cd2_1
     :equinox equinox)))



;; Has slightly invalid initial wcs, and some headers that prevent
;; terapix from dealing with wcs
(defmethod insert-initial-wcs-for-instrument ((inst dct-lmi) fits-file &key extension)
  (declare (ignore extension))
  (cf:write-fits-comment
   fits-file "Deleting headers that intefere with Astromatic.")
  (loop for header in '("CTYPE1U" "CRPIX1U" "CRVAL1U" "CD1_1U"
			"CFINT1" "CTYPE2U" "CRPIX2U" "CRVAL2U"
			"CD2_2U" "CD1_2U" "CD2_1U" "CFINT2")
	for val = (cf:read-fits-header fits-file header)
	when header
	  do (cf:delete-fits-header fits-file header)
	     (cf:write-fits-header
	      fits-file
	      (format nil "DELETED.~A" header) val))
  ;;
  (cf:write-wcs (get-initial-wcs-for-instrument inst fits-file)
		fits-file))




