

(in-package instrument-id)

;; parent class of megacam
(defclass %kpno-mosaic (imaging-instrument)
  ((name :initform "KPNO Mosaic") ;; a string
   (aperture :initform 4.0) ;; Mayall 4m
   (observatory :initform "KPNO")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass     %kpno-mosaic-v1.0-mixin () ()) ;; mixin to denote 1.0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/inst %kpno-mosaic-onechip (%kpno-mosaic onechip)
  ())

(defclass/inst kpno-mosaic-one-chip-v1.0 (%kpno-mosaic-v1.0-mixin
					  %kpno-mosaic-onechip)
  ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/inst %kpno-mosaic-array (%kpno-mosaic multichip)
  ())

(defclass/inst kpno-mosaic-array-v1.0 (%kpno-mosaic-v1.0-mixin ;; must go first
				       %kpno-mosaic-array)
  ())

;; there are many generations of mosaic, now at mosaic-3

(defclass kpno-mosaic-unknown (%kpno-mosaic) ())




(defun %kpno-mosaic-identify-instrument (fits-file)
  (when (and
	 (equalp (cf:read-fits-header fits-file "CONTROLR") "Mosaic Arcon")
	 (equalp (cf:read-fits-header fits-file "OBSERVAT") "KPNO"))
    (let ((naxis (cf:read-fits-header fits-file "NAXIS" :extension 1))
	  (nextend (cf:read-fits-header fits-file "NEXTEND" :extension 1)))
    (cond ((and (equalp naxis 0) (equalp nextend 8))
	   (make-instance 'kpno-mosaic-array-v1.0))
	  ((equalp naxis 2)
	   (make-instance 'kpno-mosaic-one-chip-v1.0
			  :chip-id (cf:read-fits-header fits-file "EXTNAME")))
	  (t
	   (make-instance 'kpno-mosaic-unknown))))))

(%add-instrument-id-function '%kpno-mosaic-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst %kpno-mosaic) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("CONTROLR" "OBSERVAT" "OBSTYPE" "MJD-OBS")
    (call-next-method))
   :test 'equalp))

(defmethod get-standard-filter-for-instrument ((inst %kpno-mosaic) fits-file)
  (let ((filter (%gethead-or-error fits-file "FILTER")))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not found in image ~A" fits-file))
    (cond ((eql 0 (search "U k1001"  filter)) :uj)
	  ((eql 0 (search "B Harris" filter)) :bj)
	  ((eql 0 (search "V Harris" filter)) :vj)
	  ((eql 0 (search "R Harris" filter)) :rc)
	  ((eql 0 (search "I Nearly" filter)) :ic)
	  ;;
	  ((eql 0 (search "g SDSS" filter))   :gsdss)
	  ((eql 0 (search "r SDSS" filter))   :rsdss)
	  ((eql 0 (search "i SDSS" filter))   :isdss)
	  ((eql 0 (search "z SDSS" filter))   :zsdss)

	  ;; fixme - there are more filters
	  (t NIL))))

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst %kpno-mosaic) fits-file) ... )

(defmethod get-object-for-instrument ((inst %kpno-mosaic) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst %kpno-mosaic) fits-file)
  (let ((obj-type (%gethead-or-error fits-file  "OBSTYPE")))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "FLAT") :FLAT)
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))

(defmethod get-mjd-start-for-instrument ((inst %kpno-mosaic) fits-file)
  (%gethead-or-error fits-file "MJD-OBS"))

;; generic version OK
;(defmethod get-mjd-mid-for-instrument ((inst XXX) fits-file) ...  )
;(defmethod get-gain-for-instrument ((inst %kpno-mosaic) fits-file)   )
;(defmethod get-chip-id-for-instrument ((inst kpno-mosaic-one-chip) fits-file) ..)

;; read the TNX as a TAN as a first estimate
(defun %kpno-mosaic-bogus-read-tnx (fits extension)
  (wcs:make-wcs-radec-tan
   :crval1 (* 1d0 (cf:read-fits-header fits "CRVAL1" :extension extension))
   :crval1 (* 1d0 (cf:read-fits-header fits "CRVAL2" :extension extension))
   :crpix1 (* 1d0 (cf:read-fits-header fits "CRPIX1" :extension extension))
   :crpix2 (* 1d0 (cf:read-fits-header fits "CRPIX2" :extension extension))
   :cd1_1  (* 1d0 (cf:read-fits-header fits "CD1_1" :extension extension))
   :cd1_2  (* 1d0 (cf:read-fits-header fits "CD1_2" :extension extension))
   :cd2_1  (* 1d0 (cf:read-fits-header fits "CD2_1" :extension extension))
   :cd2_2  (* 1d0 (cf:read-fits-header fits "CD2_2" :extension extension))
   :equinox (* 1d0 (cf:read-fits-header fits "EQUINOX" :extension extension))
   ))


(defmethod  get-pixel-scale-for-instrument
    ((inst %kpno-mosaic-v1.0-mixin) fits-file
     &key extension)
  (err-if-not-image-at-extension inst fits-file "get-pixel-scale" extension)
  (let ((wcs
	  (or 
	   (get-initial-wcs-for-instrument inst fits-file
					   :extension extension)
	      (error "Cannot find WCS for instrument."))))
    (wcs:get-pixel-scale-for-wcs wcs)))

(defmethod get-initial-wcs-for-instrument
    ((inst %kpno-mosaic-v1.0-mixin) fits-file &key (extension nil))
  (err-if-not-image-at-extension inst fits-file "get-initial-wcs" extension)
  (%kpno-mosaic-bogus-read-tnx fits-file extension))

#+nil
(defmethod get-initial-wcs-for-instrument
    ((inst kpno-mosaic-array-v1.0) fits-file &key (extension nil))
  (when (not extension)
    (error "No extension supplied"))
  (%kpno-mosaic-bogus-read-tnx fits-file extension))


