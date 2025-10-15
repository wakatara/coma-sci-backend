#|

Subaru FOCAS in imaging mode 

https://www.naoj.org/Observing/Instruments/FOCAS/ccdinfo.html

|#




(in-package instrument-id)


(defclass %subaru-focas (imaging-instrument)
  ((name :initform "Subaru FOCAS 2 chip camera") ;; a string
   (aperture :initform 8.2)
   (observatory :initform "MKO")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/inst subaru-focas-onechip (%subaru-focas onechip)
  ())

;; not even sure if this exists
(defclass/inst subaru-focas-array (%subaru-focas onechip)
  ())



(defun %subaru-focas-identify-instrument (fits-file)

  (when (and  (equalp (cf:read-fits-header fits-file "TELESCOP") "Subaru")
	      (equalp (cf:read-fits-header fits-file "INSTRUME") "FOCAS"))
    (if (not (cf:read-fits-header fits-file "EXTEND"))
	(make-instance 'subaru-focas-onechip
		       :chip-id  (cf:read-fits-header fits-file "DET-ID"))
	(make-instance 'subaru-focas-array))))



(%add-instrument-id-function '%subaru-focas-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst %subaru-focas) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("FILTER01" "DATA-TYP" "MJD-STR")
    (call-next-method))
   :test 'equalp))

;; FIXME - this is a guess based on Suprime-Cam
(defmethod get-standard-filter-for-instrument ((inst %subaru-focas) fits-file)
  (let ((filter (%gethead-or-error fits-file "FILTER01")))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not found in image ~A" fits-file))
    (cond ((equalp filter "W-J-U") :uj)
	  ((equalp filter "W-J-B") :bj)
	  ((equalp filter "W-J-V") :vj)
	  ((equalp filter "W-J-RC") :rc)
	  ((equalp filter "W-J-IC") :ic)
	  ;;
	  (t NIL))))



;; generic version OK
;(defmethod get-exptime-for-instrument ((inst %subaru-focas) fits-file) ... )

(defmethod get-object-for-instrument ((inst %subaru-focas) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst %subaru-focas) fits-file)
  (let ((obj-type (%gethead-or-error fits-file  "DATA-TYP")))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "SKYFLAT") :FLAT) ;; don't allow dome flats for now
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))


(defmethod get-mjd-start-for-instrument ((inst %subaru-focas) fits-file)
  (%gethead-or-error fits-file "MJD-STR"))

;; generic version OK
;(defmethod get-mjd-mid-for-instrument ((inst XXX) fits-file) ...  )
;(defmethod get-gain-for-instrument ((inst %subaru-focas) fits-file)   )
;(defmethod get-chip-id-for-instrument ((inst subaru-focas-one-chip) fits-file) ..)


(defmethod  get-pixel-scale-for-instrument
    ((inst %subaru-focas) fits-file
     &key extension)
  (err-if-not-image-at-extension inst fits-file "get-pixel-scale" extension)
  (let ((wcs
	  (or 
	   (get-initial-wcs-for-instrument inst fits-file
					   :extension extension)
	      (error "Cannot find WCS for instrument."))))
    (wcs:get-pixel-scale-for-wcs wcs)))






