
;; template for instrument id of a new instrument.
;; replace all instances of XXX and fix methods appropriately


(in-package instrument-id)

(defclass/inst XXX (imaging-instrument onechip OR multichip)
    ((name :initform "XXX") ;; a string
     (observatory :initform "XXX")
     ;; optional
     (aperture  :initform XXX)  ;; default is NIL meaning unknown
     (detector-type    :initform :CCD) ;; :CCD is the default
     (saturation-level :initform 60000)
     (non-linear-level :initform 60000)
     (gain-keyword :initform "XXX GAIN KEYWORD"
		   :accessor instrument-gain-keyword)))


   

(defun %XXX-identify-instrument (fits-file)
  (when (equalp (cf:read-fits-header fits-file "INSTRUME")
		"XXX")
    (make-instance 'xxx)))

(%add-instrument-id-function '%xxx-identify-instrument)


(defmethod get-standard-filter-for-instrument ((inst XXX) fits-file) 
  (error "get-standard-filter-for-instrument not defined for ~A" inst))


(defmethod get-exptime-for-instrument ((inst XXX) fits-file)
  (call-next-method))

(defmethod get-object-for-instrument ((inst XXX) fits-file)
  (call-next-method))

(defmethod get-object-type-for-instrument ((inst XXX) fits-file)
  (call-next-method))

(defmethod get-mjd-start-for-instrument ((inst XXX) fits-file)
  (call-next-method))

(defmethod get-mjd-mid-for-instrument ((inst XXX) fits-file)
  (call-next-method))

(defmethod get-gain-for-instrument ((inst XXX) fits-file &key extension )
  (call-next-method))

(defmethod write-gain-for-instrument ((inst XXX) fits-file gain &key extension)
  (call-next-method))

(defmethod get-chip-id-for-instrument ((inst XXX) fits-file &key extension)
  (call-next-method))

(defmethod get-trimsec-for-instrument ((inst XXX) fits-file &key extension)
  (call-next-method))

(defmethod get-statsec-for-instrument ((inst XXX) fits-file &key extension)
  (call-next-method))

(defmethod get-initial-wcs-for-instrument ((inst XXX) fits-file &key extension)
  (call-next-method))

(defmethod insert-initial-wcs-for-instrument ((inst XXX) fits-file &key extension)
  (call-next-method))

(defmethod get-pixel-scale-for-instrument ((inst XXX) fits-file &key extension)
  (call-next-method))


;; only if onechip; delete otherwise
(defmethod get-image-extension-for-onechip-instrument  ((inst XXX) fits-file)
  (call-next-method))


(defmethod test-if-image-at-extension-for-instrument
    ((inst XXX) fits-file &key  extension)
  (call-next-method))
