

(in-package instrument-id)

;; parent class of megacam
(defclass %hst-wfc3 (imaging-instrument)
  ((name :initform "Hst-Wfc3")
   (aperture :initform 2.01)
   (observatory :initform "HST"))) ;; this is a bogus observatory because
                                   ;; it is in orbit


;; the drizzled images with one image extension
(defclass/inst %hst-wfc3-drz (%hst-wfc3 onechip)
  ((name :initform "Hst-Wfc3 Drizzled"))) 

;; UVIS and IR sides
(defclass/inst hst-wfc3-drz-ir (%hst-wfc3-drz)
  ())
(defclass/inst hst-wfc3-drz-uvis (%hst-wfc3-drz)
  ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod get-image-extension-for-onechip-instrument
    ((inst  %hst-wfc3-drz) fits-file)
  (declare (ignore inst fits-file))
  2)  ;; 2ND ext has the image

(defun %hst-wfc3-identify-instrument (fits-file)
  (when (equalp (cf:read-fits-header fits-file "INSTRUME") "WFC3")
    (when (equalp (cf:read-fits-header fits-file "DRIZCORR") "COMPLETE")
      (let ((detector  (cf:read-fits-header fits-file "DETECTOR")))
	(cond ((equalp detector "IR")
	       (make-instance 'hst-wfc3-drz-ir))
	      ((equalp detector "UVIS")
		(make-instance 'hst-wfc3-drz-uvis)))))))
	      

(%add-instrument-id-function '%hst-wfc3-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst %hst-wfc3) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("INSTRUME" "DRIZCORR" "DETECTOR" "TARGNAME" "OBSTYPE"
      "EXPSTART" "EXPEND")
    (call-next-method))
   :test 'equalp))


(defmethod get-standard-filter-for-instrument ((inst %hst-wfc3) fits-file)
  (let ((filter (%gethead-or-error fits-file "FILTER")))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not found in image ~A" fits-file))
    (intern (string-upcase filter) :keyword)))

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst %hst-wfc3) fits-file) ... )

(defmethod get-object-for-instrument ((inst %hst-wfc3) fits-file)
  (%gethead-or-error fits-file "TARGNAME"))

(defmethod get-object-type-for-instrument ((inst %hst-wfc3) fits-file)
  (let ((obj-type (%gethead-or-error fits-file  "OBSTYPE")))
    (cond ((equalp obj-type "BIAS") :BIAS) ;; may not exist
	  ((equalp obj-type "FLAT") :FLAT) ;; may not exist
	  ((equalp obj-type "IMAGING") :OBJECT)
	  (T :OTHER))))

(defmethod get-mjd-start-for-instrument ((inst %hst-wfc3) fits-file)
  (%gethead-or-error fits-file "EXPSTART"))


(defmethod get-mjd-mid-for-instrument ((inst %hst-wfc3) fits-file)
  (* 0.5d0 (+ (%gethead-or-error fits-file "EXPSTART")
	      (%gethead-or-error fits-file "EXPEND"))))
     
;; drizzle files apparently have units of e-/sec so that multiplying
;; by exptime gives e-,   so that exptime is effective gain
(defmethod get-gain-for-instrument ((inst %hst-wfc3-drz) fits-file
				    &key extension)
  (declare (ignore extension))
  (cf:read-fits-header fits-file "EXPTIME"))




