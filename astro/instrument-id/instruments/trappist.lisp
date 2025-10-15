

(in-package instrument-id)

;; parent class of HFOSC camera
(defclass/inst %trappist (imaging-instrument onechip)
  ((name :initform "TRAPPIST Generic")
   (aperture :initform 0.60)))

(defclass/inst %trappist-south (%trappist)
  ((observatory  :initform "I40"))) ;; La Silla

(defclass/inst %trappist-north (%trappist)
  ((observatory  :initform "Z53"))) ;; Oukaïmden Observatory

(defclass %trappist-1x1-mixin () ())
(defclass %trappist-2x2-mixin () ())

(defclass/inst trappist-south-1x1 (%trappist-south %trappist-1x1-mixin)
  ((name :initform "TRAPPIST-SOUTH-1x1")))

(defclass/inst trappist-south-2x2 (%trappist-south %trappist-2x2-mixin)
  ((name :initform "TRAPPIST-SOUTH-2x2")))

(defclass/inst trappist-north-1x1 (%trappist-north %trappist-1x1-mixin)
  ((name :initform "TRAPPIST-NORTH-1x1")))

(defclass/inst trappist-north-2x2 (%trappist-north %trappist-2x2-mixin)
  ((name :initform "TRAPPIST-NORTH-2x2")))
 

   
;; FIXME - we can't identify TRAPPIST-NORTH yet
(defun %trappist-identify-instrument (fits-file)
  (when (and (equalp (cf:read-fits-header fits-file "INSTRUME") "FLI-New")
	     (equalp (cf:read-fits-header fits-file "TELESCOP") "TRAPPIST"))
    ;; sometimes trappist images are binned 2x2 (like some calibs)
    (cond ((and (eql (cf:read-fits-header fits-file "XBINNING") 1)
		(eql (cf:read-fits-header fits-file "YBINNING") 1))
	   (make-instance 'trappist-south-1x1))
	  ((and (eql (cf:read-fits-header fits-file "XBINNING") 2)
		(eql (cf:read-fits-header fits-file "YBINNING") 2))
	   (make-instance 'trappist-south-2x2)))))



(%add-instrument-id-function '%trappist-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst %trappist) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("INSTRUME" "TELESCOP" "XBINNING" "YBINNING" "IMAGETYP" "JD")      
    (call-next-method))
   :test 'equalp))

(defmethod get-standard-filter-for-instrument ((inst %trappist) fits-file)
  (let ((filter (%gethead-or-error fits-file "FILTER")))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not found in image ~A" fits-file))
    (cond ((equalp filter "U") :uj)
	  ((equalp filter "B") :bj)
	  ((equalp filter "V") :vj)
	  ((equalp filter "R") :rc)
	  ((equalp filter "I") :ic)
	  ((equalp filter "Z") :zsdss)
	  ;;;
	  (t NIL))))

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst %trappist) fits-file) ... )

(defmethod get-object-for-instrument ((inst %trappist) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst %trappist) fits-file)
  (let ((obj-type (%gethead-or-error  fits-file  "IMAGETYP")))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "FLAT") :FLAT)
	  ((equalp obj-type "LIGHT") :OBJECT) ;; this one is unusual
	  ((equalp obj-type "DARK") :DARK)
	  (T :OTHER))))

(defmethod get-mjd-start-for-instrument ((inst %trappist) fits-file)
  (let* ((jd (%gethead-or-error fits-file "JD")))
    (astro-time:jd-to-mjd jd)))



;; generic version OK
;(defmethod get-mjd-mid-for-instrument ((inst %trappist) fits-file) ...  )

;; gain from http://www.ati.ulg.ac.be/TRAPPIST/Trappist_main/Equipment.html
(defmethod get-gain-for-instrument ((inst %trappist) fits-file
				    &key extension)
  (declare (ignore extension))
  (or (cf:read-fits-header fits-file "STDHEAD.GAIN" :extension 1)
      1.1))

;; not in headers; judging by black vertical strips on sides of img
(defmethod get-datasec-for-instrument ((inst %trappist-1x1-mixin)
				       fits-file &key extension)
  (declare (ignore extension))
  (vector 17 2064 20 2080)) 

;; the bottom y-boundary of 10 is determined from the unbinned img because
;; it was not visible in darks and biases
(defmethod get-datasec-for-instrument ((inst %trappist-2x2-mixin)
				       fits-file &key extension)
  (declare (ignore extension))
  (vector 9 1032 10 1040)) 


;(defmethod get-chip-id-for-instrument ((inst %trappist) fits-file) ..)

;; eithe WCS is fit already or we just don't know 
;(defmethod get-initial-wcs-for-instrument ((inst %trappist) fits-file)  ...)
;(defmethod insert-initial-wcs-for-instrument ((inst %trappist) fits-file) ...)



