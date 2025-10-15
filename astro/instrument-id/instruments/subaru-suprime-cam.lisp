

(in-package instrument-id)

;; parent class of megacam
(defclass %subaru-suprime-cam (imaging-instrument)
  ((name :initform "Subaru Suprime-Cam") ;; a string
   (aperture :initform 8.2)
   (observatory :initform "T09")))

(defclass/inst subaru-suprime-cam-one-chip (%subaru-suprime-cam onechip)
  ())

(defclass/inst subaru-suprime-cam-array (%subaru-suprime-cam multichip)
  ())

  

(defun %suprime-cam-identify-instrument (fits-file)
  (when (equalp (cf:read-fits-header fits-file "INSTRUME") "SuprimeCam")
    (Let ((naxis (%gethead-or-error fits-file "NAXIS")))
    (cond ((and (equalp naxis 0))
	   (make-instance 'subaru-suprime-cam-array))
	  ((equalp naxis 2)
	   (let* ((inst (make-instance 'subaru-suprime-cam-one-chip))
		  (chipid (get-chip-id-for-instrument inst fits-file)))
	     (setf (chip-id inst) chipid)
	     inst))
	  (t
	   (error "Unknown SuprimeCam fits file ~A" fits-file))))))

(%add-instrument-id-function '%suprime-cam-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst %subaru-suprime-cam) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("INSTRUME" "FILTER01" "DATA-TYP" "MJD-END" "MJD-STR" "DET-ID")
    (call-next-method))
   :test 'equalp))

(defmethod get-standard-filter-for-instrument ((inst %subaru-suprime-cam) fits-file)
  (let ((filter (%gethead-or-error fits-file "FILTER01")))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not found in image ~A" fits-file))
    (cond ((equalp filter "W-J-U") :uj)
	  ((equalp filter "W-J-B") :bj)
	  ((equalp filter "W-J-V") :vj)
	  ((equalp filter "W-J-RC") :rc)
	  ((equalp filter "W-J-IC") :ic)
	  ((equalp filter "W-S-G+") :gsdss)
	  ((equalp filter "W-S-R+") :rsdss)
	  ((equalp filter "W-S-I+") :isdss)
	  ((equalp filter "W-S-Z+") :zsdss)
	  ((equalp filter "W-J-VR") :vr) ;; wide filter
	  ;;
	  (t NIL))))

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst %subaru-suprime-cam) fits-file) ... )

(defmethod get-object-for-instrument ((inst %subaru-suprime-cam) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst %subaru-suprime-cam) fits-file)
  (let ((obj-type (%gethead-or-error fits-file  "DATA-TYP")))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "SKYFLAT") :FLAT) ;; don't allow dome flats for now
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))

(defmethod get-mjd-start-for-instrument ((inst %subaru-suprime-cam) fits-file)
  (%gethead-or-error fits-file "MJD"))


(defmethod get-mjd-mid-for-instrument ((inst %subaru-suprime-cam) fits-file) 
  (if (and (cf:read-fits-header fits-file "MJD-END")
	   (cf:read-fits-header fits-file "MJD-STR"))
    (* 0.5d0 (+  (%gethead-or-error fits-file "MJD-STR")
		 (%gethead-or-error fits-file "MJD-END")))
    (call-next-method))) ;; oops, we didn't write MJD-END to the zeroth header

;(defmethod get-gain-for-instrument ((inst %subaru-suprime-cam) fits-file)   )



(defmethod get-chip-id-for-instrument ((inst %subaru-suprime-cam) fits-file
				       &key extension)
  (%gethead-or-error fits-file "DET-ID" :extension extension))



