
 
(in-package instrument-id)

;; Magellan site2k-1
(defclass/inst direct-site2k-1 (imaging-instrument onechip)
    ((name :initform "DIRECT-SITE2K-1") ;; a string
     (observatory :initform "ca")
     (aperture :initform 8.4)
     (gain-keyword :initarg :exptime-keyword :initform "EGAIN"
		   :accessor instrument-gain-keyword)))


  
 
(defun %direct-site2k-1-identify-instrument (fits-file)
  (when (and (equalp (cf:read-fits-header fits-file "INSTRUME") "Direct/SITe2k-1")
	     (equalp (cf:read-fits-header fits-file "TELESCOP") "CA 1.23M"))
    (make-instance 'direct-site2k-1)))

(%add-instrument-id-function '%direct-site2k-1-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst direct-site2k-1)
						fits-file)
  (declare (ignore inst fits-file))
  (append
   '("EXPTYPE" "SCALE" "RA-D" "DEC-D")
   (call-next-method)))

(defmethod get-standard-filter-for-instrument ((inst direct-site2k-1) fits-file)
  (let ((filter (%gethead-or-error fits-file "FILTER")))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not found in image ~A" fits-file))
    ;;
      (cond ((equalp filter "U") :uj)
	    ((equalp filter "B") :bj)
	    ((equalp filter "V") :vj)
	    ((equalp filter "R") :rc)
	    ((equalp filter "I") :ic)
	    ;;
	    (t NIL))))

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst direct-site2k-1) fits-file) ... )

(defmethod get-object-for-instrument ((inst direct-site2k-1) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst direct-site2k-1) fits-file)
  (let ((obj-type (%gethead-or-error  fits-file  "EXPTYPE")))
    (cond ((equalp obj-type "Bias") :BIAS)
	  ((equalp obj-type "Flat") :FLAT)
	  ((equalp obj-type "Object") :OBJECT)
	  (T :OTHER))))

(defmethod get-mjd-start-for-instrument ((inst direct-site2k-1) fits-file)
  (let* ((date-string  (%gethead-or-error fits-file "UT-DATE"))
	 (time-string  (%gethead-or-error fits-file "UT-TIME"))
	 (date-time-string (concatenate 'string date-string "T" time-string))
	 (mjd-start
	   (multiple-value-bind (year month day hour min sec)
	       (astro-time:parse-ut-date-and-time-string date-time-string)
	     (astro-time:calendar-date-to-mjd year month day hour min sec))))
     mjd-start))



;; generic version OK
;;(defmethod get-mjd-mid-for-instrument ((inst direct-site2k-1) fits-file) ... )

;; generic version OK, but gain-keyword is now EGAIN
;(defmethod get-gain-for-instrument ((inst direct-site2k-1) fits-file)   ...)
 
;(defmethod get-chip-id-for-instrument ((inst direct-site2k-1) fits-file) ..)


(defmethod get-initial-wcs-for-instrument ((inst direct-site2k-1) fits-file
					   &key extension)
  (declare (ignore extension))
  (let* ((pix-scale (/ (%gethead-or-error fits-file "SCALE") 3600d0))
	 (ra  (%gethead-or-error fits-file "RA-D"))
	 (dec (%gethead-or-error fits-file "DEC-D"))
	 (equinox (%gethead-or-error fits-file "EQUINOX"))
	 (pcenter  (* 0.5d0 (%gethead-or-error fits-file "NAXIS1"))) ;; for binning
	 (wcs 
	   (wcs:make-wcs-radec-tan 
	    :crval1 ra
	    :crval2 dec
	    :crpix1 pcenter
	    :crpix2 pcenter
	    :cd1_1 0d0
	    :cd2_2 0d0
	    :cd1_2 (- pix-scale) 
	    :cd2_1 (- pix-scale)
	    :equinox equinox)))
    wcs))
	
    
(defmethod insert-initial-wcs-for-instrument ((inst direct-site2k-1) fits-file
					      &key extension)
  (declare (ignore extension))
  (cf:write-wcs (get-initial-wcs-for-instrument inst fits-file) fits-file))



