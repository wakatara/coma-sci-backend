

(in-package instrument-id)


(defclass/inst calar-alto-dlr-mkiii (imaging-instrument onechip)
  ((name :initform "CALAR-ALTO-DLR-MKIII") ;; a string
   (aperture :initform 1.2)
   (observatory :initform "ca" :accessor instrument-observatory)))


  

(defun %calar-alto-dlr-mkiii-identify-instrument (fits-file)
  (when (and (or (equalp (cf:read-fits-header fits-file "INSTRUM") "DLR-MKIII") ;; yes, both
		 (equalp (cf:read-fits-header fits-file "INSTRUME") "DLR-MKIII"))
	     (equalp (cf:read-fits-header fits-file "TELESCOP") "CA 1.23M"))
    (make-instance 'calar-alto-dlr-mkiii)))

(%add-instrument-id-function '%calar-alto-dlr-mkiii-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst  calar-alto-dlr-mkiii)
						fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("INSTRUM" "INSTRUME" "INSFLNAM" "TELESOP"
      "IMAGETYP" "MJD-OBS" "RAJ2000" "DECJ2000")
    (call-next-method)) 
   :test 'equalp))

(defmethod get-standard-filter-for-instrument ((inst calar-alto-dlr-mkiii) fits-file)
  (let ((filter (%gethead-or-error fits-file "INSFLNAM")))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not found in image ~A" fits-file))
    (flet ((filter-ends-with (letter)
	     (char= (aref filter (1- (length filter))) letter)))
      ;; sometimes "Johnson_U" and sometimes "John_U" so try last char
      (cond ((filter-ends-with #\U) :uj)
	    ((filter-ends-with #\B) :bj)
	    ((filter-ends-with #\V) :vj)
	    ((filter-ends-with #\R) :rc)
	    ((filter-ends-with #\I) :ic)
	    ((equalp filter "free")      :none)
	    ;;
	    (t NIL)))))

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst calar-alto-dlr-mkiii) fits-file) ... )

(defmethod get-object-for-instrument ((inst calar-alto-dlr-mkiii) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst calar-alto-dlr-mkiii) fits-file)
  (let ((obj-type (%gethead-or-error  fits-file  "IMAGETYP")))
    (cond ((equalp obj-type "bias") :BIAS)
	  ((equalp obj-type "flat") :FLAT)
	  ((equalp obj-type "dark") :DARK)
	  ((equalp obj-type "science") :OBJECT)
	  (T :OTHER))))

(defmethod get-mjd-start-for-instrument ((inst calar-alto-dlr-mkiii) fits-file)
  (%gethead-or-error fits-file "MJD-OBS"))



;; generic version OK
;(defmethod get-mjd-mid-for-instrument ((inst calar-alto-dlr-mkiii) fits-file) ...  )

;(defmethod get-gain-for-instrument ((inst calar-alto-dlr-mkiii) fits-file)   ...)
 
;(defmethod get-chip-id-for-instrument ((inst calar-alto-dlr-mkiii) fits-file) ..)

;; UH88 starts with a WCS 
(defmethod get-initial-wcs-for-instrument ((inst calar-alto-dlr-mkiii) fits-file
					   &key extension)
  (declare (ignore extension))
  (let* ((pix-scale 1.739d-4)
	 (ra2000  (cf:read-fits-header fits-file "RAJ2000"))
	 (dec2000 (cf:read-fits-header fits-file "DECJ2000"))
	 ;; sometimes RA,DEC,EQUINOX present instead of RAJ2000,DECJ2000
	 (have-j2000 (and ra2000 dec2000))
	 (ra (when (not have-j2000) (cf:read-fits-header fits-file "RA")))
	 (dec (when (not have-j2000) (cf:read-fits-header fits-file "DEC")))
	 (equinox (when (not have-j2000)
		   (cf:read-fits-header fits-file "EQUINOX")))
	 (pcenter1  (* 0.5d0 (%gethead-or-error fits-file "NAXIS1")))
	 (pcenter2  (* 0.5d0 (%gethead-or-error fits-file "NAXIS2")))) 

    ;; convert ra,dec to ra2000,dec200 if the latter not in headers
    (when (not have-j2000)
      (multiple-value-setq (ra2000 dec2000)
	(precess-ra-dec-to-j2000 equinox ra dec)))
    
    (wcs:make-wcs-radec-tan 
     :crval1 ra2000
     :crval2 dec2000
     :crpix1 pcenter1
     :crpix2 pcenter2
     :cd1_1 (- pix-scale) 
     :cd2_2 (- pix-scale)
     :cd1_2 0d0
     :cd2_1 0d0
     :equinox 2000d0)))
	
    
(defmethod insert-initial-wcs-for-instrument ((inst calar-alto-dlr-mkiii) fits-file
					      &key extension)
  (declare (ignore extension))
  (cf:write-wcs (get-initial-wcs-for-instrument inst fits-file) fits-file))



