#|

LBT-LBC

Large Binocular Telescope, Large Binocular Cameras 

|#




(in-package instrument-id)


(defclass %lbt-lbc (imaging-instrument)
  ((name :initform "LBT LBC 4-chip camera") ;; a string
   (aperture :initform 8.4) ;; each of two mirrors
   (observatory :initform "KPNO")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass     %lbt-lbc-red-mixin () ()) ;; mixin to denote red side
(defclass     %lbt-lbc-blue-mixin () ()) ;; mixin to denote blue side
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/inst %lbt-lbc-onechip (%lbt-lbc onechip)
  ())

(defclass/inst lbt-lbc-onechip-red (%lbt-lbc-red-mixin
				     %lbt-lbc-onechip)
  ())

(defclass/inst lbt-lbc-onechip-blue (%lbt-lbc-blue-mixin
				     %lbt-lbc-onechip)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/inst %lbt-lbc-array (%lbt-lbc multichip)
  ())

(defclass/inst lbt-lbc-array-red (%lbt-lbc-red-mixin
				  %lbt-lbc-array)
  ())

(defclass/inst lbt-lbc-array-blue (%lbt-lbc-blue-mixin
				   %lbt-lbc-array)
  ())


(defclass/inst lbt-lbc-unknown (%lbt-lbc) ())

;; FIXME - maybe should split into LBT-DX and LBT-SX sub-instruments
;;   for the two adaptive optics secondaries


(defun %lbt-lbc-identify-instrument (fits-file)

  (when (member (cf:read-fits-header fits-file "TELESCOP")
		'("LBT-DX" "LBT-SX") ;; the 2 adaptive optics systems
		:test 'equalp) 
    ;; we can't trust NEXTEND in some one-chip files, so we use N-HDUS
    (let* ((n-hdus
	    (cond ((cf:fits-file-p fits-file)
		   (cf:fits-file-num-hdus fits-file))
		  (t
		   (cf:with-open-fits-file (fits-file ff)
		     (cf:fits-file-num-hdus ff)))))
	   (instrume (cf:read-fits-header fits-file "INSTRUME")))
    
      (cond ((member instrume
		     '("LBC_RED" "LBC-RED") :test 'equalp)
	     (cond ((= n-hdus 1)
		    (make-instance 'lbt-lbc-onechip-red))
		   ((= n-hdus 4)
		    (make-instance 'lbt-lbc-array-red))
		   (t
		    (make-instance 'lbt-lbc-unknown))))
	    ;;
	    ((member instrume
		     '("LBC_BLUE" "LBC-BLUE") :test 'equalp)
	     (cond ((= n-hdus 1)
		    (make-instance 'lbt-lbc-onechip-blue))
		   ((= n-hdus 4)
		    (make-instance 'lbt-lbc-array-blue))
		   (t
		    (make-instance 'lbt-lbc-unknown))))))))


(%add-instrument-id-function '%lbt-lbc-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst %lbt-lbc) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("TELESCOP" "INSTRUME" "IMAGETYP" "MJD_OBS")
    (call-next-method))
   :test 'equalp))
 
(defmethod get-standard-filter-for-instrument ((inst %lbt-lbc) fits-file)
  (let ((filter (%gethead-or-error fits-file "FILTER")))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not found in image ~A" fits-file))
    (cond ((eql 0 (search "U-BESSEL" filter)) :uj)
	  ((eql 0 (search "B-BESSEL" filter)) :bj)
	  ((eql 0 (search "V-BESSEL" filter)) :vj)
	  ((eql 0 (search "R-BESSEL" filter)) :rc)
	  ((eql 0 (search "I-BESSEL" filter)) :ic)
	  ;;
	  ((eql 0 (search "g-SLOAN" filter))   :gsdss)
	  ((eql 0 (search "r-SLOAN" filter))   :rsdss)
	  ((eql 0 (search "i-SLOAN" filter))   :isdss)
	  ((eql 0 (search "z-SLOAN" filter))   :zsdss)
	  ;;
	  ((eql 0 (search "Y" filter))   :y-lbt-lbc)

	  ;; fixme - there are more filters
	  (t NIL))))

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst %lbt-lbc) fits-file) ... )

(defmethod get-object-for-instrument ((inst %lbt-lbc) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst %lbt-lbc) fits-file)
  (let ((obj-type (%gethead-or-error fits-file  "IMAGETYP")))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "FLAT") :FLAT)
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))

(defmethod get-mjd-start-for-instrument ((inst %lbt-lbc) fits-file)
  (%gethead-or-error fits-file "MJD_OBS"))

;; generic version OK
;(defmethod get-mjd-mid-for-instrument ((inst XXX) fits-file) ...  )
;(defmethod get-gain-for-instrument ((inst %lbt-lbc) fits-file)   )
;(defmethod get-chip-id-for-instrument ((inst lbt-lbc-one-chip) fits-file) ..)


(defmethod  get-pixel-scale-for-instrument
    ((inst %lbt-lbc) fits-file
     &key extension)
  (err-if-not-image-at-extension inst fits-file "get-pixel-scale" extension)
  (let ((wcs
	  (or 
	   (get-initial-wcs-for-instrument inst fits-file
					   :extension extension)
	      (error "Cannot find WCS for instrument."))))
    (wcs:get-pixel-scale-for-wcs wcs)))






