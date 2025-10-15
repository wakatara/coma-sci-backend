#|

http://sci.esa.int/sci-fmi/36520-optical-ground-station/

1 meter, almost 1 degree camera on Tenerife, with only an IR-cut filter

|#

(in-package instrument-id)

;; parent class of HFOSC camera
(defclass/inst esa-ogs-esadc2 (imaging-instrument onechip)
  ((name :initform "ESA-OGS-ESADC2")
   (aperture :initform 1.0)
   (observatory :initform "J04"))) ;; MPC: ESA Optical Ground Station, Tenerife


  

(defun %esa-ogs-esadc2-identify-instrument (fits-file)
  (when (and (equalp (cf:read-fits-header fits-file "INSTRUME") "ESASDC2")
	     (equalp (cf:read-fits-header fits-file "TELESCOP") "OGS"))
    (make-instance 'esa-ogs-esadc2)))

(%add-instrument-id-function '%esa-ogs-esadc2-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst esa-ogs-esadc2) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("INSTRUME" "TELESCOP" "EPOMJD" "XBINNING" "YBINNING" "RA" "DEC"
      "SCALE")
    (call-next-method))
   :test 'equalp))

(defmethod get-standard-filter-for-instrument ((inst esa-ogs-esadc2) fits-file)
  (let ((filter (%gethead-or-error fits-file "FILTER")))
    (cond ((equalp filter "IRBlock") :wide)
	  (t nil))))
	   
(defmethod is-reduced-for-instrument ((inst esa-ogs-esadc2) fits-file)
  (declare (ignorable  inst))
  (let ((imtype (get-object-type-for-instrument inst fits-file))
	(reductio (cf:read-fits-header fits-file "REDUCTIO")))
    (when (or (and (eq imtype :flat) (equalp reductio "biasred"))
	      (and (eq imtype :object) (equalp reductio "flatred")))
      :ogs-reduction-pipeline)))


;; generic version OK
;(defmethod get-exptime-for-instrument ((inst esa-ogs-esadc2) fits-file) ... )

(defmethod get-object-for-instrument ((inst esa-ogs-esadc2) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst esa-ogs-esadc2) fits-file)
  (let ((obstype (ignore-errors ;; can be missing
		  (cf:read-fits-header fits-file "OBSTYPE"))))
    (cond ((equalp obstype "bias") :BIAS)
	  ((equalp obstype "flat") :FLAT)
	  ((equalp obstype "dark") :DARK)
	  ((equalp obstype "obs") :OBJECT)
	  (t :unknown))))

(defmethod get-mjd-mid-for-instrument ((inst esa-ogs-esadc2) fits-file)
  (%gethead-or-error fits-file "EPOMJD"))

(defmethod get-mjd-start-for-instrument ((inst esa-ogs-esadc2) fits-file)
  (-  (%gethead-or-error fits-file "EPOMJD")
      (/ (* 0.5d0 (%gethead-or-error fits-file "EXPTIME"))
	 #.(* 24 3600))))
  


(defmethod get-gain-for-instrument ((inst esa-ogs-esadc2) fits-file &key extension)
  (declare (ignore extension))
  (%gethead-or-error fits-file "GAIN"))

(defmethod get-pixel-scale-for-instrument ((inst esa-ogs-esadc2) fits-file &key extension)
  (declare (ignore extension))
  (let ((xbinning (%gethead-or-error fits-file "XBINNING"))
	(ybinning (%gethead-or-error fits-file "YBINNING"))
	(scale (%gethead-or-error fits-file "SCALE"))) ;; without binning
    (when (not (= xbinning ybinning))
      (error "Image has unequal XBINNING=~A, YBINNING=~A" xbinning ybinning))
    (* scale xbinning)))
    



(defmethod get-initial-wcs-for-instrument
    ((inst esa-ogs-esadc2) fits-file &key extension)
  (declare (ignore extension))
  (let* 
      ((ra  (%gethead-or-error fits-file "RA"))
       (dec (%gethead-or-error fits-file "DEC"))
       (pixel-scale (/ (get-pixel-scale-for-instrument inst fits-file) 3600d0))
       (equinox (%gethead-or-error fits-file "EQUINOX"))
       (naxis1 (%gethead-or-error fits-file "NAXIS1"))
       (naxis2 (%gethead-or-error fits-file "NAXIS2")))

    (when (not (= equinox 2000d0))
      (multiple-value-setq (ra dec)
	(precess-ra-dec-to-j2000 equinox ra dec)))
       
    (wcs:make-wcs-radec-tan 
     :crval1 ra :crval2 dec
     :crpix1 (* naxis1 0.5d0)
     :crpix2 (* naxis2 0.5d0)
     :cd1_1 (- pixel-scale)
     :cd2_2 (- pixel-scale)
     :equinox 2000d0)))


(defmethod insert-initial-wcs-for-instrument ((inst esa-ogs-esadc2) fits-file
					      &key extension)
  (declare (ignore extension))
  (cf:write-wcs (get-initial-wcs-for-instrument inst fits-file) fits-file))



