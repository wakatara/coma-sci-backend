#|

This is a catch-all for cameras that we might want to identify but
don't expect to handle, like IR cameras

The methods should all return NIL

|#

(in-package instrument-id)

(defclass %unhandled-imaging-instrument (imaging-instrument)
  ((name :initform "Unhandled Instrument")))

(defclass plate-scan () ((detector-type :initform :photographic-plate)))


(defclass/inst cfht-aobir-ir-camera (%unhandled-imaging-instrument)
  ((name :initform "CFHT AOBIR IR camera")
   (observatory :initform "MKO")))

(defclass/inst eso-vlt-isaac-IR-camera (%unhandled-imaging-instrument)
  ((name :initform "VLT Isaac IR Camera")
   (observatory :initform "Paranal")))

(defclass/inst ukirt-cgs4 (%unhandled-imaging-instrument)
  ((name :initform "UKIRT CGS4 IR camera")
   (observatory :initform "MKO")))


(defclass/inst ukirt-unkown-IR-camera (%unhandled-imaging-instrument)
  ((name :initform "UKIRT unknown IR camera")
   (observatory :initform "MKO")))

(defclass/inst uk-schmidt-plate-scan (plate-scan %unhandled-imaging-instrument)
  ((name :initform "UK Schmidt plate scan")
   (observatory :initform "AAO")))

(defclass/inst palomar-schmidt-plate-scan (plate-scan %unhandled-imaging-instrument)
  ((name :initform "Palomar Schmidt  plate scan")
   (observatory :initform "AAO")))

(defclass/inst ctio-plate-scan-calibration-spot  (plate-scan %unhandled-imaging-instrument)
  ((name :initform "Calibration spot for plate scan")
   (observatory :initform "ctio")))

#+nil
(defclass/inst needs-help-to-figure-out (%unhandled-imaging-instrument)
  ((name :initform "Instrument that needs help to figure out")
   (observatory :initform nil)))

(defmethod get-initial-wcs-for-instrument
    ((inst %unhandled-imaging-instrument) fits &key extension)
  (declare (ignore inst fits extension))
  nil)

(defmethod get-gain-for-instrument
    ((inst %unhandled-imaging-instrument) fits &key extension)
  (declare (ignore inst fits extension))
  nil)

(defmethod get-object-for-instrument ((inst %unhandled-imaging-instrument) fits)
  (declare (ignore inst fits))
  nil)

(defmethod get-object-type-for-instrument ((inst %unhandled-imaging-instrument) fits)
  (declare (ignore inst fits))
  nil)

(defmethod get-standard-filter-for-instrument ((inst %unhandled-imaging-instrument) fits)
  (declare (ignore inst fits))
  nil)

(defmethod get-trimsec-for-instrument
    ((inst %unhandled-imaging-instrument) fits &key extension)
  (declare (ignore inst fits extension))
  nil)

(defmethod get-exptime-for-instrument ((inst %unhandled-imaging-instrument) fits)
  (declare (ignore inst fits))
  nil)

(defmethod get-mjd-start-for-instrument ((inst %unhandled-imaging-instrument) fits)
  (declare (ignore inst fits))
  nil)

(defmethod get-mjd-mid-for-instrument ((inst %unhandled-imaging-instrument) fits)
  (declare (ignore inst fits))
  nil)

(defmethod get-pixel-scale-for-instrument
    ((inst %unhandled-imaging-instrument) fits &key extension)
  (declare (ignore inst fits extension))
  nil)




;; this has minimal headers
(defclass/inst unknown-plate-scan
  (plate-scan %unhandled-imaging-instrument)
  ((name :initform "Unknown Plate Scan")
   (observatory :initform nil)))

(defun %unhandled-identify-instrument (fits-file)
  (let ((observat (cf:read-fits-header fits-file "OBSERVAT"))
	(instrume (cf:read-fits-header fits-file "INSTRUME"))
	(telescop (cf:read-fits-header fits-file "TELESCOP"))
	(object (cf:read-fits-header fits-file "OBJECT"))
	(naxis1 (cf:read-fits-header fits-file "NAXIS1"))
	(naxis2 (cf:read-fits-header fits-file "NAXIS2"))
	(origin (cf:read-fits-header fits-file "ORIGIN"))
	;; ...
	)

    (cond ((and (equalp observat "CFHT 3.6m")
		(equalp instrume "aobir"))
	   (make-instance 'cfht-aobir-ir-camera))
	  ;;
	  ((and (equalp telescop "UKIRT, Mauna Kea, HI")
		(equalp instrume "CGS4"))
	   (make-instance 'ukirt-cgs4))
	  ;;
	  ((or (and (not observat) (not instrume) (not telescop)
		    (let ((object (cf:read-fits-header fits-file "OBJECT")))
		      (search "PLATE" object :test 'equalp)
		      (search "SCAN" object :test 'equalp)))
	       ;; other tests can go here
	       )
	   (make-instance 'unknown-plate-scan))
	  ;;
	  ((and (or (search "UK Schmidt" telescop :test 'equalp)
		    (search "UK 48-inch Schmidt" telescop :test 'equalp))
		(cf:read-fits-header fits-file "PLATEID"))
	   (make-instance 'uk-schmidt-plate-scan))
	  ((and (or (search "Palomar Schmidt" telescop :test 'equalp)
		    (search "Palomar 48-inch Schmidt" telescop :test 'equalp))	 
		(cf:read-fits-header fits-file "PLATEID"))
	   (make-instance 'palomar-schmidt-plate-scan))
	  ;;
	  ((and (equalp instrume "ISAAC")
		(search "VLT" telescop))
	   (make-instance 'eso-vlt-isaac-IR-camera))
	  ;;
	  ((and (equalp (cf:read-fits-header fits-file "LABEL")
			"ALICE image")
		(equalp (cf:read-fits-header fits-file "ORIGIN")
			"Starlink Project, U.K."))
	   ;; it looks like CGS4 is the best guess because
	   ;; only IRCAM and CGS4 are supported by ALICE system
	   ;; but we can't really prove it isn't imager IRCAM
	   ;; except that the data available look like spectra
	   (make-instance ;;'ukirt-unkown-IR-camera
	                    'ukirt-cgs4))
	  ;;
	  ((and (search "CTIO" object)
		(search "SPOTS" object)
		(not observat) (not instrume)
		(equalp origin "KPNO-IRAF")
		(numberp naxis1) (< naxis1 50)
		(numberp naxis2) (< naxis2 50))
	   (make-instance 'ctio-plate-scan-calibration-spot))
	       
	  
	  (t
	   nil))))

(%add-instrument-id-function '%unhandled-identify-instrument)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-standard-filter-for-instrument
    ((inst %unhandled-imaging-instrument) fits-file)
  (declare (ignore inst fits-file))
  nil)

;; exposures seem to be negative?!
(defmethod get-exptime-for-instrument ((inst %unhandled-imaging-instrument)
				       fits-file)
  (declare (ignore inst fits-file))
  nil)

  

(defmethod get-object-for-instrument ((inst %unhandled-imaging-instrument)
				      fits-file)
  (declare (ignore inst fits-file))
  nil)


(defmethod get-object-type-for-instrument ((inst %unhandled-imaging-instrument)
					   fits-file)
  (declare (ignore inst fits-file))
  nil)


(defmethod get-mjd-start-for-instrument ((inst %unhandled-imaging-instrument)
					 fits-file)
  (declare (ignore inst fits-file))
  nil)

(defmethod get-mjd-mid-for-instrument ((inst %unhandled-imaging-instrument)
				       fits-file)
  (declare (ignore inst fits-file))
  nil)

(defmethod get-gain-for-instrument ((inst %unhandled-imaging-instrument)
				       fits-file
				       &key extension)
  (declare (ignore inst fits-file extension))
  nil)
  



(defmethod get-pixel-scale-for-instrument
    ((inst %unhandled-imaging-instrument) fits-file &key extension)
  (declare (ignore inst fits-file extension))
  nil)
 

;; it looks like the whole raw array is data
(defmethod get-datasec-for-instrument ((inst %unhandled-imaging-instrument)
				       fits-file
				       &key extension)
  (declare (ignore inst fits-file extension))
  nil)




(defmethod get-initial-wcs-for-instrument ((inst %unhandled-imaging-instrument)
					   fits-file
					   &key extension)
  (declare (ignore inst fits-file extension))
  nil)
 
		    
(defmethod insert-initial-wcs-for-instrument
    ((inst %unhandled-imaging-instrument ) fits-file &key extension)
  (declare (ignore inst fits-file extension))
  nil)
					      
      






		

