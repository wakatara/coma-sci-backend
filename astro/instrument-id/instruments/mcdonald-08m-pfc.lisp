(in-package instrument-id)


#|
This instrument is nicely defined:

OBSERVAT= 'MCDONALD          '  /  observatory
TELESCOP= '0.8m              '  /  telescope name
INSTRUME= 'pfc               '  /  instrument
INSFILTE= 'R                 '  /  instrument filters

https://mcdonaldobservatory.org/research/instruments/prime-focus-corrector

The instrument is large field (0.75 deg, 1.3" pixels roughly) and tends
to have sub-regions read out, which will throw off WCS bigly
it the chip was read out away from center.

The large FOV means we might have to worry about field distortion.

|#



(defclass/inst mcdonald-0.8m-pfc (imaging-instrument onechip)
  ((name :initform "MCDONALD-0.8M-PFC") ;; a string
    (aperture :initform 0.8)
    (observatory :initform "MKO")))





(defun %mcdonald-0.8m-pfc-identify-instrument (fits-file)
  (when
      (and
       (equalp (cf:read-fits-header fits-file "OBSERVAT") "MCDONALD")
       (equalp (cf:read-fits-header fits-file "TELESCOP") "0.8M")
       (equalp (cf:read-fits-header fits-file "INSTRUME") "pfc"))
    (make-instance 'mcdonald-0.8m-pfc )))
       
	

(%add-instrument-id-function '%mcdonald-0.8m-pfc-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst mcdonald-0.8m-pfc)
						fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("OBSEVAT" "TELESCOP" "INSTRUME" "INSFILTE" "IMAGETYP"
      "DATE-OBS" "UT" "CCDSUM" "RA" "DEC")
    (call-next-method))
   :test 'equalp))

(defmethod get-standard-filter-for-instrument
    ((inst mcdonald-0.8m-pfc) fits-file)
  (let ((filter (%gethead-or-error fits-file "INSFILTE")))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not inserted into MCDONALD-0.8M-PFC image ~A"
	     fits-file))
    (cond ((equalp filter "U") :uj)
	  ((equalp filter "B") :bj)
	  ((equalp filter "V") :vj)
	  ((equalp filter "R") :rc)
	  ((equalp filter "I") :ic)
	  ;;;
	  (t NIL))))

;; exposures seem to be negative?!
(defmethod get-exptime-for-instrument ((inst mcdonald-0.8m-pfc) fits-file)
  (declare (ignore inst))
  (* 1d0 (%gethead-or-error fits-file "EXPTIME")))

  

(defmethod get-object-for-instrument ((inst mcdonald-0.8m-pfc) fits-file)
  (let ((obj (%gethead-or-error fits-file "OBJECT")))
    (if (not (equal obj "Unknown"))
	obj
	nil)))

(defmethod get-object-type-for-instrument ((inst mcdonald-0.8m-pfc) fits-file)
  (let ((imagetyp (%gethead-or-error  fits-file  "IMAGETYP")))
    (cond ((search imagetyp "BIAS" :test 'equalp) :BIAS)
	  ((search imagetyp "FLAT" :test 'equalp) :FLAT)
	  ((search imagetyp "OBJECT" :test 'equalp) :object)
	  (T :UNKNOWN))))


(defmethod get-mjd-start-for-instrument ((inst mcdonald-0.8m-pfc) fits-file)
  (astro-time:parse-ut-date-and-time-string-to-mjd
   (concatenate 'string
		(%gethead-or-error fits-file "DATE-OBS")
		"T"
		;; not clear what UT really is - hope it is UT obs!
		(%gethead-or-error fits-file "UT")))) 

(defmethod get-mjd-mid-for-instrument ((inst mcdonald-0.8m-pfc) fits-file)
  (* (get-mjd-start-for-instrument inst fits-file)
     (* 0.5d0 (/ (get-exptime-for-instrument inst fits-file) 24 3600d0))))

;; (defmethod get-gain-for-instrument .... ;; has normal GAIN keyword
  



(defmethod get-pixel-scale-for-instrument
    ((inst mcdonald-0.8m-pfc) fits-file &key extension)
  (declare (ignorable inst fits-file extension))
  (let*
      ((scale  1.35d0) ;; from fit
       (ccdsum (string-trim " " (%gethead-or-error fits-file "CCDSUM")))
       (binning
	 (cond
	   ((equalp ccdsum "1 1") 1)
	   ((equalp ccdsum "2 2") 2)
	   ((equalp ccdsum "4 4") 4) ;; may not exist
	   (t
	    (error
	     "Binning not 1,2 or 2,2, or 4,4 for mcdonald-0.8m-pfc pix scale")))))
    (* scale binning))) 

;; it looks like the whole raw array is data
;;(defmethod get-datasec-for-instrument ((inst mcdonald-0.8m-pfc)...




(defmethod get-initial-wcs-for-instrument ((inst mcdonald-0.8m-pfc) fits-file
					   &key extension)
  (declare (ignore extension))
  (let* ((rastr (%gethead-or-error fits-file "RA"))
	 (decstr (%gethead-or-error fits-file "Dec"))
	 (naxis1 (%gethead-or-error fits-file "NAXIS1"))
	 (naxis2 (%gethead-or-error fits-file "NAXIS2"))
	 (pixel-scale (/ (get-pixel-scale-for-instrument inst fits-file)
			 3600d0))
	 (ra (ra-dec:hms-string->deg rastr))
	 (dec (ra-dec:dms-string->deg decstr))
	 (epoch (%gethead-or-error fits-file "EPOCH")))

    	;; convert to 2000 if necessary
	(when (not (= epoch 2000d0))
	  (multiple-value-setq (ra dec)
	    (precess-ra-dec-to-j2000 epoch ra dec)))

    ;; FIXME - it is possible that the wcs will be mis-centered
    ;; because of off-center center readout CCDSEC - probably a rare case
    
    (wcs:make-wcs-radec-tan
     :crval1 ra
     :crval2 dec
     :crpix1 (* 0.5d0 naxis1)
     :crpix2 (* 0.5d0 naxis2)
     :cd1_1  (* +1 pixel-scale) 
     :cd2_2  (* -1 pixel-scale) 
     :cd1_2  0d0
     :cd2_1  0d0
     :equinox 2000d0))) 
		    
(defmethod insert-initial-wcs-for-instrument
    ((inst mcdonald-0.8m-pfc) fits-file &key extension)
  (declare (ignore extension))
  (when (not (cf:read-wcs fits-file))
    (let ((wcs (get-initial-wcs-for-instrument inst fits-file)))
      (when (not wcs)
	(error "No initial WCS to write"))
      (cf:write-wcs wcs fits-file))))
					      
      





