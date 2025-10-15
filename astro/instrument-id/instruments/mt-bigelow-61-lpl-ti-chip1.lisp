#|

Mt. Bigelow 61" with LPL TI Chip #1

This is a dichroic camera with non-standard filters

CHIP-ID = 'LPL TI CHIP #1 (JPL 255)  '
TELESCOP= '61 in. MOUNT BIGELOW'

RA,DEC set to zero in headers, and no EPOCH given.


Very little information:

https://pds.nasa.gov/ds-view/pds/viewInstrumentProfile.jsp?INSTRUMENT_ID=LPLCCD&INSTRUMENT_HOST_ID=MTBG61



|#



(in-package instrument-id)

 

;; examples: /Volumes/PDS/CDROMS/VALIDATED/CTIO_03-92_RAW/n1.bias.fits
(defclass/inst mt-bigelow-61in-lpl-chip1 (imaging-instrument onechip)
  ((name :initform "Mt Bigelow 61 inch")
   (aperture :initform 1.55) ;; 61 inches
   (observatory :initform "MtBigelow"))) 



(defun mt-bigelow-61in-lpl-chip1-identify-instrument (fits-file)
  (when (and (equalp (cf:read-fits-header fits-file "TELESCOP")
		     "61 in. MOUNT BIGELOW")
	     (search "LPL TI CHIP #1"
		     (cf:read-fits-header fits-file "CHIP-ID")))
    (make-instance 'mt-bigelow-61in-lpl-chip1)))
 
    

(%add-instrument-id-function 'mt-bigelow-61in-lpl-chip1-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst mt-bigelow-61in-lpl-chip1) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("CHIP-ID" "TELESCOP")
    (call-next-method))
   :test 'equalp))

;; In these images, there is a FILTERS keyword representing the
;; "filter bolt positons" but that sometimes represents an actual
;; FILTER.  There is no universal map of the position to actual
;; filters, so we assume that if FILTERS is not a filter, then the
;; headers have been updated with a legitimate filter with a FILTER
;; keyword, or else return NIL.
(defmethod get-standard-filter-for-instrument
    ((inst mt-bigelow-61in-lpl-chip1) fits-file)
  (let* ((filter (cf:read-fits-header fits-file "FILTER")))

    (cond ((equalp filter "OPEN") :open-mt-bigelow-61)
	  ((equalp filter "VR")   :vr-mt-bigelow-61)
	  ((equalp filter "IJ")   :ij-mt-bigelow-61)
	  ((equalp filter "BLUE/DICHR") :blue/dichr-mt-bigelow-61)
	  ((equalp filter "BLUE") :blue-mt-bigelow-61)
	  ((equalp filter "RED") :blue-mt-bigelow-61)
	  ;;
	  ;;
	  (t NIL))))




;; exposure is in millisec
(defmethod get-exptime-for-instrument ((inst mt-bigelow-61in-lpl-chip1) fits-file)
  (declare (ignorable inst))
  (* 0.001d0 (%gethead-or-error fits-file "EXPTIME")))

(defmethod get-object-for-instrument ((inst mt-bigelow-61in-lpl-chip1) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst mt-bigelow-61in-lpl-chip1)
					   fits-file)
  (declare (ignore inst fits-file))
  :unknown) ;; no info


(defmethod get-mjd-start-for-instrument ((inst mt-bigelow-61in-lpl-chip1)
					 fits-file)
  (declare (ignore inst fits-file))
  nil) ;; no observing date given!


(defmethod get-mjd-mid-for-instrument ((inst mt-bigelow-61in-lpl-chip1)
					 fits-file)
  (declare (ignore inst fits-file))
  nil) ;; no observing date given!


;; we THINK this is the ESCALE header (e-/ADCU)
(defmethod get-gain-for-instrument ((inst mt-bigelow-61in-lpl-chip1)
				    fits-file  &key extension)
  (declare (ignore inst extension))
  nil) ;; no gain given!



;; use general methods because we assume that whole image is valid
;; (and images can be trimmed)
(defmethod get-trimsec-for-instrument ((inst mt-bigelow-61in-lpl-chip1)
				       fits-file &key extension)
  (declare (ignore inst fits-file extension))
  (call-next-method))

(defmethod get-datasec-for-instrument  ((inst mt-bigelow-61in-lpl-chip1)
					fits-file &key extension)
  (declare (ignore inst fits-file extension))
  (call-next-method))



(defmethod get-pixel-scale-for-instrument  ((inst mt-bigelow-61in-lpl-chip1)
					    fits-file &key extension)
  (declare (ignore inst extension fits-file))
  0.907d0) ;; from astrometry.net fit - no info on binning
  



;; this is useless, because RA,DEC,EPOCH usually not given
(defmethod get-initial-wcs-for-instrument
    ((inst mt-bigelow-61in-lpl-chip1) fits-file
     &key extension)
  (declare (ignore extension))

  (let* ((rastr (%gethead-or-error fits-file "RA"))
	 (decstr (%gethead-or-error fits-file "Dec"))
	 (pix-scale/deg (/ (get-pixel-scale-for-instrument inst fits-file)
			   3600d0))
	 (naxis1    (%gethead-or-error fits-file "NAXIS1"))
	 (naxis2    (%gethead-or-error fits-file "NAXIS2"))
	 (ra (ra-dec:hms-string->deg rastr))
	 (dec (ra-dec:dms-string->deg decstr))
	 ;; don't trust epoch in header?  It seems that epoch is that
	 ;; of the observations
	 (current-epoch-year
	   (astro-time:mjd-to-decimal-year
	    (get-mjd-mid-for-instrument inst fits-file)))
	 (epoch (or (cf:read-fits-header fits-file "EPOCH") 2000d0)))

    (when (and (zerop ra) (zerop dec))
      (error "RA and DEC are both zero - undefined"))
    
    
    ;; precess to current era - if EPOCH=0 assume that epoch is the
    ;; time of observation.  This is just a guess.
    (multiple-value-setq (ra dec)
      (precess-ra-dec-to-j2000
       (if (zerop epoch) current-epoch-year epoch)
       ra dec))


    (cond ((typep inst 'mt-bigelow-61in-lpl-chip1)
	   (wcs:make-wcs-radec-tan
	    :crval1 ra
	    :crval2 dec
	    :crpix1 (* 0.5d0 naxis1)
	    :crpix2 (* 0.5d0 naxis2)
	    :cd1_1  0d0
	    :cd2_2  0d0
	    :cd1_2  (* +1 pix-scale/deg)
	    :cd2_1  (* +1 pix-scale/deg)
	    :equinox 2000d0))

	  (t
	   nil))))





(defmethod insert-initial-wcs-for-instrument ((inst mt-bigelow-61in-lpl-chip1)
					      fits-file
					       &key extension)
  (declare (ignore extension))
  (when (not (cf:read-wcs fits-file))
    (cf:write-wcs (get-initial-wcs-for-fits fits-file)
		  fits-file)))



