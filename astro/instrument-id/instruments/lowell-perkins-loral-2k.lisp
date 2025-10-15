#|

TELSCOP = 'PERKINS ' 
DETECTOR= 'Loral 2048x2048 CCD' / detector name
INSTRUM = 'Lowell Filter Instrument Module' / instrument name


some info here:
https://web.archive.org/web/20020215215124/http://www2.lowell.edu/Research/Mesa/instrumentation/ccdtable.html

|#



(in-package instrument-id)


(defclass/inst lowell-perkins-loral-2k (imaging-instrument onechip)
  ((name :initform "LOWELL Perkins Loral 2kx2k CCD")
   (observatory :initform "Lowell")
   (aperture :initform 1.8288) ;; lowell perkins 72"
   (saturation-level :initform 65535)
   (non-linear-level :initform 40000)))


  

(defun %lowell-perkins-loral-2k-identify-instrument (fits-file)
  (when (and
	 (equalp (cf:read-fits-header fits-file "TELSCOP") "PERKINS")
	 (or (equalp (cf:read-fits-header fits-file "DETECTOR")
		     "Loral 2048x2048 CCD")
	     ;; this looks like an older revision with less header info
	     (equalp (cf:read-fits-header fits-file "LCCDMOD")
		     "loral"))
	 (search "Lowell" (cf:read-fits-header fits-file "OBSERVAT")
		 :test 'equalp))
    (make-instance 'lowell-perkins-loral-2k)))

(%add-instrument-id-function '%lowell-perkins-loral-2k-identify-instrument)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst lowell-perkins-loral-2k)
						fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("TELESCOP" "DETECTOR" "LCCMOD" "OBSERVAT" "FILTNAME" "IMAGETYP"
      "DATE-OBS" "RA" "DEC")
    (call-next-method))
   :test 'equalp))

(defmethod get-standard-filter-for-instrument
    ((inst lowell-perkins-loral-2k) fits-file)
  (let* ((filter (%gethead-or-error fits-file "FILTNAME")))
    (cond ((equalp filter "U") :uj)
	  ((equalp filter "B") :bj)
	  ((equalp filter "V") :vj)
	  ((equalp filter "R") :rc)
	  ((equalp filter "I") :ic)
	  ((equalp filter "none") :empty)
	  ;;;
	  (t NIL))))

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst lowell-perkins-loral-2k) fits-file) ... )

(defmethod get-object-for-instrument ((inst lowell-perkins-loral-2k) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument
    ((inst lowell-perkins-loral-2k) fits-file)
  (let ((obj-type (%gethead-or-error fits-file  "IMAGETYP")))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "FLAT") :FLAT)
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))

(defmethod get-mjd-start-for-instrument
    ((inst lowell-perkins-loral-2k) fits-file)
  (let* ((date-string ;; yyyy/mm/dd
	   (%gethead-or-error fits-file "DATE-OBS"))
	 (ut-string (%gethead-or-error fits-file "UT"))
	 (full-date-string
	   (concatenate 'string 
			(substitute #\- #\/ date-string)
			"T" ut-string))
	 (mjd-start
	   (multiple-value-bind (year month day hour min sec)
	       (astro-time:parse-ut-date-and-time-string full-date-string)
	     (astro-time:calendar-date-to-mjd year month day hour min sec))))
    mjd-start))


;; generic version OK
;(defmethod get-mjd-mid-for-instrument ((inst lowell-perkins-loral-2k) fits-file) ...  )

(defmethod get-gain-for-instrument ((inst lowell-perkins-loral-2k) fits-file
				    &key (extension nil))
  (declare (ignore extension))
  (let ((gain (%gethead-or-error fits-file "GAIN")))
    ;; if gain is zero, use value from web page, and HOPE that old Loral
    ;; was same as new Loral, because the chip does not control gain.
    (if (zerop gain)
	3.1d0
	gain)))
	

;(defmethod get-chip-id-for-instrument ((inst lowell-perkins-loral-2k) fits-file) ..)

;; DATASEC header is present
;;(defmethod get-datasec-for-instrument
;;    ((inst lowell-perkins-loral-2k) fits-file &key extension)
;;  (declare (ignore extension))
;;  (vector 33 1056 1 2304))

;; note that the WCS inside lowell prism is actually a bogus linear one (at least
;; in some old images)

;; got pixel value from here:
;; https://pds.nasa.gov/ds-view/pds/viewInstrumentProfile.jsp?INSTRUMENT_ID=LO72CCD&INSTRUMENT_HOST_ID=LO72
(defmethod  get-pixel-scale-for-instrument
    ((inst lowell-perkins-loral-2k) fits-file
     &key extension)
  (declare (ignore inst extension))
  ;; no binning keyword - thank you, engineers!
  (let*
      ((naxis1 (%gethead-or-error fits-file "NAXIS1"))
       (binning
	 (cond ((< naxis1  550) 4)
	       ((< naxis1  720) 3)
	       ((< naxis1 1100) 2)
	       ((> naxis1 1900) 1)
	       (t
		(error "Can't guess binning factorfor lowell-perkins-loral-2k")))))
    ;;
    (* binning 0.096)))
    

;; this archived web page says N=up, E=right
;; http://web.archive.org/web/20020215215124/http://www.lowell.edu:80/Research/Mesa/instrumentation/ccdtable.html
;; this has been validated by blinking a frame and a sky survey image
;; after matching wcs
(defmethod get-initial-wcs-for-instrument
    ((inst lowell-perkins-loral-2k) fits-file &key (extension))
  (declare (ignore extension))
  (let* ((rastr (%gethead-or-error fits-file "RA"))
	 (decstr (%gethead-or-error fits-file "DEC"))
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

     (wcs:make-wcs-radec-tan
		:crval1 ra
		:crval2 dec
		:crpix1 (* 0.5d0 naxis1)
		:crpix2 (* 0.5d0 naxis2)
		:cd1_1  (* +1 pixel-scale) ;; east right
		:cd2_2  (* +1 pixel-scale) ;; north up
		:cd1_2  0d0
		:cd2_1  0d0
		:equinox 2000d0)))







(defmethod insert-initial-wcs-for-instrument
    ((inst lowell-perkins-loral-2k) fits-file
     &key extension)
  (declare (ignore extension))
  (cf:write-wcs (get-initial-wcs-for-instrument inst fits-file) fits-file))




