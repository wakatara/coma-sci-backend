#|

a grab bag for some various old cameras on CTIO with similar headers

|#



(in-package instrument-id)

;; parent class of HFOSC camera - in the belief that they will share header qualities.
(defclass/inst %ctio-old-camera (imaging-instrument onechip)
    ((name :initform "Parent class of CTIO cameras") ;; a string
     (observatory :initform "CTIO")))

;; examples: /Volumes/PDS/CDROMS/VALIDATED/CTIO_03-92_RAW/n1.bias.fits
(defclass/inst ctio-tek1k-2 (%ctio-old-camera)
    ((name :initform "CTIO Tek1K-2"))) ;; a string

;; examples: /Volumes/PDS/CDROMS/VALIDATED/CTIO_05-89/ctiomay89n1raw/n1.00116.fits
;; evidenced by DETECTOR=13
(defclass/inst  ctio-421x576 (%ctio-old-camera) ;; 421x576 camera
    ((name :initform "CTIO 421x576 camera"))) ;; a string

(defclass/inst ctio-ti1-800x800 (%ctio-old-camera)
  ((name :initform "CTIO DETECTOR TI2 800x800 camera, unknown telescope")))

(defclass/inst ctio-ti2-800x800 (%ctio-old-camera)
  ((name :initform "CTIO DETECTOR TI2 800x800 camera, unknown telescope")))

(defclass/inst ctio-ti3-800x800 (%ctio-old-camera)
  ((name :initform "CTIO DETECTOR TI3 (TI 3) 800x800 camera, unknown telescope")))

(defclass/inst ctio-515-419x400 (%ctio-old-camera)
  ((name :initform "CTIO DETECTOR 555 419x400camera, unknown telescope")))

(defclass/inst ctio-cfccd-on-ctio1.5m  (%ctio-old-camera)
  ((name :initform "CTIO Cass 2k direct imager on CTIO 1.5m")))

(defclass/inst ctio-tek1-552x512  (%ctio-old-camera)
  ((name :initform "CTIO 552x512 old Tek1 CCD")))

(defclass/inst ctio-detector-2049 (%ctio-old-camera)
  ((name :initform "CTIO 1K camera with DETECTOR '2049'")))

;; the TEK4 seems to be an error because raw files that start as Tek1
;; end up as "Tek4" or "Tek 4"
(defclass/inst ctio-tek4-552x512-BOGUS-NAME-FOR-TEK1  (ctio-tek1-552x512)
  ((name :initform "CTIO 552x512 old Tek1 CCD mis-labeled as Tek4")))

(defclass/inst ctio-thomson2-1kx1k  (%ctio-old-camera)
  ((name :initform "CTIO Thomson2 1k x 1k CCD")))

(defclass/inst ctio-site-512x512 (%ctio-old-camera)
  ((name :initform "CTIO 1m SITe 512x512 CCD")))

(defclass/inst ctio-2050-1kx1k (%ctio-old-camera)
  ((name :initform "CTIO '2050' 1kx1k CCD")))


(defclass/inst 400x444-camera (%ctio-old-camera)
  ((name :initform "CTIO '2050' 1kx1k CCD")))

#|

 problem: 

   There appears to be a mythical tek4 detector in 
   /Volumes/PDS/CDROMS/VALIDATED/CTIO_04-90_RAW_FLAT/ctioapr90n1raw

|#


(defun %ctio-old-camera-identify-instrument (fits-file)
  (let ((observatory (cf:read-fits-header fits-file "OBSERVAT"))
	(telescop (cf:read-fits-header fits-file "TELESCOP"))
	(naxis1    (cf:read-fits-header fits-file "NAXIS1"))
	(naxis2    (cf:read-fits-header fits-file "NAXIS2"))
	(instrument (cf:read-fits-header fits-file "INSTRUME"))
	(detector (let ((det (cf:read-fits-header fits-file "DETECTOR")))
		    (if (stringp det)
			(string-trim " " det)))))
    (declare (ignorable naxis1 naxis2))
    (cond ((equalp observatory "CTIO")
	   (cond
	     ;;
	     ((not (and (integerp naxis1) (integerp naxis2)))
	      nil)  ;; not a first image header
	     ;;
	     ((equalp detector "Tek1K-2")
	      (make-instance 'ctio-tek1k-2))
	     ;;
	     ((equalp detector "Tek1")
	      (make-instance 'ctio-tek1-552x512))
	     ;;
	     ((equalp detector "2049")
	       (make-instance 'ctio-detector-2049))
	     ;;
	     ((equalp detector "Thomson2")
	       (make-instance 'ctio-thomson2-1kx1k))
	     ;;
	     ;; bogus name for Tek1?
	     ((equalp (remove #\space detector) "Tek4")
	      (make-instance 'ctio-tek4-552x512-BOGUS-NAME-FOR-TEK1))
	     ;;
	     ((and (equalp telescop "CTIO 1.5 meter telescope")
		   (equalp detector "Site2K_6")
		   (equalp instrument "cfccd"))
	      (make-instance 'ctio-cfccd-on-ctio1.5m))
	     ;;
	     ((equalp detector "TI1")
	      (make-instance 'ctio-ti1-800x800))
	     ;;
	     ((equalp detector "TI2")
	      (make-instance 'ctio-ti2-800x800))
	     ;;
	     ((equalp (remove #\space detector) "TI3") ;; written as 'TI 3'
	      (make-instance 'ctio-ti3-800x800))
	     ;;
	     ((equalp detector "515")
	      (make-instance 'ctio-515-419x400))
	     ;;
	     ((equalp detector "13")
	      (make-instance 'ctio-421x576))
	     ;;
	     ((and (equalp detector "SITe 512x512")
		   (equalp instrument "Y4KCam"))
	      (make-instance 'ctio-site-512x512))
	     ;;
	     ((equalp detector "2050")
	      (make-instance 'ctio-2050-1kx1k))
	     ;;
	     )))))
	  

(%add-instrument-id-function '%ctio-old-camera-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst %ctio-old-camera)
						fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("OBSERVAT" "TELSCOP" "INSTRUME" "DETECTOR" "FILTER" "FILTERS"
      "FILTER2" "FILTERID" "IMAGETYP" "IMGTYP" "UT" "DATE-OBS"
      "MJD-OBS" "CCDXBIN" "CCDYBIN" "RA" "DEC" "EPOCH" "EQUINOX")
    (call-next-method))
   :test 'equalp))

;; In these images, there is a FILTERS keyword representing the
;; "filter bolt positons" but that sometimes represents an actual
;; FILTER.  There is no universal map of the position to actual
;; filters, so we assume that if FILTERS is not a filter, then the
;; headers have been updated with a legitimate filter with a FILTER
;; keyword, or else return NIL.
(defmethod get-standard-filter-for-instrument
    ((inst %ctio-old-camera) fits-file)
  (let ((filter (cf:read-fits-header fits-file "FILTER"))
	(filters (%gethead-or-error fits-file "FILTERS")))
    (flet ((get-filter (filt)
	     (cond ((equalp filt "U") :uj)
		   ((equalp filt "B") :bj)
		   ((equalp filt "V") :vj)
		   ((equalp filt "R") :rc)
		   ((equalp filt "I") :ic)
		   ;;
		   (t NIL))))
      (or
       ;; maybe FILTERS is valid
       (get-filter filters)
       ;; else maybe we inserted a FILTER keyword
       (get-filter filter)))))

;; the ctio-cfccd-on-ctio1.5m has more detail 
(defmethod get-standard-filter-for-instrument
    ((inst ctio-cfccd-on-ctio1.5m) fits-file)
  (let ((filter2 (%gethead-or-error fits-file "FILTER2")))
    (flet ((get-filter (filt)
	     (cond ((equalp filt "U") :uj)
		   ((equalp filt "B") :bj)
		   ((equalp filt "V") :vj)
		   ((equalp filt "R") :rc)
		   ((equalp filt "I") :ic)
		   ;;
		   (t NIL))))
      ;; assume FILTER2 is valid
      (get-filter filter2))))

;; the ctio-site-512x512 has a FILTERID
(defmethod get-standard-filter-for-instrument
    ((inst ctio-site-512x512) fits-file)
  (let ((filterid (%gethead-or-error fits-file "FILTERID")))
    (flet ((get-filter (filt)
	     (cond ((equalp filt "U") :uj)
		   ((equalp filt "B") :bj)
		   ((equalp filt "V") :vj)
		   ((equalp filt "R") :rc)
		   ((equalp filt "I") :ic)
		   ;;
		   (t NIL))))
      ;; assume FILTER2 is valid
      (get-filter filterid))))


;; generic version OK
;(defmethod get-exptime-for-instrument ((inst %ctio-old-camera) fits-file) ... )

(defmethod get-object-for-instrument ((inst %ctio-old-camera) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst %ctio-old-camera) fits-file)
  (let ((obj-type (or (cf:read-fits-header  fits-file  "IMAGETYP")
		      (cf:read-fits-header  fits-file  "IMGTYPE")
		      (error "IMAGETYP or IMGTYPE header not found in ~A fits-file"
			     fits-file))))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ;; can be "SKY FLAT" or "DOME FLAT"
	  ((search "FLAT" obj-type :test 'equalp) :FLAT)
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))




;; and we need to fix this to be YYYY-MM-DD
(defun %fix-old-ctio-date-string (dstring)
  (cond ((not (find #\/ dstring))
	 dstring) ;; string is OK new-style date
	;;
	(t
	 (astro-time:parse-dd/mm/yy-string dstring :output :string))))


(defmethod get-mjd-start-for-instrument ((inst %ctio-old-camera) fits-file)
  (let* ((ut-string (%gethead-or-error fits-file "UT"))
	 (date-string-raw  (%gethead-or-error fits-file "DATE-OBS"))
	 (date-string (%fix-old-ctio-date-string date-string-raw))
	 (full-ut-string  (format nil "~AT~A" date-string ut-string))	 
	 (mjd-start 
	   (multiple-value-bind (year month day hour min sec)
	       (astro-time:parse-ut-date-and-time-string full-ut-string)
	     (astro-time:calendar-date-to-mjd year month day hour min sec))))
    mjd-start))


(defmethod get-mjd-start-for-instrument ((inst ctio-site-512x512) fits-file)
  (declare (ignore inst))
  (%gethead-or-error fits-file "MJD-OBS"))


;; generic version OK
;;(defmethod get-mjd-mid-for-instrument ((inst %ctio-old-camera) fits-file) ...  )





(defmethod get-gain-for-instrument ((inst %ctio-old-camera)
				    fits-file  &key extension)
  (declare (ignore inst extension))
  nil)


;; the ctio-cfccd-on-ctio1.5m has a 4 gains (upper/lower/left/right)
;; but it seems like it has just 2 amps.  It seems that
;; only amps 22 and 21 are applicable (is here a 4 amp read mode?)
(defmethod get-gain-for-instrument ((inst ctio-cfccd-on-ctio1.5m)
				    fits-file  &key extension)
  (declare (ignore inst extension))
  (or
   ;; try to see if there is a user-inserted value
   (cf:read-fits-header fits-file "GAIN")
   ;;
   (let ((g21 (cf:read-fits-header fits-file "GTGAIN21"))
	 (g22 (cf:read-fits-header fits-file "GTGAIN22")))
     (if (and g21 g22)
	 (* 0.5 (+ g21 g22))))
   ;;
   nil))


(defmethod get-gain-for-instrument ((inst ctio-tek1k-2)
				    fits-file  &key extension)
  (declare (ignore inst extension))
  (or
   (cf:read-fits-header fits-file "GAIN") 
   nil #+nil 2.5)) ;; this is approximate, based in image statistics. Could be really wrong.


(defmethod get-gain-for-instrument ((inst ctio-ti1-800x800)
				    fits-file  &key extension)
  (declare (ignore inst extension))
  (or
   (cf:read-fits-header fits-file "GAIN") 
   nil #+nil 0.90)) ;; this is approximate, based in image statistics. Could be really wrong.


(defmethod get-gain-for-instrument ((inst ctio-ti2-800x800)
				    fits-file  &key extension)
  (declare (ignore inst extension))
  (or
   (cf:read-fits-header fits-file "GAIN") 
   nil #+nil 3.0)) ;; this is approximate, based in image statistics. Could be really wrong.

(defmethod get-gain-for-instrument ((inst ctio-ti3-800x800)
				    fits-file  &key extension)
  (declare (ignore inst extension))
  (or
   (cf:read-fits-header fits-file "GAIN") 
   nil #+nil 3.0)) ;; this is approximate, based in image statistics. Could be really wrong.


(defmethod get-gain-for-instrument ((inst ctio-421x576)
				    fits-file  &key extension)
  (declare (ignore inst extension))
  (or
   (cf:read-fits-header fits-file "GAIN") 
   nil #+nil 1.25)) ;; this is approximate, based in image statistics

(defmethod get-gain-for-instrument ((inst ctio-tek1-552x512)
				    fits-file  &key extension)
  (declare (ignore inst extension))
  (or
   (cf:read-fits-header fits-file "GAIN") 
   nil #+nil 1.20)) ;; this is approximate, based in image statistics

(defmethod get-gain-for-instrument ((inst ctio-thomson2-1kx1k)
				    fits-file  &key extension)
  (declare (ignore inst extension))
  (or
   (cf:read-fits-header fits-file "GAIN") 
   nil #+nil 0.5)) ;; this is approximate, based in image statistics

(defmethod get-gain-for-instrument ((inst ctio-2050-1kx1k)
				    fits-file  &key extension)
  (declare (ignore inst extension))
  (or
   (cf:read-fits-header fits-file "GAIN") 
   nil #+nil 2.1)) ;; this is likely WRONG, because it is hard to get consistent answer from stats

(defmethod get-gain-for-instrument ((inst ctio-site-512x512)
				    fits-file  &key extension)
  (declare (ignore inst extension))
  (%gethead-or-error fits-file "GAIN"))



;; use general methods because we assume that whole image is valid
;; (and images can be trimmed)
(defmethod get-trimsec-for-instrument ((inst %ctio-old-camera)
				       fits-file &key extension)
  (declare (ignore inst fits-file extension))
  (call-next-method))
(defmethod get-datasec-for-instrument  ((inst %ctio-old-camera) fits-file &key extension)
  (declare (ignore inst fits-file extension))
  (call-next-method))



(defmethod get-pixel-scale-for-instrument  ((inst %ctio-old-camera)
					    fits-file &key extension)
  (declare (ignore inst extension))
  (%gethead-or-error fits-file "SECPPIX"))  ;; this seems to be off by 5% for the 1k.


(defmethod get-pixel-scale-for-instrument ((inst ctio-tek1k-2) fits-file
					   &key extension)
  (declare (ignore fits-file inst extension))
  0.4697d0) ;; this appears to be the measured value, but header SECPPIX says 0.446.


(defmethod get-pixel-scale-for-instrument ((inst ctio-421x576)
					   fits-file &key extension)
  (declare (ignore fits-file inst extension))
  2.12d0) ;; this appears to be the measured value,
          ;; but header SECPPIX says 0.362
  

(defmethod get-pixel-scale-for-instrument ((inst ctio-tek1-552x512)
					   fits-file &key extension)
  (declare (ignore fits-file inst extension))
  0.177)


(defmethod get-pixel-scale-for-instrument ((inst ctio-thomson2-1kx1k)
					   fits-file &key extension)
  (declare (ignore fits-file inst extension))
  1.84) ;; this one is WEIRD - sometimes it says SECPPIX=1.8 and sometimes
        ;; 0.33, but fitting them seems to give 1.84. 


;; this one is off by a fair bit - seccpix says 0.446 but a fit is 0.471
(defmethod get-pixel-scale-for-instrument  ((inst ctio-detector-2049)
					    fits-file &key extension)
  (declare (ignore inst extension))
  (let* ((secppix (%gethead-or-error fits-file "SECPPIX")))
    (* 1.056 secppix))) ;; our empirical correction

(defmethod get-pixel-scale-for-instrument ((inst ctio-cfccd-on-ctio1.5m)
					   fits-file &key extension)
  (declare (ignore  inst extension))
  (let ((xpixsize (%gethead-or-error fits-file "XPIXSIZE"))
	(ypixsize (%gethead-or-error fits-file "YPIXSIZE")))
    (when (not (= xpixsize ypixsize))
      (error "XPIXSIZE=~A is not same as YPIXSIZE=~A" xpixsize ypixsize))
    xpixsize))


(defmethod get-pixel-scale-for-instrument ((inst ctio-site-512x512)
					   fits-file &key extension)
  (declare (ignore inst extension))
  (let ((b1 (%gethead-or-error fits-file "CCDXBIN")))
    (/ 0.469d0 b1)))
  
(defmethod get-pixel-scale-for-instrument ((inst ctio-ti1-800x800)
					    fits-file &key extension)
  (declare (ignore inst fits-file extension))
  1.47d0) ;; from a fit

;; NOTE - SECPPIX header is OK for ctio-ti2-800x800

(defmethod get-pixel-scale-for-instrument ((inst ctio-2050-1kx1k)
					    fits-file &key extension)
  (declare (ignore inst fits-file extension))
  0.471d0) ;; from a fit - close to SECPPIX=0.446 but not close enough

;; UH88 starts with a WCS 
(defmethod get-initial-wcs-for-instrument ((inst %ctio-old-camera) fits-file
					   &key extension)
  (declare (ignore extension))

  (let* ((rastr (%gethead-or-error fits-file "RA"))
	 (decstr (%gethead-or-error fits-file "Dec"))
	 (pix-scale/deg (/ (get-pixel-scale-for-instrument inst fits-file) 3600d0))
	 (naxis1    (%gethead-or-error fits-file "NAXIS1"))
	 (naxis2    (%gethead-or-error fits-file "NAXIS2"))
	 (ra (ra-dec:hms-string->deg rastr))
	 (dec (ra-dec:dms-string->deg decstr))
	 ;; don't trust epoch in header?  It seems that epoch is somtimes that
	 ;; of the observations
	 (current-epoch-year
	   (astro-time:mjd-to-decimal-year
	    (get-mjd-mid-for-instrument inst fits-file)))
	 (epoch (cf:read-fits-header fits-file "EPOCH"))
	 (equinox (cf:read-fits-header fits-file "EQUINOX"))) ;; only some insts 

    (when (not epoch)  ;; this is a wild desperate guess
      (setf epoch current-epoch-year))

    ;; precess to current era - if EPOCH=0 assume that epoch is the
    ;; time of observation.  This is just a guess.
    (multiple-value-setq (ra dec)
      (precess-ra-dec-to-j2000
       (if (zerop epoch) current-epoch-year epoch)
       ra dec))
    

    (cond ((typep inst 'ctio-tek1k-2)
	   (wcs:make-wcs-radec-tan
	    :crval1 ra
	    :crval2 dec
	    :crpix1 (* 0.5d0 naxis1)
	    :crpix2 (* 0.5d0 naxis2)
	    :cd1_1  0d0
	    :cd2_2  0d0
	    :cd1_2  (* -1 pix-scale/deg)
	    :cd2_1  (* +1 pix-scale/deg)
	    :equinox 2000d0))
	  ;;
	  ((typep inst 'ctio-421x576)
	   (wcs:make-wcs-radec-tan
	    :crval1 ra
	    :crval2 dec
	    :crpix1 (* 0.5d0 naxis1)
	    :crpix2 (* 0.5d0 naxis2)
	    :cd1_1  0d0
	    :cd2_2  0d0
	    :cd1_2  (* +1 pix-scale/deg)
	    :cd2_1  (* -1 pix-scale/deg)
	    :equinox 2000d0))
	  ;;
	  ((typep inst 'ctio-ti1-800x800) ;; SECPPIX very wrong for this one
	   (wcs:make-wcs-radec-tan
	    :crval1 ra
	    :crval2 dec
	    :crpix1 (* 0.5d0 naxis1)
	    :crpix2 (* 0.5d0 naxis2)
	    :cd1_1  0d0
	    :cd2_2  0d0
	    :cd1_2  (* +1 pix-scale/deg)
	    :cd2_1  (* -1 pix-scale/deg)
	    :equinox 2000d0))
	  ;;
	  ;;   curiously the ti1,ti2 cameras are rotated by 90 to each other
	  ((typep inst 'ctio-ti2-800x800) ;; SECPPIX is OK for this one
	   (wcs:make-wcs-radec-tan
	    :crval1 ra
	    :crval2 dec
	    :crpix1 (* 0.5d0 naxis1)
	    :crpix2 (* 0.5d0 naxis2)
	    :cd1_1  (* -1 pix-scale/deg)
	    :cd2_2  (* -1 pix-scale/deg)
	    :cd1_2  0d0
	    :cd2_1  0d0
	    :equinox 2000d0))
	  ;;
	  ;; and TI3 has yet another flip
	  ((typep inst 'ctio-ti3-800x800) ;; SECPPIX is OK for this one
	   (wcs:make-wcs-radec-tan
	    :crval1 ra
	    :crval2 dec
	    :crpix1 (* 0.5d0 naxis1)
	    :crpix2 (* 0.5d0 naxis2)
	    :cd1_1  (* +1 pix-scale/deg)
	    :cd2_2  (* +1 pix-scale/deg)
	    :cd1_2  0d0
	    :cd2_1  0d0
	    :equinox 2000d0))
	  ;;
	  ((typep inst 'ctio-detector-2049) ;; SECPPIX is OK for this one
	   (wcs:make-wcs-radec-tan
	    :crval1 ra
	    :crval2 dec
	    :crpix1 (* 0.5d0 naxis1)
	    :crpix2 (* 0.5d0 naxis2)
	    :cd1_1  0d0
	    :cd2_2  0d0
	    :cd1_2  (* +1 pix-scale/deg)
	    :cd2_1  (* -1 pix-scale/deg)
	    :equinox 2000d0))
	  ;;
	  ((typep inst 'ctio-515-419x400)
	   (wcs:make-wcs-radec-tan
	    :crval1 ra
	    :crval2 dec
	    :crpix1 (* 0.5d0 naxis1)
	    :crpix2 (* 0.5d0 naxis2)
	    :cd1_1  0d0
	    :cd2_2  0d0
	    :cd1_2  (* -1 pix-scale/deg)
	    :cd2_1  (* +1 pix-scale/deg)
	    :equinox 2000d0))
	  ;;
	  ((typep inst 'ctio-cfccd-on-ctio1.5m)
	   (wcs:make-wcs-radec-tan
	    :crval1 ra
	    :crval2 dec
	    :crpix1 (* 0.5d0 naxis1)
	    :crpix2 (* 0.5d0 naxis2)
	    :cd1_1  0d0
	    :cd2_2  0d0
	    :cd1_2  (* -1 pix-scale/deg)
	    :cd2_1  (* +1 pix-scale/deg)
	    :equinox 2000d0))
	  ;;
	  ((typep inst 'ctio-tek1-552x512)
	   (wcs:make-wcs-radec-tan
	    :crval1 ra
	    :crval2 dec
	    :crpix1 (* 0.5d0 naxis1)
	    :crpix2 (* 0.5d0 naxis2)
	    :cd1_1  0d0
	    :cd2_2  0d0
	    :cd1_2  (* +1 pix-scale/deg)
	    :cd2_1  (* -1 pix-scale/deg)
	    :equinox 2000d0))
	  ;;
	  ((typep inst 'ctio-thomson2-1kx1k)
	   ;; this instrument has zero ra,dec frequently. Ooops!
	   (if (not (and (zerop ra) (zerop dec)))
	       (wcs:make-wcs-radec-tan
		:crval1 ra
		:crval2 dec
		:crpix1 (* 0.5d0 naxis1)
		:crpix2 (* 0.5d0 naxis2)
		:cd1_1  (* +1 pix-scale/deg)
		:cd2_2  (* +1 pix-scale/deg)
		:cd1_2  0d0
		:cd2_1  0d0
		:equinox 2000d0)))
	  ;;
	  ((typep inst 'ctio-site-512x512)
	   ;; this instrument has zero ra,dec frequently. Ooops!
	   (if (floatp equinox) ;; should be here for this inst
	       (wcs:make-wcs-radec-tan
		:crval1 ra
		:crval2 dec
		:crpix1 (* 0.5d0 naxis1)
		:crpix2 (* 0.5d0 naxis2)
		:cd1_1  0d0
		:cd2_2  0d0
		:cd1_2  (* +1 pix-scale/deg)
		:cd2_1  (* +1 pix-scale/deg)
		:equinox 2000d0)))
	  ;;
	  ((typep inst 'ctio-2050-1kx1k)
	   (wcs:make-wcs-radec-tan
	    :crval1 ra
	    :crval2 dec
	    :crpix1 (* 0.5d0 naxis1)
	    :crpix2 (* 0.5d0 naxis2)
	    :cd1_1  0d0
	    :cd2_2  0d0
	    :cd1_2  (* -1 pix-scale/deg)
	    :cd2_1  (* +1 pix-scale/deg)
	    :equinox 2000d0))
	  )))



(defmethod insert-initial-wcs-for-instrument ((inst %ctio-old-camera) fits-file
					       &key extension)
  (declare (ignore extension))
  (when t ;(not (cf:read-wcs fits-file))
    (cf:write-wcs (get-initial-wcs-for-fits fits-file)
		  fits-file)))



