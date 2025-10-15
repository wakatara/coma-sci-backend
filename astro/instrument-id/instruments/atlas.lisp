

(in-package instrument-id)

;; parent class of HFOSC camera

(defclass/inst %atlas (imaging-instrument)
  ((name :initform "ATLAS")
   (aperture :initform 0.50)
   (observatory  :initform nil)))


;; general stamp class
(defclass/inst %atlas-stamp (preprocessed %atlas onechip)
  ((wcs-origin :initform "atlas/refcat")
   (phot-calib-origin :initform "atlas/refcat")))



(defclass %atlas-diff-mixin () ()) ;; difference
(defclass %atlas-reduced-mixin () ())  ;; reduced

(defclass %atlas-hko-mixin ()
  ((observatory :initarg :observatory :initform "T05")))
(defclass %atlas-mlo-mixin ()
  ((observatory :initarg :observatory :initform "T08")))
(defclass %atlas-sutherland-mixin ()
  ((observatory :initarg :observatory :initform "M22")))
(defclass  %atlas-chile-mixin ()
  ((observatory :initarg :observatory :initform "W68")))
(defclass  %atlas-tdo-mixin ()
  ((observatory :initarg :observatory :initform "Y64")))

(defclass/inst atlas-stamp/hko
  (%atlas-hko-mixin %atlas-reduced-mixin %atlas-stamp)
  ())
;;
(defclass/inst atlas-stamp-diff/hko
  (%atlas-hko-mixin %atlas-diff-mixin %atlas-stamp)
  ())

(defclass/inst atlas-stamp/mlo
  (%atlas-mlo-mixin %atlas-reduced-mixin %atlas-stamp)
  ())
;;
(defclass/inst atlas-stamp-diff/mlo
  (%atlas-mlo-mixin %atlas-diff-mixin %atlas-stamp)
  ())

(defclass/inst atlas-stamp/sutherland 
  (%atlas-sutherland-mixin %atlas-reduced-mixin
			   %atlas-stamp)
  ())
;;
(defclass/inst atlas-stamp-diff/sutherland
  (%atlas-sutherland-mixin %atlas-diff-mixin %atlas-stamp)
  ())


(defclass/inst atlas-stamp/chile
  (%atlas-chile-mixin %atlas-reduced-mixin %atlas-stamp)
  ())
;;
(defclass/inst atlas-stamp-diff/chile 
  (%atlas-chile-mixin %atlas-diff-mixin %atlas-stamp)
  ())

(defclass/inst atlas-stamp/tdo
  (%atlas-tdo-mixin %atlas-reduced-mixin %atlas-stamp)
  ())
;;
(defclass/inst atlas-stamp-diff/tdo
  (%atlas-tdo-mixin %atlas-diff-mixin %atlas-stamp)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUX Camera
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/inst %atlas-aux-stamp (%atlas-stamp)  ;; a child class for the AUX cameras
  ((aperture :initform 0.50)))

(defclass/inst atlas-aux-stamp/hko
  (%atlas-hko-mixin %atlas-reduced-mixin %atlas-aux-stamp)
  ())
;;
(defclass/inst atlas-aux-stamp-diff/hko
  (%atlas-hko-mixin %atlas-diff-mixin %atlas-aux-stamp)
  ())

(defclass/inst atlas-aux-stamp/mlo
  (%atlas-mlo-mixin %atlas-reduced-mixin %atlas-aux-stamp)
  ())
;;
(defclass/inst atlas-aux-stamp-diff/mlo
  (%atlas-mlo-mixin %atlas-diff-mixin %atlas-aux-stamp)
  ())

(defclass/inst atlas-aux-stamp/sutherland 
  (%atlas-sutherland-mixin %atlas-reduced-mixin
			   %atlas-aux-stamp)
  ())
;;
(defclass/inst atlas-aux-stamp-diff/sutherland
  (%atlas-sutherland-mixin %atlas-diff-mixin %atlas-aux-stamp)
  ())


(defclass/inst atlas-aux-stamp/chile
  (%atlas-chile-mixin %atlas-reduced-mixin %atlas-aux-stamp)
  ())
;;
(defclass/inst atlas-aux-stamp-diff/chile 
  (%atlas-chile-mixin %atlas-diff-mixin %atlas-aux-stamp)
  ())

(defclass/inst atlas-aux-stamp/tdo
  (%atlas-tdo-mixin %atlas-reduced-mixin %atlas-aux-stamp)
  ())
;;
(defclass/inst atlas-aux-stamp-diff/tdo
  (%atlas-tdo-mixin %atlas-diff-mixin %atlas-aux-stamp)
  ())



#|



Also be ready to handle

1. shallower AUXCAM files

2. the Tenerife (TDR) telescope, with "05r" site code

|#

#|

eg, for OBSNAME="02a60205o0444c"

(values "hko" ;; site:  hko, mko, suth, chile, tdo
         #\a   ;; camera: a,x,k,s,r  (r for tdo only)
         60205 ;; MJD of night
         #\o   ;; exposure type:  o, s, f, d e
         444   ;; exposure of night
         #\c   ;; filter: c,o,H,g,r,i,z(?)
  )
|#


(defun %parse-atlas-obsname (obsname)
  (when (and (stringp obsname)
	     (= (length obsname) 14))
    (let* ((sitestr (subseq obsname 0 2))
	   (camchar (aref obsname 2))
	   (mjdstr (subseq obsname 3 8))
	   (obstypechar (aref obsname 8))
	   (expstr (subseq obsname 9 13))
	   (filterchar (aref obsname 13))
	   ;;
	   (site (cond ((equalp sitestr "01") "mlo")
		       ((equalp sitestr "02") "hko")
		       ((equalp sitestr "03") "suth")
		       ((equalp sitestr "04") "chile")
		       ((equalp sitestr "05") "tdo"))) ;; tenerife
	   (mjd (ignore-errors (parse-integer mjdstr)))
	   (obstype (if (member obstypechar '(#\o #\s #\f #\d #\e))
			obstypechar))
	   (expnum (ignore-errors (parse-integer expstr)))
	   (filter (if (member filterchar '(#\c #\o #\H #\g #\r #\i #\z))
		       filterchar)))
      (values site camchar mjd obstype expnum  filter))))



(defun %atlas-identify-instrument (fits-file)
  (let ((flatfile (cf:read-fits-header fits-file "FLATFILE"))
	(controlr (cf:read-fits-header fits-file "CONTROLR"))
	(obsname (cf:read-fits-header fits-file  "OBSNAME")))
    (multiple-value-bind (site camera mjdnight exptype expnum filter)
	(%parse-atlas-obsname obsname)
      (declare (ignorable mjdnight exptype expnum filter))
    ;; unfortunately, no INSTRUME header
    (when (and (eql 0  (search "/atlas" flatfile))
	       (equalp controlr "Archon")
	       site camera)
      (let* ((diffim   (cf:read-fits-header fits-file "DIFFIM")))
	(cond ((eql camera #\a) ;; the main camera
	       (cond ((equalp site "hko")
		      (if diffim
			  (make-instance 'atlas-stamp-diff/hko)
			  (make-instance 'atlas-stamp/hko)))
		     ((equalp site "mlo")
		      (if diffim
			  (make-instance 'atlas-stamp-diff/mlo)
			  (make-instance 'atlas-stamp/mlo)))
		     ((equalp site "suth")
		      (if diffim
			  (make-instance 'atlas-stamp-diff/sutherland)
			  (make-instance 'atlas-stamp/sutherland)))
		     ((equalp site "chile")
		      (if diffim
			  (make-instance 'atlas-stamp-diff/chile)
			  (make-instance 'atlas-stamp/chile)))))
	      ;; the following is not tested
	      ((eql camera #\r) ;; RASA cam in TDO telescope
	       (cond ((equalp site "tdo")
		      (if diffim
			  (make-instance 'atlas-stamp-diff/tdo)
			  (make-instance 'atlas-stamp/tdo)))))
	       ;; the following is not tested
	      ((eql camera #\x) ;; the wide-field AUX camera
	       (cond ((equalp site "hko")
		      (if diffim
			  (make-instance 'atlas-aux-stamp-diff/hko)
			  (make-instance 'atlas-aux-stamp/hko)))
		     ((equalp site "mlo")
		      (if diffim
			  (make-instance 'atlas-aux-stamp-diff/mlo)
			  (make-instance 'atlas-aux-stamp/mlo)))
		     ((equalp site "suth")
		      (if diffim
			  (make-instance 'atlas-aux-stamp-diff/sutherland)
			  (make-instance 'atlas-stamp/sutherland)))
		     ((equalp site "chile")
		      (if diffim
			  (make-instance 'atlas-aux-stamp-diff/chile)
			  (make-instance 'atlas-aux-stamp/chile)))
		     ((equalp site "tdo")
		      (if diffim
			  (make-instance 'atlas-aux-stamp-diff/tdo)
			  (make-instance 'atlas-aux-stamp/tdo)))))
	       ))))))
		


(%add-instrument-id-function '%atlas-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst %atlas) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("CTLRID" "MJD-CAM" "MJD-OBS" "DARKTIME" "DARKFILE" "FLATFILE" "MASKFILE" "PIXSIZE"
      "CONTROLR" "CAMID" "DIFFIM" "MJDCAMOF" "MJDDFMOF" "OBSNAME" "OBSID" "FILTID"
      "SURVMODE" "RA-CMD" "DEC-CMD" "ENVTSKY" "ENVTEM" "ENVWDIR" "ENVHUM" "SITELONG"
      "SITELAT" "SITEELEV" "FOCUS" "MJD-DFM0" "MJD-DFM1" "MJD-DFM2" "MJD-DFM3"
      "CLOUD" "RA" "DEC" "MAGZPT" "ZPRMS" "ZPNPT" "FWHM0" "FWHMX" "FWHMY"
      "FWHMX2" "FWHMXY" "FWHMY2" "OBSTYPE" "AZ" "ALT" "SECZ" "AIRMASS" "HA" "HA-HMS"
      "LST" "LST-HMS" "LAMBDA" "BETA" "L2" "B2" "SUNELONG" "MANGLE" "MOONSKY" "SEEING"
      "SKYMAG" "MAG5SIG")      
    (call-next-method))
   :test 'equalp))

(defmethod get-standard-filter-for-instrument ((inst %atlas) fits-file)
  (let* ((ext (get-image-extension-for-onechip-instrument inst fits-file))
	 (filter (%gethead-or-error fits-file "FILTER"
				    :extension ext)))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not found in image ~A" fits-file))
    (cond ((equalp filter "o") :oatlas)
	  ((equalp filter "c") :catlas)
	  ((equalp filter "h") :hatlas)
	  ((equalp filter "g") :gab)  ;; we will have to define these, but we 
	  ((equalp filter "r") :rab)  ;; probably won't see them
	  ((equalp filter "i") :iab)
	  ;;;
	  (t NIL))))

;; generic version OK
(defmethod get-exptime-for-instrument ((inst %atlas) fits-file)
  (let* ((ext (get-image-extension-for-onechip-instrument inst fits-file)))
    (cf:read-fits-header fits-file "EXPTIME" :extension ext)))
	 

(defmethod get-object-for-instrument ((inst %atlas) fits-file)
  (let* ((ext (get-image-extension-for-onechip-instrument inst fits-file)))
    (%gethead-or-error fits-file "OBJECT" :extension ext)))

;; if 2 extensions, use 2nd
(defmethod get-image-extension-for-onechip-instrument   ((inst %atlas-stamp)
							 fits-file)
  (cf:maybe-with-open-fits-file (fits-file ff)
    (if (= (cf:fits-file-num-hdus ff) 2)
	2
	1)))

(defmethod get-object-type-for-instrument ((inst %atlas) fits-file)
  (let* ((ext (get-image-extension-for-onechip-instrument inst fits-file))
	 (obj-type (%gethead-or-error  fits-file  "OBSTYPE"
				       :extension ext)))
    (cond ((equalp obj-type "BIAS") :BIAS)     ;; not sure but probably
	  ((equalp obj-type "FLAT") :FLAT)     ;; wont see these
	  ((equalp obj-type "OBJECT") :OBJECT) 
	  ((equalp obj-type "DARK") :DARK)     ;; not sure
	  (T :OTHER))))

(defmethod get-mjd-start-for-instrument ((inst %atlas) fits-file)
  (let* ((ext (get-image-extension-for-onechip-instrument inst fits-file)))
    (cf:read-fits-header fits-file "MJD-OBS" :extension ext)))



;; generic version OK
;(defmethod get-mjd-mid-for-instrument ((inst %atlas) fits-file) ...  )

(defmethod get-gain-for-instrument ((inst %atlas) fits-file
				    &key extension)
  (declare (ignore extension))
  (let* ((ext (get-image-extension-for-onechip-instrument inst fits-file)))
    (cf:read-fits-header fits-file "GAIN" :extension ext)))


;; for a stamp, whole thing is datasec
(defmethod get-datasec-for-instrument ((inst %atlas-stamp) fits-file &key extension)
  (declare (ignorable extension))
  (let ((naxis1 (cf:read-fits-header fits-file "NAXIS1"))
	(naxis2 (cf:read-fits-header fits-file "NAXIS2")))
    (vector 1 naxis1 1 naxis2)))

(defmethod get-calibrated-zeropoint-for-instrument ((inst %atlas-stamp) fits-file
						    &key extension
						      (flux-units :adu)
						      (exposure   :one-second))
  (declare (ignorable extension)
	   (type (member :adu :electrons) flux-units)
	   (type (member :one-second :exptime) exposure))
  (let* ((ext (get-image-extension-for-onechip-instrument inst fits-file))
	 (magzp (cf:read-fits-header fits-file "MAGZPT"
				     :extension ext)) ;; ADU per sec
	 (rmszp (cf:read-fits-header fits-file "ZPRMS"
				     :extension ext))
	 (nzpt  (cf:read-fits-header fits-file "ZPNPT"
				     :extension ext))
	 (errzp ;; fudge the error on zp because a naive rms/sqrt(N)
	        ;; is too optimistic
	   (max 0.005d0 ;; put a 0.05 mag floor on it
		(/ rmszp (sqrt (1+ nzpt)))))  ;; avoid divide by zero
	 (gain  (cf:read-fits-header fits-file "GAIN"
				     :extension ext))
	 (exptime  (cf:read-fits-header fits-file "EXPTIME"
					:extension ext)))

    (when (eql flux-units :electrons)
      (incf magzp (* +2.5d0 (log gain 10))))

    (when (eql exposure :exptime) ;; header gives
      (incf magzp (* +2.5d0 (log exptime 10))))

    (values magzp errzp)))
		
    
	
    
  

