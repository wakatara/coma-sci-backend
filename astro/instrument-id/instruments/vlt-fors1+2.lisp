


(in-package instrument-id)

;; parent class
(defclass/inst %vlt-fors (imaging-instrument onechip)
    ((name :initform "VLT-FORS 1 or 2") 
     (observatory :initform "Paranal")
     (gain-keyword :initform "HIERARCH ESO DET OUT1 GAIN"
		   :accessor instrument-gain-keyword) ;; need an accessor
     ;; manual says they are basically linear to truncation limit
     (saturation-level :initform 65535)
     (non-linear-level :initform 65535)
     (aperture  :initform 8.2)))



(defclass/inst %vlt-fors1 (%vlt-fors)
    ((name :initform "VLT-FORS 1")))
(defclass/inst %vlt-fors2 (%vlt-fors)
    ((name :initform "VLT-FORS 2")))

      

;; the chips appear as separate files, so we treat them as separate
;; instruments.  This should be harmless.

(defclass/inst vlt-fors1-tek-chip1 (%vlt-fors1)  ())
(defclass/inst vlt-fors1-tek-chip2 (%vlt-fors1)  ())
;; there is some other version of FORS1 but we've never seen it


(defclass/inst vlt-fors2-mit/ll-chip1 (%vlt-fors2)  ())
(defclass/inst vlt-fors2-mit/ll-chip2 (%vlt-fors2)  ())
(defclass/inst vlt-fors2-e2v-chip1 (%vlt-fors2)  ())
(defclass/inst vlt-fors2-e2v-chip2 (%vlt-fors2)  ())



(defun %vlt-fors-identify-instrument (fits-file)
  (let ((instrum (cf:read-fits-header fits-file "INSTRUME" :extension 1))
	(telescop (cf:read-fits-header fits-file "TELESCOP" :extension 1))
	(chip1-name (cf:read-fits-header fits-file "HIERARCH ESO DET CHIP1 NAME" :extension 1))
	(chip1-id (cf:read-fits-header fits-file "HIERARCH ESO DET CHIP1 ID" :extension 1))
	(extname (cf:read-fits-header fits-file "EXTNAME" :extension 1))
	(det-out1-chip (cf:read-fits-header fits-file "HIERARCH ESO DET OUT1 CHIP" :extension 1)))
    
    (cond ((and (equalp instrum "FORS2")
		;; originally TELESCOP was ESO-VLT-U1 but some just have ESO-VLT
		(search "ESO-VLT" telescop))
	   (cond ;; split fors2 by E2V and MIT/LL versions
	     ((equalp chip1-name "MIT/LL mosaic")
	      (cond ((or (equalp extname "CHIP1")
			 (equalp det-out1-chip 1))
		     (make-instance 'vlt-fors2-mit/ll-chip1))
		    ((or (equalp extname "CHIP2")
			 (equalp det-out1-chip 2))
		     (make-instance 'vlt-fors2-mit/ll-chip2))))
	     (t ;; assume other chip will aways be e2v
	      (cond ((or (equalp extname "CHIP1")
			 (equalp det-out1-chip 1))
		     (make-instance 'vlt-fors2-e2v-chip1))
		    ((or (equalp extname" CHIP2")
			 (equalp det-out1-chip 2))
		     (make-instance 'vlt-fors2-e2v-chip2))))))
	  ;;
	  ((and (equalp instrum "FORS1")
		(search "ESO-VLT" telescop :test 'equalp))
	   ;; the only chip we know of
	   (cond
	     ((search "TK2048EB4" chip1-id :test 'eql) ;; TEK TK2048EB1, the only chip we know of
	      (cond ((or (equalp extname "CHIP1")
			 (equalp det-out1-chip 1))
		     (make-instance 'vlt-fors1-tek-chip1))
		    ((or (equalp extname "CHIP2")
			 (equalp det-out1-chip 2))
		     (make-instance 'vlt-fors2-tek-chip2))))
	     (t  ;; the other chip we can't identify
	      nil)))
	  ;;
	  (T
	   NIL))))
  



(%add-instrument-id-function '%vlt-fors-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-critical-headers-for-instrument ((inst %vlt-fors) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("HIERARCH ESO DET CHIP1 NAME"
      "HIERARCH ESO INS FILT1 ID"
      "HIERARCH ESO DET OUT1 CHIP"
      "HIERARCH ESO DET OUT1 GAIN"
      "HIERARCH ESO DPR TYPE"
      "HIERARCH ESO DPR TECH"
      )
    (call-next-method))
    :test 'equalp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see http://www.eso.org/sci/facilities/paranal/instruments/fors/doc/VLT-MAN-ESO-13100-1543_v96_0.pdf
;; table 2.1
(defmethod get-standard-filter-for-instrument ((inst %vlt-fors) fits-file)
  (let ((filter (cf:read-fits-header fits-file "HIERARCH ESO INS FILT1 NAME")))
    (cond ((not filter) :open) ;; from experience, no filter keyword means OPEN
	  ;; the BESSEL filters are probably the simple Bessel
	  ;; approximation to Johnson-Cousins
	  ((equalp filter "U_BESS") :uj)
	  ((equalp filter "B_BESS") :bj)
	  ((equalp filter "V_BESS") :vj)
	  ((equalp filter "R_BESS") :rc)
	  ((equalp filter "I_BESS") :ic)
	  ;; the HIGH filters seem to be high transmittance boxy UBVRI
	  ((equalp filter "u_HIGH") :uj)
	  ((equalp filter "b_HIGH") :bj)
	  ((equalp filter "v_HIGH") :vj)
	  ;; Special seems to be similar
	  ((equalp filter "R_SPECIAL") :rc)
	  ((equalp filter "U_SPECIAL") :uj)
	  ;; most of the GUNN filters are assumed SDSS
	  ((equalp filter "u_GUNN") :usdss)
	  ((equalp filter "g_GUNN") :gsdss)
	  ((equalp filter "r_GUNN") :rsdss)
	  ((equalp filter "i_GUNN") :isdss)
	  ((equalp filter "z_GUNN") :zsdss)
	  ;; but the v-gunn seems to be rarely used; defined Thuan and Gunn PASP 88 543
	  ((equalp filter "v_GUNN") :vgunn)
	  ;;;
	  (t NIL))))

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst %vlt-fors) fits-file) ... )

(defmethod get-object-for-instrument ((inst %vlt-fors2) fits-file)
  (%gethead-or-error fits-file "OBJECT"))
(defmethod get-object-for-instrument ((inst %vlt-fors1) fits-file)
  (%gethead-or-error fits-file "HIERARCH ESO OBS TARG NAME"))

(defmethod get-object-type-for-instrument ((inst %vlt-fors) fits-file)
  (let ((obj-type (%gethead-or-error  fits-file  "HIERARCH ESO DPR TYPE")))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "FLAT,SKY") :FLAT)
	  ((equalp obj-type "FLAT,DOME") :FLAT) ;; not sure if this exists
	  ((equalp obj-type "OBJECT") :OBJECT)
	  ((equalp obj-type "STD") :OBJECT) ;; a standard is an object
	  (T :OTHER))))

(defmethod get-mjd-start-for-instrument ((inst %vlt-fors) fits-file)
  (declare (ignorable inst))
  (%gethead-or-error fits-file "MJD-OBS"))

(defmethod get-mjd-mid-for-instrument ((inst %vlt-fors) fits-file)
  (declare (ignorable inst))
  (+ (%gethead-or-error fits-file "MJD-OBS")
     (* 0.5 (/ (%gethead-or-error fits-file "EXPTIME") 24d0 3600d0))))

;; generic version OK
(defmethod get-gain-for-instrument ((inst %vlt-fors) fits-file &key extension)
  (declare (ignorable inst fits-file extension))
  (call-next-method))

(defmethod get-pixel-scale-for-instrument ((inst %vlt-fors) fits-file &key extension)
  (declare (ignorable inst extension))
  (let ((pscale (cf:read-fits-header fits-file "HIERARCH ESO INS PIXSCALE" :extension 1))
	(xbin (cf:read-fits-header fits-file "HIERARCH ESO DET WIN1 BINX" :extension 1))
	(ybin (cf:read-fits-header fits-file "HIERARCH ESO DET WIN1 BINY" :extension 1)))
    (or (if (and pscale xbin ybin
		 (= xbin ybin))
	    (* pscale xbin)) ;; the pixel scale is UNBINNED 
      (call-next-method)))) ;; try to use WCS, which is not in bias frames
 
(defmethod get-datasec-for-instrument ((inst %vlt-fors2) fits-file &key extension)
  (declare (ignore extension))
  (let* ((inst-str (string-upcase (type-of inst))) ;; KLUDGE!
	 (chipid (cond ((search "CHIP1" inst-str) "CHIP1")
		       ((search "CHIP2" inst-str) "CHIP2")
		       (t ;; must have gotten a generic %VLT-FORS2
			(%gethead-or-error fits-file "EXTNAME")))))
    ;; different size trimsecs for two chips
    (cond ((equalp chipid "CHIP1")
	   (vector 188 1860 7 963))
	  ((equalp chipid "CHIP2")
	   (vector 188 1860 315 1028))
	  (t
	   (error "VLT-FORS2 CHIPID=~A and not one of CHIP1 or CHIP2" chipid)))))




(defmethod get-datasec-for-instrument ((inst %vlt-fors1) fits-file &key extension)
  (declare (ignore extension))
  (vector 18 2064 1  2048)) ;; this is a guess
	   

(defmethod get-chip-id-for-instrument ((inst %vlt-fors1) fits-file &key extension)
  (declare (ignorable inst extension))
  (%gethead-or-error fits-file "HIERARCH ESO DET OUT1 CHIP"))
(defmethod get-chip-id-for-instrument ((inst %vlt-fors2) fits-file &key extension)
  (declare (ignorable inst extension))
  (%gethead-or-error fits-file "EXTNAME"))

;; FORS2 starts with a WCS for objects, but so
;(defmethod get-initial-wcs-for-instrument ((inst %vlt-fors) fits-file &key extension) ..)

;(defmethod insert-initial-wcs-for-instrument ((inst %vlt-fors) fits-file &key extension)




