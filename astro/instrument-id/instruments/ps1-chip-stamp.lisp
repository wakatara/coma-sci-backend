

(in-package instrument-id)


(defclass/inst %ps-stamp (imaging-instrument onechip preprocessed)
  ((name :initform "PS1-CHIP-STAMP-PARENT") ;; a string
   (aperture :initform 1.80)
   ;; preprocessed-fields
   (wcs-origin :initform "Pan-STARRS")
   (phot-calib-origin :initform "Pan-STARRS")
   ))

(defclass %ps1-mixin ()
  ((observatory :initform "F51")))
(defclass %ps2-mixin ()
  ((observatory :initform "F52")))

(defclass %ps-stamp-compressed-mixin (%ps-stamp) ())


;; generic types of stamp (chip, warp, compressed, or not)
(defclass/inst %ps-chip-stamp (%ps-stamp)
  ((name :initform "PS-CHIP-STAMP-PARENT"))) 
(defclass/inst %ps-chip-stamp-compressed (%ps-chip-stamp  %ps-stamp-compressed-mixin)
  ((name :initform "PS-CHIP-STAMP-COMPRESSED")))
(defclass/inst %ps-warp-stamp (%ps-stamp)
  ((name :initform "PS-WARP-STAMP-PARENT")))
(defclass/inst %ps-warp-stamp-compressed (%ps-warp-stamp  %ps-stamp-compressed-mixin)
  ((name :initform "PS-WARP-STAMP-COMPRESSED")))

;; define PS1
(defclass/inst ps1-chip-stamp (%ps1-mixin %ps-chip-stamp)
  ((name :initform "PS1-CHIP-STAMP-PARENT"))) 
(defclass/inst ps1-chip-stamp-compressed (%ps1-mixin %ps-chip-stamp-compressed)
  ((name :initform "PS1-CHIP-STAMP-COMPRESSED")))
(defclass/inst ps1-warp-stamp (%ps1-mixin %ps-warp-stamp)
  ((name :initform "PS1-WARP-STAMP-PARENT")))
(defclass/inst ps1-warp-stamp-compressed (%ps1-mixin %ps-warp-stamp-compressed)
  ((name :initform "PS1-WARP-STAMP-COMPRESSED")))

;; define PS2
(defclass/inst ps2-chip-stamp (%ps2-mixin %ps-chip-stamp)
  ((name :initform "PS2-CHIP-STAMP-PARENT"))) 
(defclass/inst ps2-chip-stamp-compressed (%ps2-mixin %ps-chip-stamp-compressed)
  ((name :initform "PS2-CHIP-STAMP-COMPRESSED")))
(defclass/inst ps2-warp-stamp (%ps2-mixin %ps-warp-stamp)
  ((name :initform "PS2-WARP-STAMP-PARENT")))
(defclass/inst ps2-warp-stamp-compressed (%ps2-mixin %ps-warp-stamp-compressed)
  ((name :initform "PS2-WARP-STAMP-COMPRESSED")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun %ps1-chip-stamp-identify-instrument/ff (ff)
  (declare (type cf:fits-file ff))
  (let* ((n-ext (if (= (cf:fits-file-num-hdus ff) 2)
                    2	;; uncompressed
                    1)) ;; compressed
	 (ps1?
	   (and
            (equalp
             (or ;; not sure why 2 versions
              (cf:read-fits-header ff "INSTRUME" :extension n-ext)
              (cf:read-fits-header ff "HIERARCH FPA.INSTRUMENT" :extension n-ext))
             "GPC1")
            (equalp ;; not sure why 2 versions
             (or (cf:read-fits-header ff "TELESCOP" :extension n-ext)
		 (cf:read-fits-header ff "HIERARCH FPA.TELESCOPE" :extension n-ext))
             "PS1")))
	 (ps2?
	   (and
            (equalp
             (or ;; not sure why 2 versions
              (cf:read-fits-header ff "INSTRUME" :extension n-ext)
              (cf:read-fits-header ff "HIERARCH FPA.INSTRUMENT" :extension n-ext))
             "GPC1")
            (equalp ;; not sure why 2 versions
             (or (cf:read-fits-header ff "TELESCOP" :extension n-ext)
		 (cf:read-fits-header ff "HIERARCH FPA.TELESCOPE" :extension n-ext))
             "PS2"))))
    
    (when (or ps1? ps2?)
      
      (let ((is-warp ;; is there a better way if ID'ing warp?
              (cf:read-fits-header ff "WARP_V"  :extension n-ext)))
	(cond (ps1?
	       (make-instance
		(if is-warp
		    (if (= n-ext 1)
			'ps1-warp-stamp
			'ps1-warp-stamp-compressed)
		    (if (= n-ext 1)
			'ps1-chip-stamp
			 'ps1-chip-stamp-compressed))))
	      (ps2?
	       (if is-warp
		   (if (= n-ext 1)
		       'ps2-warp-stamp
		       'ps2-warp-stamp-compressed)
		   (if (= n-ext 1)
		       'ps2-chip-stamp
		       'ps2-chip-stamp-compressed))))))))

(defun %ps1-chip-stamp-identify-instrument (fits-file)
  (cond ((typep fits-file 'cf:fits-file)
         (%ps1-chip-stamp-identify-instrument/ff fits-file))
        (t
         (cf:with-open-fits-file (fits-file ff)
           (%ps1-chip-stamp-identify-instrument/ff ff)))))





(%add-instrument-id-function '%ps1-chip-stamp-identify-instrument)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst %ps-stamp) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("FILTERID" "HIERARCH FPA.FILTERID" "OBSTYPE" "HIERARCH FPA.OBSTYPE"
      "MJD-OBS" "HIERARCH CELL.GAIN" "ZPT_OBS" "ZPT_ERR"
      "HIERARCH FPA.ZP")
    (call-next-method))
   :test 'equalp))

(defmethod is-reduced-for-instrument ((inst %ps-stamp) fits-file)
  (declare (ignorable  inst fits-file))
  :pan-starrs)

(defun %ps-get-standard-for-instrument (inst filter-keyword fits-file)
  (let ((filter (cf:read-fits-header
		  fits-file filter-keyword
		  :extension (get-image-extension-for-onechip-instrument
			      inst fits-file))))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not found in image ~A" fits-file))
    (cond ((string-utils:string-starts-with filter "g") :gps1)
	  ((string-utils:string-starts-with filter "r") :rps1)
	  ((string-utils:string-starts-with filter "i") :ips1)
	  ((string-utils:string-starts-with filter "z") :zps1)
	  ((string-utils:string-starts-with filter "y") :yps1)
	  ((string-utils:string-starts-with filter "w") :wps1) 
	  ;;;
	  (t NIL))))

(defmethod get-standard-filter-for-instrument ((inst %ps-chip-stamp) fits-file)
  (%ps-get-standard-for-instrument inst "FILTERID" fits-file))

(defmethod get-standard-filter-for-instrument ((inst %ps-warp-stamp) fits-file)
  (%ps-get-standard-for-instrument inst "HIERARCH FPA.FILTERID" fits-file))
  
  

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst ps1-chip-stamp) fits-file) ... )

(defmethod get-object-for-instrument ((inst %ps-stamp) fits-file)
  (%gethead-or-error
   fits-file "OBJECT"
   :extension (get-image-extension-for-onechip-instrument inst fits-file)))

(defmethod get-object-type-for-instrument ((inst %ps-chip-stamp) fits-file)
  (let ((obj-type
	  (%gethead-or-error
	   fits-file  "OBSTYPE"
	   :extension (get-image-extension-for-onechip-instrument
		       inst fits-file))))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "FLAT") :FLAT) ;; no idea if this is right
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))

(defmethod get-object-type-for-instrument ((inst %ps-warp-stamp) fits-file)
  (let ((obj-type
	  (%gethead-or-error
	   fits-file  "HIERARCH FPA.OBSTYPE"
	   :extension (get-image-extension-for-onechip-instrument
		       inst fits-file))))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "FLAT") :FLAT) ;; no idea if this is right
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))


(defmethod get-mjd-start-for-instrument ((inst %ps-stamp) fits-file)
  (%gethead-or-error
   fits-file "MJD-OBS"
   :extension (get-image-extension-for-onechip-instrument
	       inst fits-file)))

(defmethod get-exptime-for-instrument ((inst %ps-stamp) fits-file)
  (%gethead-or-error
   fits-file "EXPTIME"
   :extension (get-image-extension-for-onechip-instrument
	       inst fits-file)))

(defmethod get-gain-for-instrument ((inst %ps-chip-stamp) fits-file
				    &key extension)
  (declare (ignore extension))
  (%gethead-or-error
   fits-file "GAIN"
   :extension (get-image-extension-for-onechip-instrument
	       inst fits-file)))

(defmethod get-gain-for-instrument ((inst %ps-warp-stamp) fits-file
				    &key extension)
  (declare (ignore extension))
  (%gethead-or-error
   fits-file "HIERARCH CELL.GAIN"
   :extension (get-image-extension-for-onechip-instrument
	       inst fits-file)))



;; can't use naxis1,2 for compressed
(defmethod get-datasec-for-instrument ((inst ps1-chip-stamp-compressed)
				       (ff cf:fits-file)
				       &key extension)
  (declare (ignore extension))
  (cf:with-fits-extension (ff 2)
    (copy-seq (cf:fits-file-current-image-size ff))))

(defmethod get-datasec-for-instrument ((inst ps1-chip-stamp-compressed)
				       (fits-file string)
				       &key extension)
  (cf:with-open-fits-file (fits-file ff)
    (get-trimsec-for-instrument inst ff :extension extension)))


(defmethod get-datasec-for-instrument ((inst ps1-chip-stamp)
				       (fits-file string)
				       &key extension)
  (declare (ignore extension))
  (vector (cf:read-fits-header fits-file "NAXIS1" :extension 1)
	  (cf:read-fits-header fits-file "NAXIS2" :extension 1)))
  
 
;; generic version OK
;(defmethod get-mjd-mid-for-instrument ((inst ps1-chip-stamp) fits-file) ...  )

;(defmethod get-gain-for-instrument ((inst ps1-chip-stamp) fits-file)   ...)
 
;(defmethod get-trimsec-for-instrument ((inst ps1-chip-stamp) fits-file
;				       &key extension)
  
(defmethod get-chip-id-for-instrument  ((inst %ps-stamp) fits-file
					&key (extension nil))
  (declare (ignore extension))
  "NO_CHIP_DEFINED") ;; these aren't chips, just images.

(defmethod get-image-extension-for-onechip-instrument ((inst %ps-chip-stamp)
						       fits-file)
  (declare (ignore inst fits-file))
  1)

(defmethod get-image-extension-for-onechip-instrument ((inst %ps-chip-stamp-compressed)
						       fits-file)
  (declare (ignore inst fits-file))
  2)

(defmethod get-image-extension-for-onechip-instrument ((inst %ps-warp-stamp)
						       fits-file)
  (declare (ignore inst fits-file))
  1)

(defmethod get-image-extension-for-onechip-instrument ((inst %ps-warp-stamp-compressed)
						       fits-file)
  (declare (ignore inst fits-file))
  2)

(defmethod get-calibrated-zeropoint-for-instrument
    ((inst %ps-stamp)
     fits-file
     &key extension
       (flux-units :adu)
       (exposure   :exptime))
  (declare (type (member :adu :electrons) flux-units)
	   (type (member :one-second :exptime) exposure)
	   (ignore extension))
  (let* ((ext (get-image-extension-for-onechip-instrument inst fits-file))
	 ;; the zeropoint in the header is PER ADU (but gain is very close to 1)
	 (zp-adu-sec (or (cf:read-fits-header fits-file "ZPT_OBS" :extension ext)
			 (cf:read-fits-header fits-file "HIERARCH FPA.ZP" :extension ext)
			 (error "zeropoint headers ZPT_OBS or HIERARCH FPA.ZP not found")))
	 (zp-err     (or (cf:read-fits-header fits-file "ZPT_ERR" :extension ext)
			 0.01d0)) ;; this is a blind guess if not provided
	 (gain (get-gain-for-instrument inst fits-file))
	 (exptime (get-exptime-for-instrument inst fits-file))
	 (zp
	   (+ zp-adu-sec
	      ;;
	      (cond ((eq flux-units :adu)
		     0)
		    ((eq flux-units :electrons)
		     (* +2.5 (log gain 10)))
		    (t (error "Invalid flux-units ~A" flux-units)))
	      ;;
	      (cond ((eq exposure :one-second)
		     0)
		    ((eq exposure :exptime)
		     (* 2.5 (log exptime 10)))
		    (t (error "Invalid exposure ~A" exposure))))))
    ;;
    (values zp zp-err)))
  

;; PS1 should have WCS
;(defmethod get-initial-wcs-for-instrument ((inst ps1-chip-stamp) fits-file)  ...)
;(defmethod insert-initial-wcs-for-instrument ((inst ps1-chip-stamp) fits-file) ...)



