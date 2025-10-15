
;; WHT Auxiliary port camera
;; http://www.ing.iac.es/astronomy/instruments/acam/

(in-package instrument-id)

(defclass/inst wht-acam (imaging-instrument onechip)
    ((name :initform "wht-acam") ;; a string
     (observatory :initform "lapalma")
     ;; optional
     (saturation-level :initform 60000)
     (non-linear-level :initform 60000)
     (aperture  :initform 4.20)
     (gain-keyword :initform "GAIN")))


   

(defun %wht-acam-identify-instrument (fits-file)
  (when (and (equalp (cf:read-fits-header fits-file "INSTRUME"  :extension 1)
		     "ACAM")
	     (equalp (cf:read-fits-header fits-file "TELESCOP"  :extension 1)
		     "WHT"))
    (make-instance 'wht-acam)))

(%add-instrument-id-function '%wht-acam-identify-instrument)


(defmethod get-critical-headers-for-instrument ((inst wht-acam) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("INSTRUME" "TELESCOP" "ACAMWH1" "ACAMWH2" "ROTSKYPA" "RA" "DEC"
      "PV1_1" "PV1_2" "PV2_1" "PV2_2"
      "PROJP1" "PROJP2")
    (call-next-method))
   :test 'equalp))

(defmethod get-standard-filter-for-instrument ((inst wht-acam) fits-file) 
  (let* ((filt1 (cf:read-fits-header fits-file "ACAMWH1" :extension 1))
	 (filt2 (cf:read-fits-header fits-file "ACAMWH2" :extension 1)))
    (cond ((not (equalp filt1 "CLEAR"))
	   :unknown)
	  ((equalp filt2 "slog")  :gsdss) ;; a guess - might never have existed?
	  ((equalp filt2 "slog2") :gsdss) ;; current filter
	  ((equalp filt2 "slor")  :rsdss)
	  ((equalp filt2 "sloi")  :isdss) ;; a guess
	  ((equalp filt2 "SlnU")  :usdss) ;; seen in data
	  ((equalp filt2 "sloz")  :zsdss) ;; a guess
	  (t :unknown))))
	  



(defmethod get-exptime-for-instrument ((inst wht-acam) fits-file)
  (%gethead-or-error fits-file "EXPTIME"  :extension 1))

(defmethod get-object-for-instrument ((inst wht-acam) fits-file)
  (%gethead-or-error fits-file "OBJECT" :extension 1))

(defmethod get-object-type-for-instrument ((inst wht-acam) fits-file)
  (let ((imagetyp (%gethead-or-error fits-file "IMAGETYP" :extension 1)))
    (cond ((equalp imagetyp "zero") :bias)
	  ((equalp imagetyp "flat") :flat)
	  ((equalp imagetyp "object") :object)
	  (t :other))))
	  

(defmethod get-mjd-start-for-instrument ((inst wht-acam) fits-file)
  (%gethead-or-error fits-file "MJD-OBS" :extension 1))

(defmethod get-mjd-mid-for-instrument ((inst wht-acam) fits-file)
  (+ (get-mjd-start-for-instrument inst fits-file)
     (/ (get-exptime-for-instrument inst fits-file) 24d0 3600d0 2)))
  
(defmethod get-gain-for-instrument ((inst wht-acam) fits-file &key extension)
  (declare (ignore extension))
  (%gethead-or-error fits-file "GAIN" :extension 2))

(defmethod write-gain-for-instrument ((inst wht-acam) fits-file gain &key extension)
  (declare (ignore extension))
  (cf:write-fits-header fits-file "GAIN" gain :extension 2))

(defmethod get-chip-id-for-instrument ((inst wht-acam) fits-file &key extension)
  (declare (ignore extension))
  (call-next-method))

(defmethod get-trimsec-for-instrument ((inst wht-acam) fits-file &key extension)
  (declare (ignore extension))
  (call-next-method))

(defmethod get-statsec-for-instrument  ((inst wht-acam) fits-file &key extension)
  (declare (ignore extension))
  (vector 500 1500 650 1650))  ;; #(x1 x2 y1 y2), well inside vignetting circle

(defmethod get-initial-wcs-for-instrument ((inst wht-acam) fits-file &key extension)
  (declare (ignore extension))
  (let* ((rotangle (* (/ pi 180) (%gethead-or-error fits-file "ROTSKYPA" :extension 1)))
	 (naxis1 (%gethead-or-error fits-file "NAXIS1" :extension 2))
	 (naxis2 (%gethead-or-error fits-file "NAXIS2" :extension 2))
	 (cosrot (cos rotangle))
	 (sinrot (sin rotangle))
	 (rastring  (%gethead-or-error fits-file "RA" :extension 1))
	 (decstring (%gethead-or-error fits-file "DEC" :extension 1))
	 (equinox   (%gethead-or-error fits-file "EQUINOX" :extension 1))
	 (ra (ra-dec:hms-string->deg rastring))
	 (dec (ra-dec:dms-string->deg decstring))
	 (pixel-scale (/ 0.253 3600d0)))
    
    ;; convert to 2000 if necessary
    (when (and (plusp equinox) (not (= equinox 2000d0)))
      (multiple-value-setq (ra dec)
	(precess-ra-dec-to-j2000 equinox ra dec)))

    ;; http://www.ing.iac.es/astronomy/instruments/acam/imaging.html
    (wcs:make-wcs-radec-tan
     :crval1 ra
     :crval2 dec
     :crpix1 (* 0.5d0 naxis1)
     :crpix2 (* 0.46d0 naxis2) ;; center of circle is a bit below center of chip
     :cd1_1  (* pixel-scale +1 sinrot)
     :cd2_2  (* pixel-scale -1 sinrot)
     :cd1_2  (* pixel-scale +1 cosrot)
     :cd2_1  (* pixel-scale +1 cosrot)
     :equinox 2000d0)))

    
		   

(defmethod insert-initial-wcs-for-instrument ((inst wht-acam) fits-file
					      &key extension)
  (declare (ignore extension))
  (let ((wcs (get-initial-wcs-for-instrument inst fits-file)))
    (when (not wcs)
      (error "No initial WCS to write"))
    (cf:write-wcs wcs fits-file :extension 2)
    ;;
    ;; get rid of old headers
    (loop for header in '("PV1_1" "PV1_2" "PV2_1" "PV2_2"
			  "PROJP1" "PROJP2" )
	  for value = (cf:read-fits-header fits-file header :extension 2)
	  when value
	  do (cf:delete-fits-header fits-file header :extension 2)
	     (cf:write-fits-header fits-file (format nil "~A_OLD" header) value
				   :comment "Original value" :extension 2))))


(defmethod get-pixel-scale-for-instrument ((inst wht-acam) fits-file &key extension)
  (declare (ignore extension))
  0.253d0) 


;; only if onechip; delete otherwise
(defmethod get-image-extension-for-onechip-instrument  ((inst wht-acam) fits-file)
  2)


(defmethod test-if-image-at-extension-for-instrument
    ((inst wht-acam ) fits-file &key  extension)
  (declare (ignore extension))
  (call-next-method))
