;; DFOSC on Danish 1.54 meter at La Silla
;;    see http://www.ls.eso.org/lasilla/Telescopes/2p2T/D1p5M/manual/

(in-package instrument-id)

;; parent class 
(defclass/inst dfosc-danish-154-eso (imaging-instrument onechip)
    ((name :initform "FOSC-DANISH-154-ESO") 
     (observatory :initform "lasilla")
     (aperture :initform 1.54)
     ;; NO GAIN KEYWORD - gain seems to be 3.3 for default amp B according to p16 of manual
     ;; manual says they are basically linear to truncatin limit
     (saturation-level :initform 65535)
     (non-linear-level :initform 65535)))
     
     


  

(defun dfosc-danish-154-eso-identify-instrument (fits-file)
  (when (and (equalp (cf:read-fits-header fits-file "INSTRUME") "DFOSC")
	     (equalp (cf:read-fits-header fits-file "TELESCOP") "Danish 1.54m"))
    (make-instance 'dfosc-danish-154-eso)))


(%add-instrument-id-function 'dfosc-danish-154-eso-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst dfosc-danish-154-eso)
						fits-file)
  (declare (ignore inst fits-file))
  (append
   '("HIERARCH ESO INS OPTI-2 NAME"
     "HIERARCH ESO GEN EXPO TYPE"
     "HIERARCH ESO DET FRAM CDELT1"
     "HIERARCH ESO DET FRAM CDELT2"
     "ROTATOR"
     )
   (call-next-method)))

(defmethod get-standard-filter-for-instrument ((inst dfosc-danish-154-eso) fits-file)
  (let ((filter (cf:read-fits-header fits-file "HIERARCH ESO INS OPTI-2 NAME")))
    (cond ((not filter) :undefined) ;; biases don't have a filter header
	  ((equalp filter "Bessel U") :uj)
	  ((equalp filter "Bessel B") :bj)
	  ((equalp filter "Bessel V") :vj)
	  ((equalp filter "Bessel R") :rc)
	  ;; the following are guesses from Table 2.2 of manual plus one example
	  ((equalp filter "gunn G") :gsdss)
	  ((equalp filter "gunn R") :rsdss)
	  ((equalp filter "gunn I") :isdss)
	  ((equalp filter "gunn Z") :zsdss)
	  ;;;
	  (t NIL))))


(defmethod get-exptime-for-instrument ((inst dfosc-danish-154-eso) fits-file)
  (declare (ignorable inst))
  (%gethead-or-error fits-file "EXPTIME"))

(defmethod get-object-for-instrument ((inst dfosc-danish-154-eso) fits-file)
  (declare (ignorable inst))
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst dfosc-danish-154-eso) fits-file)
  (let ((obj-type (%gethead-or-error  fits-file  "HIERARCH ESO GEN EXPO TYPE"))
	(object   (%gethead-or-error  fits-file  "OBJECT"))
	(exptime   (%gethead-or-error  fits-file  "EXPTIME")))
	(cond
	  ((and (equalp "BIAS" object)  (zerop exptime))
	   :BIAS)
	  ((or (search "FLAT" object :test 'equalp) ;; anything with OBJECT=*flat* is a flat
	       (search "FL"   obj-type :test 'equalp)) ;; this is a guess.
	   :FLAT) ;; unfortunately, a flat is obj-type=SCI
	  ((equalp obj-type "SCI") :OBJECT) ;; any other SCI is an object
	  (T :OTHER))))

;; sometimes MJD-START header is present, and sometimes we need to construct DATE-OBS + UT
(defmethod get-mjd-start-for-instrument ((inst dfosc-danish-154-eso) fits-file)
  (declare (ignorable inst))
  (or (cf:read-fits-header fits-file "MJD-OBS") ;; often this is present
      (let ((date-obs (cf:read-fits-header fits-file "DATE-OBS"))
	    (ut-obs   (cf:read-fits-header fits-file "UT")))
	(cond ((not (and (stringp date-obs) (stringp ut-obs)))
	       (error "MJD-OBS not present, and DATE-OBS + UT headers also not present"))
	      (t
	       (let ((ut-string (concatenate 'string (string-trim " " date-obs) "T" (string-trim " " ut-obs))))
		 (or 
		  (ignore-errors (astro-time:parse-ut-date-and-time-string-to-mjd ut-string))
		  (error "Could not parse date ~A" ut-string))))))))

(defmethod get-mjd-mid-for-instrument ((inst dfosc-danish-154-eso) fits-file)
  (+ (get-mjd-start-for-instrument inst fits-file)
     (* 0.5d0 (/ (%gethead-or-error fits-file "EXPTIME") 24d0 3600d0))))

;; see page 16 of manual - assumes default AMP B
(defmethod get-gain-for-instrument ((inst dfosc-danish-154-eso) fits-file &key extension)
  (declare (ignorable inst fits-file extension))
  3.3d0) 
	   
 
(defmethod get-datasec-for-instrument ((inst dfosc-danish-154-eso) fits-file &key extension)
  (declare (ignore extension inst))
  (let* ((bin1 (%gethead-or-error fits-file "HIERARCH ESO DET FRAM CDELT1"))
	 (bin2 (%gethead-or-error fits-file "HIERARCH ESO DET FRAM CDELT2")))
    (vector (floor 51 bin1) (floor 2098 bin1)
	    (floor 21 bin2) (floor 2052 bin2))))
	   

(defmethod get-chip-id-for-instrument ((inst dfosc-danish-154-eso) fits-file &key extension)
  (declare (ignorable inst fits-file extension))
  (call-next-method))


;; value taken from manual
(defmethod  get-pixel-scale-for-instrument ((inst dfosc-danish-154-eso) fits-file
					    &key extension)
  (declare (ignore extension inst))
  (let* ((bin1 (%gethead-or-error fits-file "HIERARCH ESO DET FRAM CDELT1"))
	 (bin2 (%gethead-or-error fits-file "HIERARCH ESO DET FRAM CDELT2")))
    (when (not (= bin1 bin2))
      (error "Pixel scale not defined because bin1=~A bin2=~A" bin1 bin2))
    (* 0.39d0 bin1)))


;; form page of manual, the default configution always has East left,
;; Y up, for both +=90 of rotangle.   The rotator angle is +-90 depending
;; on whether telescope is east/west of pier.
(defmethod get-initial-wcs-for-instrument ((inst  dfosc-danish-154-eso) fits-file
					   &key extension)
  (declare (ignore extension))
  (let* ((ra-raw (%gethead-or-error fits-file "RA"))
	 (dec-raw (%gethead-or-error fits-file "Dec"))
	 (ra (if (floatp ra-raw)
		 (* 15 ra-raw) ;; this is MADNESS - decimal hours
		 (ra-dec:hms-string->deg ra-raw)))
	 (dec (if (floatp dec-raw) dec-raw (ra-dec:dms-string->deg dec-raw)))
	 (bin1 (%gethead-or-error fits-file "HIERARCH ESO DET FRAM CDELT1"))
	 (bin2 (%gethead-or-error fits-file "HIERARCH ESO DET FRAM CDELT2"))
	 (secpix1 (* 0.39d0 bin1))
	 (secpix2 (* 0.39d0 bin2))
	 (naxis1 (%gethead-or-error fits-file "NAXIS1"))
	 (naxis2 (%gethead-or-error fits-file "NAXIS2"))
	 (epoch (or (cf:read-fits-header fits-file "EPOCH") 2000)) ;; epoch often missing?
	 (rotangle-raw  (%gethead-or-error fits-file "ROTATOR"))
	 (rotangle (cond ((realp rotangle-raw)
	  		  (* rotangle-raw 1d0))
	  		 ((stringp rotangle-raw)
	  		  (jk-parse-float:parse-float rotangle-raw))))
	 (s1 (*  secpix1  (/ 1d0 3600)))
	 (s2 (*  secpix2  (/ 1d0 3600))))

    (when (> (abs (- 90 (abs rotangle))) 3.0)
      (error "ROTATOR=~A is not close to either 90 or -90, so WCS will be bad" rotangle))

    (when (and (zerop ra) (zerop dec))
      (error "RA=0 and DEC=0.  Almost certainly, the coordinates are not in the headers, and WCS will be bad."))
    
    ;; convert to 2000 if necessary
    (when (not (= epoch 2000d0))
      (multiple-value-setq (ra dec)
	(precess-ra-dec-to-j2000 
	 epoch ra dec)))
			 		       
    (wcs:make-wcs-radec-tan
     :crval1 ra
     :crval2 dec
     :crpix1 (* 0.5d0 naxis1)
     :crpix2 (* 0.5d0 naxis2)
     :cd1_1  (- s1)
     :cd2_2  s2
     :cd1_2  0d0
     :cd2_1  0d0
     :equinox 2000d0)))
            


(defmethod insert-initial-wcs-for-instrument ((inst dfosc-danish-154-eso) fits-file
					       &key extension)
  (declare (ignore extension))
  (when (not (cf:read-wcs fits-file))
    (cf:write-wcs (get-initial-wcs-for-fits fits-file)
		  fits-file)))






