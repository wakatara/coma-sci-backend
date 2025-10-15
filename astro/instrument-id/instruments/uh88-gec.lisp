#|

Old GEC camera on UH88

Some info here: 

https://starbrite.jpl.nasa.gov/ds-view/pds/viewProfile.jsp?dsid=EAR-C-CCD-3-EDR-HALLEY-OUTBURST-UH-V1.0

(we find a pixel scale of 0.598 however)

    Instrumentation
    ===============
Date       Tel       Instrume  Pix Scale Gain Noise  Cond  Cal  Obs  Notes
                           (arcsec/pix) e-/ADU  e-
 
Feb 1991   UH 2.2m   GEC          0.5622  1.2   6    c1    self M,D
Apr 1991   UH 2.2m   TEK 1024     0.22    3.54  10   c          M
May 1991   UH 2.2m   TEK 1024             3.54  10   P      --  M
Jun 1991   UH 2.2m   TEK 1024     0.36    3.54  10   P      --  M
Jan 1992   UH 2.2m   TEK1024/WFGS 0.351   3.54  10   P      --  M
(WFGS is the focal reducer - actually spectrograph but only fr optics used)
D       David Wilson    Assist
M       Karen Meech

|#

(in-package instrument-id)


(defclass/inst uh88-gec (imaging-instrument onechip)
  ((name :initform "UH88-GEC") ;; a string
   (aperture :initform 1.22)
   (observatory :initform "MKO")))





(defun %uh88-gec-identify-instrument (fits-file)
  (let ((sensor (cf:read-fits-header fits-file "SENSOR"))
	(naxis1 (cf:read-fits-header fits-file "NAXIS1"))
	(naxis2 (cf:read-fits-header fits-file "NAXIS2")))
    (when (and (equal sensor "GEC")
	       (<= naxis1 420) ;; allow for trimming
	       (<= naxis2 580))
      (make-instance 'uh88-gec))))
	

(%add-instrument-id-function '%uh88-gec-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod get-critical-headers-for-instrument ((inst uh88-gec) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("SENSOR" "EXPOSURE" )
    (call-next-method))
   :test 'equalp))

(defmethod get-standard-filter-for-instrument ((inst uh88-gec) fits-file)
  (let ((filter (%gethead-or-error fits-file "FILTER")))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not inserted into UH88-GEC image ~A" fits-file))
    (cond ((equalp filter "U") :uj)
	  ((equalp filter "B") :bj)
	  ((equalp filter "V") :vj)
	  ((equalp filter "R") :rc)
	  ((equalp filter "I") :ic)
	  ;;;
	  (t NIL))))

;; exposures seem to be negative?!
(defmethod get-exptime-for-instrument ((inst uh88-gec) fits-file)
  (let ((exposure (%gethead-or-error fits-file "EXPOSURE")))
    (cond ((numberp exposure)
	   (abs (* 1d0 exposure))) ;; can be <0
	  ((string exposure)
	   (or (ignore-errors (abs (jk-parse-float:parse-float exposure)))
	       (error "Could not parse EXPOSURE=~A" exposure))))))
  

(defmethod get-object-for-instrument ((inst uh88-gec) fits-file)
  (let ((obj (%gethead-or-error fits-file "OBJECT")))
    (if (not (equal obj "Unknown"))
	obj
	nil)))

(defmethod get-object-type-for-instrument ((inst uh88-gec) fits-file)
  (let ((obj (%gethead-or-error  fits-file  "OBJECT")))
    (cond ((search obj "BIAS" :test 'equalp) :BIAS)
	  ((search obj "FLAT" :test 'equalp) :FLAT)
	  ((search obj "Unknown" :test 'equalp) nil)
	  (T :OBJECT))))



;; the problem is that the DATE-OBS is given as MM/DD/YY not DD/MM/YY -
;; is this always the case? sometimes?
;;  FIXME - assume universal use of M/D/Y?
(defmethod get-mjd-start-for-instrument ((inst uh88-gec) fits-file)
  (let* ((ut-string
	   (string-trim " " (%gethead-or-error fits-file "UT")))
	 (date-string-raw  
	   (string-trim " " (%gethead-or-error fits-file "DATE-OBS")))
	 (date-string 
	   (or (ignore-errors (astro-time:parse-mm/dd/yy-string
			       date-string-raw
			       :separator-char #\/
			       :output :string))
	       (error "Could not parse DATE-OBS=~A ias DD/MM/YY for uh88-gec image"
		      date-string-raw)))
	 (full-ut-string  (format nil "~AT~A" date-string ut-string))	 
	 (mjd-start 
	   (astro-time:parse-ut-date-and-time-string-to-mjd full-ut-string)))
    mjd-start))


(defmethod get-mjd-mid-for-instrument ((inst uh88-gec) fits-file)
  (+ (get-mjd-start-for-instrument inst fits-file)
     (* 0.5d0
	(/ (get-exptime-for-instrument inst fits-file) (* 24 3600)))))

(defmethod get-gain-for-instrument ((inst uh88-gec) fits-file  &key extension)
  (declare (ignorable inst fits-file extension))
  1.2d0) ;; from the PDS web site (see top of this file)
  

(defmethod get-pixel-scale-for-instrument ((inst uh88-gec) fits-file &key extension)
  (declare (ignorable inst fits-file extension))
  0.598d0) ;; out measured value, differing from the PDS web site (see top of this file)
           ;; which has 0.5622


(defmethod get-datasec-for-instrument ((inst uh88-gec) fits-file &key extension)
  (declare (ignore inst extension))
  #(21 405 1 576))


;; See if a WCS was inserted; if not, try an RA,DEC, either decimal degrees or xx:mm:ss
(defmethod get-initial-wcs-for-instrument ((inst uh88-gec) fits-file
					   &key extension)
  (declare (ignore extension))
  (or (cf:read-wcs fits-file) ;; if WCS was inserted use that
      ;; else try ra,dec as degrees or HH:MM:SS DD:MM:SS
      (let* ((ra-raw (cf:read-fits-header fits-file "RA"))
	     (dec-raw (cf:read-fits-header fits-file "DEC"))
	     (equinox (or (cf:read-fits-header fits-file "EQUINOX")
			  (cf:read-fits-header fits-file "EPOCH")))
			  
	     (ra (when ra-raw
		   (ignore-errors (if (floatp ra-raw)
				      ra-raw 
				      (ra-dec:hms-string->deg ra-raw)))))
	     (dec (when dec-raw
		    (ignore-errors (if (floatp dec-raw)
				       dec-raw
				       (ra-dec:dms-string->deg dec-raw))))))

	
	(cond ((not (and ra dec equinox))
	       nil) ;; no WCS
	      (t
	       (let ((pixel-scale (* 1/3600 (get-pixel-scale-for-instrument inst fits-file)))
		     (naxis1 (%gethead-or-error fits-file "NAXIS1"))
		     (naxis2 (%gethead-or-error fits-file "NAXIS2")))
		 ;; precess to J2000
		 (multiple-value-setq (ra dec)
		     (precess-ra-dec-to-j2000 equinox ra dec))
		 
		 (wcs:make-wcs-radec-tan
		  :crval1 ra
		  :crval2 dec
		  :crpix1 (* 0.5d0 naxis1)
		  :crpix2 (* 0.5d0 naxis2)
		  :cd1_1  0d0
		  :cd2_2  0d0
		  :cd1_2  (* -1 pixel-scale) 
		  :cd2_1  (* -1 pixel-scale) 
		  :equinox 2000d0))))))) 
		    
(defmethod insert-initial-wcs-for-instrument ((inst uh88-gec) fits-file &key extension)
  (declare (ignore extension))
  (when (not (cf:read-wcs fits-file))
    (let ((wcs (get-initial-wcs-for-instrument inst fits-file)))
      (when (not wcs)
	(error "No initial WCS to write"))
      (cf:write-wcs wcs fits-file))))
					      
      





