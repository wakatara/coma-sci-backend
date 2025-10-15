#|

UH 8K Mosaic mounted on cfht and uh88

http://www.cfht.hawaii.edu/Instruments/Imaging/UH8k/Manual/manual.html



|#

(in-package instrument-id)

;; parent class of UH 8K Mosaic camera  (which seems to have had individual fits files)
(defclass/inst %uh-8k-mosaic (imaging-instrument onechip)
  ((name :initform "UH-8K-MOSAIC") ;; a string
   (aperture :initform 1.22)
   (observatory :initform "MKO")))

;; mounted on CFHT
(defclass/inst uh-8k-mosaic-cfht (%uh-8k-mosaic)
  ())

;; mounted on UH88
(defclass/inst uh-8k-mosaic-uh88 (%uh-8k-mosaic)
  ())

(defun %uh-8k-mosaic-identify-instrument (fits-file)
  (when (and (equalp (cf:read-fits-header fits-file "INSTRUME")
		     "UH 8K CCD Mosaic Camera"))
    (cond ((equalp (cf:read-fits-header fits-file "TELESCOP")
		   "CFHT 3.6m")
	   (let* ((inst  (make-instance 'uh-8k-mosaic-cfht))
		  (chipid (get-chip-id-for-instrument inst fits-file)))
	     (setf (chip-id inst) chipid)
	     inst))
	  ;;
	  ((equalp (cf:read-fits-header fits-file "TELESCOP")
		   "2.2m UH")
	   (let* ((inst  (make-instance 'uh-8k-mosaic-uh88))
		  (chipid (get-chip-id-for-instrument inst fits-file)))
	     (setf (chip-id inst) chipid)
	     inst)))))
	  

(%add-instrument-id-function '%uh-8k-mosaic-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; filter seems to be of form "1 I Int" or "0 R Int"
;; OR alternative form "Z (0)","R","I", "V"
(defmethod get-standard-filter-for-instrument ((inst %uh-8k-mosaic) fits-file)
  (let* ((filter-string (%gethead-or-error fits-file "FILTER"))
	 (filter
	   (cond 
	     ;; case of "Z(0)","R", etc
	     ((and (>= (length filter-string) 1)
		       (member (aref filter-string 0)
			       '(#\U #\B #\V #\R #\I #\Z)))
		  (string (aref filter-string 0)))
	     ;; case of 1 I Int, etc
	     ((and (>= (length filter-string) 3)
		   (search "Int" filter-string)
		   (member (aref filter-string 2)
			   '(#\U #\B #\V #\R #\I #\Z)))
	      (string (aref filter-string 2)))
	     ;;
	     (t
	      (error "Cannot parse UH88-8K filter name ~A" filter-string)))))
    
    (cond ((equal filter "U") :uj)  ;; TOTAL GUESS
	  ((equal filter "B") :bj)
	  ((equal filter "V") :vj)
	  ((equal filter "R") :rc)
	  ((equal filter "I") :ic)
	  ((equal filter "Z") :z)
	  ;;;
	  (t NIL))))

(defmethod get-critical-headers-for-instrument ((inst %uh-8k-mosaic) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("TELESCOP" "INSTRUME" "IMAGETYP" "CHIPID" "SECPIX1" "SECPIX2" "RA" "DEC"
      "CHIPID" "ROTANGLE")
    (call-next-method))
   :test 'equalp))
  
;; generic version OK
;(defmethod get-exptime-for-instrument ((inst %uh-8k-mosaic) fits-file) ... )

(defmethod get-object-for-instrument ((inst %uh-8k-mosaic) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst %uh-8k-mosaic) fits-file)
  (let ((obj-type (%gethead-or-error  fits-file  "IMAGETYP")))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "FLATFIELD") :FLAT)
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))

;; in the past uh88 had a date string like DD/MM/YY
;; and we need to fix this to be YYYY-MM-DD - not sure if necessary for
;; 8K as well, but it's here just in case
(defun %fix-old-8kmosaic-date-string (dstring)
  (flet ((failparse ()
	   (error
	    "Failed to parse DATE-OBS=~A for old-style DD/MM/YY UH88 date"
	    dstring)))
    (cond ((not (find #\/ dstring))
	   dstring) ;; string is OK
	  ;;
	  (t
	   (let*
	       ((nsl1 (position #\/ dstring))
		(nsl2  (or (position #\/ dstring :start (1+ nsl1))
			   (failparse)))
		(day
		  (ignore-errors (parse-integer dstring :start 0 :end nsl1)))
		(month
		  (ignore-errors (parse-integer dstring
						:start (1+ nsl1) :end nsl2)))
		(yr
		  (ignore-errors (parse-integer dstring :start (1+ nsl2) )))
		;;
		(fullyr
		  (when yr  (cond ((> yr 1900) yr) ;; turn YY into YYYY
				 ((> yr 50) (+ yr 1900))
				 ((<= yr 50) (+ yr 2000))))))
	     ;;
	     (when (not
		    (and day month fullyr
			 (<= 1900 fullyr 2100)
			 (<= 1 month 12)
			 (<= 1 day 31)))
	       (failparse))
	     ;;
	     ;; generate properly formatted date
	     (format nil "~D-~2,'0D-~2,'0D" fullyr month day))))))

(defmethod get-mjd-start-for-instrument ((inst %uh-8k-mosaic) fits-file)
  ;; some old versions have JD
  (let ((jd (cf:read-fits-header fits-file "JD")))
    (cond ((and jd (floatp jd))
	   (astro-time:jd-to-mjd jd))
	  (t ;; fall back on UT,DATE-OBS format
	   (let* ((ut-string (%gethead-or-error fits-file "UT"))
		  (date-string-raw  (%gethead-or-error fits-file "DATE-OBS"))
		  (date-string (%fix-old-8kmosaic-date-string date-string-raw))
		  (full-ut-string  (format nil "~AT~A" date-string ut-string))	 
		  (mjd-start 
		    (multiple-value-bind (year month day hour min sec)
			(astro-time:parse-ut-date-and-time-string full-ut-string)
		      (astro-time:calendar-date-to-mjd year month day hour min sec))))
	     mjd-start)))))



;; generic version OK
;(defmethod get-mjd-mid-for-instrument ((inst %uh-8k-mosaic) fits-file) ...  )

;(defmethod get-gain-for-instrument ((inst %uh-8k-mosaic) fits-file)   ...)

;; this is the real TRIM-sec, but DATASEC is 
(defmethod get-trimsec-for-instrument ((inst %uh-8k-mosaic) fits-file &key extension)
  (declare (ignore extension))
  (vector 5 2048 1 2068))

(defmethod get-datasec-for-instrument ((inst %uh-8k-mosaic) fits-file &key extension)
  (declare (ignore extension))
  (vector 1 2048 1 2068))

(defmethod get-chip-id-for-instrument ((inst %uh-8k-mosaic) fits-file &key (extension))
  (declare (ignore extension))
  (%gethead-or-error fits-file "CHIPID"))

(defmethod get-pixel-scale-for-instrument  ((inst %uh-8k-mosaic)
					    fits-file &key extension)
  (declare (ignore inst extension))
  (let ((s1 (%gethead-or-error fits-file "SECPIX1"))
	(s2 (%gethead-or-error fits-file "SECPIX2")))
    (when (not (= s1 s2))
      (error "Unequal pixel scales SECPIX1=~A  SEXPIX2=~A IN ~A" s1 s2 fits-file))
    s1))

(defmethod get-pixel-scale-for-instrument  ((inst uh-8k-mosaic-uh88)
					    fits-file &key extension)
  (declare (ignore inst extension))
  (let ((s1 (%gethead-or-error fits-file "SECPIX1"))
	(s2 (%gethead-or-error fits-file "SECPIX2")))
    (when (not (= s1 s2))
      (error "Unequal pixel scales SECPIX1=~A  SEXPIX2=~A IN ~A" s1 s2 fits-file))
    (* 1.04 ;; headers seem a little wrong?
       s1)))

(defmethod get-initial-wcs-for-instrument ((inst %uh-8k-mosaic) fits-file
					   &key extension)
  (declare (ignore extension))
  (or nil ;; no initial WCS, ever?
      (let* ((rastr (%gethead-or-error fits-file "RA"))
	     (decstr (%gethead-or-error fits-file "Dec"))
	     ;; the pixel scale in header seems a little wrong for UH88
	     (pixscale-adj (cond ((typep inst 'uh-8k-mosaic-uh88)
				  1.04)
				 (t
				  1.0)))
	     (secpix1 (* pixscale-adj (%gethead-or-error fits-file "SECPIX2")))
	     (secpix2 (* pixscale-adj (%gethead-or-error fits-file "SECPIX2")))
	     (ra (ra-dec:hms-string->deg rastr))
	     (dec (ra-dec:dms-string->deg decstr))
	     (naxis1 (%gethead-or-error fits-file "NAXIS1"))
	     (naxis2 (%gethead-or-error fits-file "NAXIS2"))
	     (epoch (%gethead-or-error fits-file "EPOCH"))
	     ;;(rotangle  (%gethead-or-error fits-file "ROTANGLE"))
	     (rotangle 0d0) ;; it looks like angle of array is zero, despite keyword
	     (cosrot (cos (* (/ pi 180) rotangle)))
	     (sinrot (sin (* (/ pi 180) rotangle)))
	     (ccdnum (%gethead-or-error fits-file "CHIPID"))
	     ;; offset is where the center is, in array width units from
	     ;;  chip center
     ;; see 
     ;; http://www.cfht.hawaii.edu/Instruments/Imaging/UH8k/Manual/manual.html
	     ;;
	     ;; note that the chips have different orientations
	     ;;   |--*|--*|--*|--*|     where * is 0,0
             ;;   |   |   |   |   |
             ;;   | 2 | 3 | 5 | 4 |
             ;;   |   |   |   |   |
             ;;   |---|---|---|---|
             ;;   |   |   |   |   |
             ;;   | 0 | 1 | 7 | 6 |
             ;;   |   |   |   |   |
             ;;   |*--|*--|*--|*--|
	     (offset (cond ((= ccdnum 0) '(+1.5 +0.5))
			   ((= ccdnum 1) '(+0.5 +0.5))
			   ((= ccdnum 2) '(-1.5 +0.5))
			   ((= ccdnum 3) '(-0.5 +0.5))
			   ((= ccdnum 4) '(+1.5 +0.5))
			   ((= ccdnum 5) '(+0.5 +0.5))
			   ((= ccdnum 6) '(-1.5 +0.5))
			   ((= ccdnum 7) '(-0.5 +0.5))))
	     (xoff (* naxis1 (first  offset)))
	     (yoff (* naxis2 (second offset)))
	     (chip-flip (if (member ccdnum '(0 1 7 6)) +1 -1)) ;; flips of chips
	     ;; seems to be a reflection of y axis for uh88 version -
	     ;;     is there a right angle mirror in path?
	     (tele-flip1  1)
	     (tele-flip2  (cond ((typep inst 'uh-8k-mosaic-uh88) -1)
				(t      	                 +1)))
	     ;;
	     (s1 (* +1 tele-flip1 chip-flip secpix1  (/ 1d0 3600)))
	     (s2 (* -1 tele-flip2 chip-flip secpix2  (/ 1d0 3600))))

	
	;; convert to 2000 if necessary
	(when (not (= epoch 2000d0))
	  (multiple-value-setq (ra dec)
	    (precess-ra-dec-to-j2000 epoch ra dec)))
			 		       
	(wcs:make-wcs-radec-tan
	 :crval1 ra
	 :crval2 dec
	 :crpix1 (+ xoff (* 0.5d0 naxis1))
	 :crpix2 (+ yoff (* 0.5d0 naxis2))
	 :cd1_1  (*    cosrot s1) 
	 :cd2_2  (*    cosrot s2)  
	 :cd1_2  (* +1 sinrot s2)
	 :cd2_1  (* -1 sinrot s1)
	 :equinox 2000d0))))
      


(defmethod insert-initial-wcs-for-instrument ((inst %uh-8k-mosaic) fits-file
					       &key extension)
  (declare (ignore extension))
  (when (not (cf:read-wcs fits-file))
    (cf:write-wcs (get-initial-wcs-for-fits fits-file)
		  fits-file)))



