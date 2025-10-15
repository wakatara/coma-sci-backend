
#|


The modern cameras are pretty clear.


====

The 'old' cameras have heinously sparse headers. 

We don't understand why some are rotated.

The different classes may be 

 tek1024 
 tek20148 
 lorall2048 (no info)
 tek1024/lfw
 tek2048/lfw
 lorall2048/lfw (no info)







|#

(in-package instrument-id)


;; parent class of UH
(defclass/inst %uh88-camera (imaging-instrument onechip)
    ((name :initform "UH88-CAMERA parent class") ;; a string
     (observatory :initform "MKO")
     (aperture :initform 1.22)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a set of 2k x 2k cameras, apparently all similar
(defclass/inst %uh88-tek (%uh88-camera)
    ((name :initform "UH88-TEK parent class") 
     (observatory :initform "MKO")))

(defclass/inst uh88-tek-tek (%uh88-tek)
    ((name :initform "UH88-TEK - plain TEK") 
     (observatory :initform "MKO")))

(defclass/inst uh88-tek-tec (%uh88-tek) ;; we ASSUME this is a TEK, but it prob. doesnt matter
    ((name :initform "UH88-TEK - TEC") 
     (observatory :initform "MKO")))

(defclass/inst uh88-tek/lfw (%uh88-tek)
    ((name :initform "UH88-TEK - TEK/LFW") 
     (observatory :initform "MKO")))

(defclass/inst uh88-tek2048/lfw (%uh88-tek)
    ((name :initform "UH88-TEK - tek2048/lfw") 
     (observatory :initform "MKO")))

(defclass/inst uh88-tektronix-2048-ccd (%uh88-tek)
    ((name :initform "UH88-TEK - tektronix-2048-ccd") 
     (observatory :initform "MKO")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; an older 1k x 2k instrument
(defclass/inst uh88-LL-Hi-Z-7-5-2-CCD (%uh88-camera)
  ((name :initform "UH88 1kx2k imager LL Hi-Z 7-5-2 CCD")))

;; another older but well annotated instrument
(defclass/inst uh88-orbit-CCD (%uh88-camera)
  ((name :initform "UH88 2kx2k imager 'UH Orbit CCD'")))

;; another older but well annotated instrument, Infra-Red
;; http://www.ifa.hawaii.edu/instrumentation/quirc/quirc.html
(defclass/inst uh88-quirc (%uh88-camera)
  ((name :initform "UH88 QUIRC IR camera, 1k Rockwell SG3 detector")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parent class of olde UH88 cameras
(defclass/inst %uh88-old-camera (%uh88-camera)
  ((name :initform "UH88 old camera parent class")))

(defclass/inst uh88-old-unknown (%uh88-camera)
  ((name :initform "an old uh88 camera that we can't classify")))

(defclass/inst %uh88-old-tek (%uh88-old-camera)
  ((name :initform "UH88 old camera parent class")))

;; the following classes are suggested by
;;   https://web.archive.org/web/20020728104656/http://ccd.ifa.hawaii.edu:80/uh88/ccd.php
(defclass/inst uh88-old-tek-2048 (%uh88-old-tek)
  ((name :initform "Old TEK2048 UH88 camera")))

(defclass/inst uh88-old-tek-1024 (%uh88-old-tek)
  ((name :initform "Old TEK1024 UH88 camera")))

(defclass/inst uh88-old-tek-2048 (%uh88-old-tek)
  ((name :initform "Old TEK2048 UH88 camera")))

;; not sure how to ID this one or un-confuse from tek2048
(defclass/inst uh88-old-loral-2048 (%uh88-old-tek) 
  ((name :initform "Old TEK2048 UH88 camera")))

(defclass/inst %uh88-old-tek/wfgs (%uh88-old-tek)
  ((name :initform "UH88 old camera parent class with WFGS")))

;; the same cameras but with the focal reducer (0.35 vs 0.22 pixel scale)
(defclass/inst uh88-old-tek-1024/wfgs (%uh88-old-tek/wfgs)
  ((name :initform "Old TEK1024 UH88 camera with WFGS focal reducer")))

(defclass/inst uh88-old-tek-2048/wfgs (%uh88-old-tek/wfgs)
  ((name :initform "Old TEK2048 UH88 camera with WFGS focal reducer")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a very old 1100x1080 instrument - seems to have different rotation?
;(defclass/inst %uh88-very-old-camera (%uh88-old-camera)
;  ((name :initform "UH88 very old camera parent class with sub-binnings")))




;(defclass/inst uh88-very-old-1100x1080 (%uh88-very-old-camera)
;   ((name :initform "UH88 very old camera 1100x1080")))
;(defclass/inst uh88-very-old-2100x2080 (%uh88-very-old-camera)
;  ((name :initform "UH88 very old camera 2100x2080")))
;(defclass/inst uh88-very-old-512x512 (%uh88-very-old-camera)
;  ((name :initform "UH88 very old camera 512x512")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the next three might be same instrument
;(defclass/inst uh88-old-1150x1100 (%uh88-old-camera)
;   ((name :initform "UH88 old camera 1150x1100")))
;(defclass/inst uh88-old-1100x1100 (%uh88-old-camera)
;  ((name :initform "UH88 old camera 1100x1100")))
;(defclass/inst uh88-old-1k-0.22arcsec (%uh88-old-camera)
;  ((name :initform "UH88 old camera, 1kx1k, 0.22 arcsec")))
;; this one might be with a wider field filter setup (and lenses)
;;(defclass/inst uh88-old-1k-0.36arcsec (%uh88-old-camera)
;;  ((name :initform "UH88 old camera, 1kx1k, 0.36 arcsec")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %uh88-camera-identify-instrument (fits-file)
  (let* ((telescope (cf:read-fits-header fits-file "TELESCOP"))
	 (naxis1 (cf:read-fits-header fits-file "NAXIS1"))
	 (naxis2 (cf:read-fits-header fits-file "NAXIS2"))
	 (%instrument (cf:read-fits-header fits-file "INSTRUME"))
	 (instrument (if %instrument (string-trim " " %instrument)))
	 ;; if RA is not present then this might be a very old header
	 (ra (cf:read-fits-header fits-file "RA"))
	 (xra (cf:read-fits-header fits-file "XRA")) ;; early Xperimental hdr

	 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 ;; calculate the binning as robustly as possible in case we need it
	 (colbin (cf:read-fits-header fits-file "COLBIN"))
	 (rowbin (cf:read-fits-header fits-file "ROWBIN"))
	 (ccdsum-string (cf:read-fits-header fits-file "CCDSUM"));; rarely present 
	 (binvec (parse-ccdsum ccdsum-string :return :vector))
	 (b1 (or colbin (if binvec (aref binvec 0)))) ;; think this is right
	 (b2 (or rowbin (if binvec (aref binvec 1))))
	 (bin (if (and b1 b2 (= b1 b2)) b1)) ;; the binning we want
	 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 ;;
	 ;; FIXME - is this right?  is FULLROWS the number even when we don't
	 ;; use entire chip?
	 (fullrows (cf:read-fits-header fits-file "FULLROWS"))
	 ;;(fullcols (cf:read-fits-header "FULLCOLS"))
	 ;; this is our estimate of the initial CCD size
	 (ccdsize (when (integerp fullrows) fullrows))
	 ;; make sure that ccdsize isn't ridiculous
	 (ccdsize-ok (and ccdsize (< (max naxis1 naxis2) (+ 200 ccdsize))))
	 ;; frequently present, but not always
	 (secppix (cf:read-fits-header fits-file "SECPPIX"))
	 ;; some headers common to UH88
	 (ccdpicno (cf:read-fits-header fits-file "CCDPICNO"))
	 (cfver (cf:read-fits-header fits-file "CFVER"))    
	 (exposure (cf:read-fits-header fits-file "EXPOSURE"));; old exptime
	 (untm-obs (cf:read-fits-header fits-file "UNTM-OBS"));; UNIX sec
	 ;; this should guarantee (?) that this is an old UH88 camera
	 (is-old-uh88 (and ccdpicno
			   (if ccdsize ccdsize-ok t) ;; ccdsize OK if computable
			   cfver exposure untm-obs)))

    
    (declare (ignorable bin)) ;; don't use this (yet?)
 
    

    (flet ((approx= (x y)
	     (and (floatp x)
		  (floatp y)
		  (< (abs (- x y)) 0.01))))
	  
      (cond ((not (and (integerp naxis1) (integerp naxis2)))
	      nil)  ;; not a first image header
	     ;;
	     ((and (search "UH" telescope :test 'equal)
		   (search "2.2" telescope :test 'equal))
	     (cond
	       ;;
	       ((equalp instrument "TEK")
		(make-instance 'uh88-tek-tek))
	       ;;
	       ((equalp instrument "TEC")
		(make-instance 'uh88-tek-tec))
	       ;;
	       ((equalp instrument "TEK/LFW")
		(make-instance 'uh88-tek/lfw))
	       ;;
	       ;; this is a search because this inst often has junk
	       ;; attached to the instrument name
	       ((search "TEK2048/LFW" instrument :test 'equalp)
		(make-instance 'uh88-tek2048/lfw))
	       ;;
	       ((equalp instrument "UH Tektronix 2048 CCD")
		(make-instance 'uh88-tektronix-2048-ccd))
	       ;;
	       ((equalp instrument "LL Hi-Z 7-5-2 CCD")
		(make-instance 'uh88-ll-hi-z-7-5-2-ccd))
	       ;;
	       ((equalp instrument "UH Orbit CCD")
		(make-instance 'uh88-orbit-ccd))
	      ;;
	       ((equalp instrument "QUIRC")
		(make-instance 'uh88-quirc))
	       ))


	     ;; FIXME - could put date checks here
	     
	     ;;
	     ;; old instruments that are harder to ID
	     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	     ;; the next 3 are the same instrument, but they seem
	     ;; to have SAME SECPPIX, so they must be sub-reads
	     ;; data dir with all examples at once is:
	     ;; /Volumes/PDS/CDROMS/VALIDATED/UH_01-93_N123_RAW/uhjan93n3raw
	     ((and (not instrument) ;; some headers AREN'T here
		   (not telescope)
		   is-old-uh88)
	      (cond
		;;
		((or ;;
		  (and (eql ccdsize 1024) ;; old headers have no SECPPIX or RA
		       (or (and (not secppix) (not ra) xra) ;; old header 
			   (approx= secppix 0.219)))
		  ;; allow for a really old instrument with no ability
		  ;; to compute original ccdsize, but with a SECPPIX header
		  (and (not ccdsize)
		       (approx= secppix 0.22)))
		 (make-instance 'uh88-old-tek-1024))
		;;
		((and (eql ccdsize 2048)
		      (or (and (not secppix) (not ra) xra) ;; old header
			  (approx= secppix 0.219)))
		 (make-instance 'uh88-old-tek-2048))
		;;
		((and (eql ccdsize 1024)
		      (approx= secppix 0.351))
		 (make-instance 'uh88-old-tek-1024/wfgs))
		;;
		((and (eql ccdsize 2048)
		      (approx= secppix 0.351))
		 (make-instance 'uh88-old-tek-2048/wfgs))
		;;
		(t
		 (make-instance 'uh88-old-unknown))))))))

	

(%add-instrument-id-function '%uh88-camera-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst %uh88-camera) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("INSTRUME" "RA" "XRA" "DEC" "XDEC" "COLBIN" "ROWBIN" "FULLROWS"
      "FULLCOLS" "SECPPIX" "CCDPICNO" "CFVER" "UNTM-OBS" "EXPOSURE"
      "FILTERS" "IMAGETYP" "TIME-OBS" "UT" "UNTM-OBS" "CCDBIN1" "CCDBIN2"
      "SECPIX1" "SECPIX2" "SECPIX" "CCDBIN1" "CCDBIN2" "XEQUINOX")
    (call-next-method))
   :test 'equalp))

(defmethod get-standard-filter-for-instrument ((inst %uh88-camera) fits-file)
  (let ((filter (%gethead-or-error fits-file "FILTER")))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not found in image ~A" fits-file))
    (cond ((equalp filter "U") :uj)
	  ((equalp filter "B") :bj)
	  ((equalp filter "V") :vj)
	  ((equalp filter "R") :rc)
	  ((equalp filter "I") :ic)
	  ((equalp filter "open") :open-uh88tek)
	  ;;;
	  (t NIL))))

;; sometimes the OLD cameras have FILTERS and not FILTER
(defmethod get-standard-filter-for-instrument
    ((inst %uh88-old-camera) fits-file)
  (let* ((%filter (cf:read-fits-header fits-file "FILTER"))
	 (%filters (cf:read-fits-header  fits-file "FILTERS"))
	 ;; try FILTER first then FILTERS
	 (filter (cond
		   ((and (stringp %filter) (>= (length %filter) 1))
		    %filter)
		   (t
		    %filters))))
    ;;
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "Valid FILTER or FILTERS keyword not found in image ~A" fits-file))
    ;;
    (cond ((equalp filter "U") :uj)
	  ((equalp filter "B") :bj)
	  ((equalp filter "V") :vj)
	  ((equalp filter "R") :rc)
	  ((equalp filter "I") :ic)
	  ;;;
	  (t NIL))))


(defmethod get-standard-filter-for-instrument ((inst uh88-quirc) fits-file)
  (let ((filter (%gethead-or-error fits-file "FILTER")))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not found in image ~A" fits-file))
    (cond ((equalp filter "Jn") :j)
	  ((equalp filter "Hn") :h)
	  ((equalp filter "Kn") :k)
	  ((equalp filter "Dark") :dark)
	  ;;;
	  (t NIL))))



(defmethod get-exptime-for-instrument ((inst %uh88-camera) fits-file)
  (declare (ignorable inst))
  (or (cf:read-fits-header fits-file "EXPOSURE") ;;  old cameras
      (cf:read-fits-header fits-file "EXPTIME")))

(defmethod get-object-for-instrument ((inst %uh88-camera) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst %uh88-camera) fits-file)
  (let ((imagetyp (%gethead-or-error  fits-file  "IMAGETYP")))
    (cond ((equalp imagetyp "BIAS") :BIAS)
	  ((equalp imagetyp "FLATFIELD") :FLAT)
	  ((equalp imagetyp "OBJECT") :OBJECT)
	  (T :OTHER))))

;; for some  OLD-TEK there is no IMAGETYP
(defmethod get-object-type-for-instrument ((inst %uh88-old-tek) fits-file)
  (let* ((imagetyp (cf:read-fits-header  fits-file  "IMAGETYP"))
	 (object (when (not imagetyp)
		   (cf:read-fits-header  fits-file  "OBJECT"))))
    (if imagetyp
	(cond ((equalp imagetyp "BIAS") :BIAS)
	      ((equalp imagetyp "FLATFIELD") :FLAT)
	      ((equalp imagetyp "DARK") :FLAT)
	      ((equalp imagetyp "OBJECT") :OBJECT)
	      (T :OTHER))
	(cond ((search "bias" object :test 'equalp) :BIAS)
	      ((search "flat" object :test 'equalp) :FLAT)
	      ((search "dark" object :test 'equalp)  :DARK) ;; just in case
	      ((search "focus" object :test 'equalp)  :FOCUS) ;; just in case
	      (T :OBJECT)))))



;; in the past uh88 had a date string like DD/MM/YY
;; and we need to fix this to be YYYY-MM-DD
(defun %fix-old-uh88-date-string (dstring)
  (cond
    ;; 31/01/98 or 31-01-98
    ((and (<= (length dstring) 8))
     (astro-time:parse-dd/mm/yy-string
      dstring :output :string
	      :separator-char  (cond ((= (count #\- dstring) 2) #\-)
				     ((= (count #\/ dstring) 2) #\/)
				     (t nil))))
    ;; assume a normal date string 1998-01-31
    ((= (length dstring) 10)
     (astro-time:parse-yy/mm/dd-string
      dstring :output :string
	      :separator-char (cond ((= (count #\- dstring) 2) #\-)
				    ((= (count #\/ dstring) 2) #\/)
				    (t nil))))))
				    
				     
				       


(defmethod get-mjd-start-for-instrument ((inst %uh88-camera) fits-file)
  ;; use the unix time if we have it, because the DATE-OBS is sometimes
  ;; screwed up (it is MM/DD/YY when it claims DD/MM/YY)
  (let ((ut-sec (cf:read-fits-header fits-file "UNTM-OBS")))
    ;; if possible, just use UNIX seconds
    (if ut-sec
	(astro-time:ut-to-mjd
	 (astro-time:unix-time-to-lisp-ut ut-sec))
	;; else do the whole parsing of (UT or TIME-OBS) plus DATE-OBS
	(let* ((ut-string
		 (string-trim
		  " "
		  (or (cf:read-fits-header fits-file "UT")
		      ;; some early versions have this, when reduced?
		      (cf:read-fits-header fits-file "TIME-OBS")
		      (error
		       "Neither UT or TIME-OBS header for HH:MM:SS of obs time"))))
	       (date-string-raw  
		 (string-trim " " (%gethead-or-error fits-file "DATE-OBS")))
	       (date-string (%fix-old-uh88-date-string date-string-raw))
	       (full-ut-string  (format nil "~AT~A" date-string ut-string))	 
	       (mjd-start 
		 (multiple-value-bind (year month day hour min sec)
		     (astro-time:parse-ut-date-and-time-string full-ut-string)
		   (astro-time:calendar-date-to-mjd year month day hour min sec))))
	  mjd-start))))
      
      


;; generic version OK
;(defmethod get-mjd-mid-for-instrument ((inst %uh88-camera) fits-file) ...  )

(defmethod get-gain-for-instrument ((inst %uh88-old-camera)
				    fits-file &key extension)
  (declare (ignore inst  extension))
  ;; it won't have GAIN by default, and will return NIL
  (cf:read-fits-header fits-file "GAIN")) 


;; from https://starbrite.jpl.nasa.gov/ds-view/pds/viewProfile.jsp?dsid=EAR-C-CCD-3-EDR-HALLEY-OUTBURST-UH-V1.0
(defmethod get-gain-for-instrument ((inst %uh88-old-tek)
				    fits-file &key extension)
  (declare (ignore inst extension))
  (or
   (cf:read-fits-header fits-file "GAIN")
   3.54))
   

;; the SECPIX1,2 headers are WRONG for uh88-orbit-ccd - they say 0.14
(defmethod get-pixel-scale-for-instrument ((inst uh88-orbit-ccd)
					    fits-file &key extension)
  (declare (ignore inst extension))
  (* 0.220d0
     (%gethead-or-error fits-file "CCDBIN1")))
	

(defmethod get-pixel-scale-for-instrument ((inst uh88-quirc)
					    fits-file &key extension)
  (declare (ignore inst extension))
  (let ((s1 (%gethead-or-error fits-file "SECPIX1"))	
	(s2 (%gethead-or-error fits-file "SECPIX2")))
    (if (and (floatp s1) (floatp s2) (eql s1 s2))
      s1
      (error "SECPIX1=~A and SECPIX2=~A are not defined and equal" s1 s2))))


(defmethod get-pixel-scale-for-instrument ((inst %uh88-old-tek) 
					   fits-file &key extension)
  (declare (ignore inst extension))
  (or (cf:read-fits-header fits-file "SECPPIX")
      0.22d0))

;; https://starbrite.jpl.nasa.gov/ds-view/pds/viewProfile.jsp?dsid=EAR-C-CCD-3-EDR-HALLEY-OUTBURST-UH-V1.0
(defmethod get-pixel-scale-for-instrument ((inst %uh88-old-tek/wfgs) 
					   fits-file &key extension)
  (declare (ignore inst extension))
  (or (cf:read-fits-header fits-file "SECPPIX")
      0.351d0))

	    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the real TRIM-sec, but DATASEC is 
(defmethod get-trimsec-for-instrument ((inst %uh88-tek)
				       fits-file &key extension)
  (declare (ignore extension))
  (vector 5 2048 1 2068))

(defmethod get-datasec-for-instrument ((inst %uh88-tek)
				       fits-file &key extension)
  (declare (ignore extension))
  (vector 1 2048 1 2068))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-trimsec-for-instrument ((inst uh88-ll-hi-z-7-5-2-ccd)
				       fits-file &key extension)
  (declare (ignore extension))
  (vector 3 1023 4 2048))

(defmethod get-datasec-for-instrument ((inst uh88-ll-hi-z-7-5-2-ccd)
				       fits-file &key extension)
  (declare (ignore extension))
  (vector 1 1024 1 2068))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod get-initial-wcs-for-instrument ((inst %uh88-camera) fits-file
					   &key extension)
  (declare (ignore extension))
  (or (cf:read-wcs fits-file) ;; the old version
      (let* ((rastr (%gethead-or-error fits-file "RA"))
	     (decstr (%gethead-or-error fits-file "Dec"))
	     (naxis1 (%gethead-or-error fits-file "NAXIS1"))
	     (naxis2 (%gethead-or-error fits-file "NAXIS2"))
	     (bin1 (cf:read-fits-header fits-file "CCDBIN1"))
             (bin2 (cf:read-fits-header fits-file "CCDBIN2"))
	     (ra (ra-dec:hms-string->deg rastr))
 	     (dec (ra-dec:dms-string->deg decstr))
	     (epoch (%gethead-or-error fits-file "EPOCH")))

	
	;; convert to 2000 if necessary
	(when (and (plusp epoch) (not (= epoch 2000d0)))
	  (multiple-value-setq (ra dec)
	    (precess-ra-dec-to-j2000 epoch ra dec)))

	;; QUIRC doesn't define epoch - pray it is 2000
	(when (zerop epoch) (setf epoch 2000d0))
	
	(cond ((typep inst '%uh88-tek)
	       (when (not (and bin1 bin2))
		 (error "CCDBIN1 and CCDBIN2 not defined/floats"))
	       (wcs:make-wcs-radec-tan
		:crval1 ra
		:crval2 dec
		:crpix1 (* 0.5d0 naxis1)
		:crpix2 (* 0.5d0 naxis2)
		:cd1_1  (* +6.111d-5 bin1) 
		:cd2_2  (* -6.111d-5 bin2)
		:cd1_2  0d0
		:cd2_1  0d0
		:equinox 2000d0))
	      ;;
	      ((typep inst 'uh88-ll-hi-z-7-5-2-ccd)
	        (when (not (and bin1 bin2))
		  (error "CCDBIN1 and CCDBIN2 not defined/floats"))
	        (wcs:make-wcs-radec-tan
		:crval1 ra
		:crval2 dec
		:crpix1 (* 0.5d0 naxis1)
		:crpix2 (* 0.5d0 naxis2)
		:cd1_1  (* +7.634d-5 (/ bin1 2.0)) 
		:cd2_2  (* +7.634d-5 (/ bin2 2.0))
		:cd1_2  0d0
		:cd2_1  0d0
		:equinox 2000d0))
	      ;;
	      ((typep inst 'uh88-orbit-ccd)
	       (let ((pix-scale (get-pixel-scale-for-instrument inst fits-file)))
		 (wcs:make-wcs-radec-tan
		  :crval1 ra
		  :crval2 dec
		  :crpix1 (* 0.5d0 naxis1)
		  :crpix2 (* 0.5d0 naxis2)
		  :cd1_1  (* +1 (/ pix-scale 3600d0))
		  :cd2_2  (* -1 (/ pix-scale 3600d0))
		  :cd1_2  0d0
		  :cd2_1  0d0
		  :equinox 2000d0)))
	      ;;
	      ;; no way easily to test QUIRC - IR images too messy
	      ((typep inst 'uh88-quirc)
	       (let ((pix-scale (get-pixel-scale-for-instrument inst fits-file)))
		 (wcs:make-wcs-radec-tan
		  :crval1 ra
		  :crval2 dec
		  :crpix1 (* 0.5d0 naxis1)
		  :crpix2 (* 0.5d0 naxis2)
		  ;; N up, E left according to manual
		  ;; http://www.ifa.hawaii.edu/88inch/manuals/quirc.pdf
		  :cd1_1  (* -1 (/ pix-scale 3600d0))
		  :cd2_2  (* +1 (/ pix-scale 3600d0))
		  :cd1_2  0d0
		  :cd2_1  0d0
		  :equinox 2000d0)))

	      ))))

;; a common version for the OLD cameras
(defmethod get-initial-wcs-for-instrument ((inst %uh88-old-camera) fits-file
					   &key extension)
  (declare (ignorable inst extension)) 
  (or nil ;; no old wcs, ever
      (let* ((rastr (or (cf:read-fits-header fits-file "RA")
			(cf:read-fits-header fits-file "XRA") ;; experim hdr
			(error "RA or XRA not found")))
	     (decstr (or (cf:read-fits-header fits-file "DEC")
			 (cf:read-fits-header fits-file "XDEC") ;; experim hdr
			 (error "DEC or XDEC not found")))
	     (naxis1 (%gethead-or-error fits-file "NAXIS1"))
	     (naxis2 (%gethead-or-error fits-file "NAXIS2"))
	     ;; always 0.219 but we might eventually deal with binned data
	     (secppix (or (cf:read-fits-header fits-file "SECPPIX")
			  0.219d0)) ;; early data didn't have header
	     (ra (or (ignore-errors (ra-dec:hms-string->deg rastr))
		     (error "Could not make sense of RA='~A'" rastr)))
 	     (dec (or (ignore-errors (ra-dec:dms-string->deg decstr))
		      (error "Could not make sense of DEC='~A'" decstr)))
	     (epoch (cf:read-fits-header fits-file "EPOCH"))
	     ;; early data may not have epoch, so we try experimental
	     ;; header XEQUINOX, which is often missing or zero.
	     ;; Set it to 2000 and pray if invalid or zero
	     (xequinox
	       (if (not epoch)
		   (or (ignore-errors
			(jk-parse-float:parse-float
			 (cf:read-fits-header fits-file "XEQUINOX")))
		       2000d0))))

	;; Hail Mary if epoch is not set
	(when (not epoch)
	  (setf epoch (if (zerop xequinox) 2000d0 xequinox)))
	

	
	;; convert to 2000 if necessary
	(when (not (= epoch 2000d0))
	  (multiple-value-setq (ra dec)
	    (precess-ra-dec-to-j2000 epoch ra dec)))
	
	;; these will be WRONG - FIXME - there are rotations in
	;;  some frames we don't understand - different cameras? mounts?
	;;  readouts?
	(cond
	  ;; maybe the rotated one is different
	  ((typep inst '%uh88-old-tek/wfgs)
	   (wcs:make-wcs-radec-tan
	    :crval1 ra
	    :crval2 dec
	    :crpix1 (* 0.5d0 naxis1)
	    :crpix2 (* 0.5d0 naxis2)
	    :cd1_1  (* +1 (/ secppix 3600))
	    :cd2_2  (* -1 (/ secppix 3600))
	    :cd1_2  0d0
	    :cd2_1  0d0
	    :equinox 2000d0))
	  ;;
	  ((typep inst '%uh88-old-tek)
	   (wcs:make-wcs-radec-tan
	    :crval1 ra
	    :crval2 dec
	    :crpix1 (* 0.5d0 naxis1)
	    :crpix2 (* 0.5d0 naxis2)
	    :cd1_1  (* +1 (/ secppix 3600))
	    :cd2_2  (* -1 (/ secppix 3600))
	    :cd1_2  0d0
	    :cd2_1  0d0
	    :equinox 2000d0))

	  (t
	   nil)))))




(defmethod insert-initial-wcs-for-instrument ((inst %uh88-camera) fits-file
					       &key extension)
  (declare (ignore extension))
  (when (not (cf:read-wcs fits-file))
    (cf:write-wcs (get-initial-wcs-for-fits fits-file)
		  fits-file)))



