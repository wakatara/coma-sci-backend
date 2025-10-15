#|

a grab bag for some various old cameras on KPNO with similar headers

for some hints see:

https://www.noao.edu/noao/noaonews/jun96/node40.html

WARNING - the cameras might be mounted on the 0.9m or the 4m, and we
have no way of knowing, unless there are subtle header differences

It seems that TI2,3 were always on the 0.9m meter, though


|#

 

(in-package instrument-id)

;; mirror diameter can be either 0.9 or 0.4
(defclass/inst %kpno-old-camera (imaging-instrument onechip)
    ((name :initform "Parent class of old KPNO cameras") ;; a string
     (observatory :initform "KPNO")))


;; examples: /Volumes/PDS/CDROMS/VALIDATED/CTIO_03-92_RAW/n1.bias.fits
(defclass/inst kpno-south-444x400 (%kpno-old-camera)
    ((name :initform "Southern 444x400 imager"))) ;; a string

(defclass/inst %kpno-t1ka (%kpno-old-camera)
  ((name :initform "Parent class of KPNO t1ka 800x800 camera")))
(defclass/inst kpno-kp09m-t1ka (%kpno-t1ka)
  ((name :initform "KPNO 0.9m t1ka 800x800 camera")))
(defclass/inst kpno-kp21m-t1ka (%kpno-t1ka)
  ((name :initform "KPNO 2.1m t1ka 800x800 camera")))

(defclass/inst %kpno-t2ka (%kpno-old-camera)
  ((name :initform "Parent class of KPNO t2ka 2k camera")))
(defclass/inst kpno-kp09m-t2ka (%kpno-t2ka)
  ((name :initform "KPNO 0.9m t2ka 2k camera")))
(defclass/inst kpno-kp21m-t2ka (%kpno-t2ka)
  ((name :initform "KPNO 2.1m t2ka 2k camera")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/inst %kpno-TI (%kpno-old-camera)
  ((name :initform "Parent class of KPNO TIx imager")))
;;;;;;
(defclass/inst %kpno-TI2 (%kpno-ti)
  ((name :initform "Parent class of KPNO TI2 imager")))
(defclass/inst kpno-TI2-400x400 (%kpno-ti2)
  ((name :initform "Northern 400x400 TI2 KPNO imager in 2x2 bin")))
(defclass/inst kpno-TI2-800x800 (%kpno-ti2)
  ((name :initform "Northern 400x400 TI2 KPNO imager in 1x1 bin")))
;;;;;;;;;;
(defclass/inst %kpno-TI3 (%kpno-ti)
  ((name :initform "Parent class of KPNO TI2 imager")))
(defclass/inst kpno-TI3-400x400 (%kpno-ti3)
  ((name :initform "Northern 400x400 TI3 KPNO imager in 2x2 bin")))
(defclass/inst kpno-TI3-800x800 (%kpno-ti3)
  ((name :initform "Northern 400x400 TI3 KPNO imager in 1x1 bin")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass/inst %kpno-rca (%kpno-old-camera)
  ((name :initform "Parent class of KPNO RCAx imager")))
;;;;;;
(defclass/inst %kpno-rca1 (%kpno-rca)
  ((name :initform "Parent class of KPNO RCA1 imager")))
(defclass/inst kpno-rca1-400x400 (%kpno-rca1) ;; FIX ME - this is a guess
  ((name :initform "Northern 400x400 RCA1 KPNO imager in 2x2 bin")))
(defclass/inst kpno-rca1-800x800 (%kpno-rca1)
  ((name :initform "Northern 400x400 RCA1 KPNO imager probably in 1x1 bin")))
;;;
(defclass/inst %kpno-rca2 (%kpno-rca)
  ((name :initform "Parent class of KPNO RCA2 imager")))
(defclass/inst kpno-rca2-400x400 (%kpno-rca2) ;; FIX ME - this is a guess
  ((name :initform "Northern 400x400 RCA2 KPNO imager in 2x2 bin")))
(defclass/inst kpno-rca2-800x800 (%kpno-rca2)
  ((name :initform "Northern 400x400 RCA2 KPNO imager probably in 1x1 bin")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %kpno-old-camera-identify-instrument (fits-file)
  (let* ((observatory (cf:read-fits-header fits-file "OBSERVAT"))
	 (telescop (cf:read-fits-header fits-file "TELESCOP"))
	 (naxis1    (cf:read-fits-header fits-file "NAXIS1"))
	 (naxis2    (cf:read-fits-header fits-file "NAXIS2"))
	 (instrument (cf:read-fits-header fits-file "INSTRUME"))
	 (detector (cf:read-fits-header fits-file "DETECTOR"))
	 (f1pos    (cf:read-fits-header fits-file "F1POS"))
	 (f2pos    (cf:read-fits-header fits-file "F2POS"))
	 (tvfilt   (cf:read-fits-header fits-file "TVFILT"))
	 (cmp-lamp   (cf:read-fits-header fits-file "CMP-LAMP"))
	 (ccdsum (cf:read-fits-header fits-file "CCDSUM"))
	 (bvec   (when ccdsum (parse-ccdsum ccdsum :return :vector)))
	 (binning (when (and bvec (= (aref bvec 0) (aref bvec 1)))
		    (aref bvec 0))))
		     
    (declare (ignorable telescop instrument detector observatory))

    (cond ((not (and (integerp naxis1) (integerp naxis2)))
	   nil)		    ;; not a first image header
	  ;;
	  ((and observatory ;; there can be confusion with CTIO for some insts
		(not (search "KPNO" observatory :test 'equalp)))
	   nil) ;; if OBSERVAT is present and not KPNO, return NIL
	  ;;
	  ((and (or (and (= naxis1 444) (= naxis2 400))
		    (and (= naxis1 396) (= naxis2 396)))
		(not telescop)
		(not detector)
		(not observatory)
		f1pos f2pos tvfilt cmp-lamp) ;; these headers must exist
	   (make-instance 'kpno-south-444x400))
	  ;;
	  ((and (equalp telescop "kp09m")
		(equalp detector "t2ka"))
	   (make-instance 'kpno-kp09m-t2ka))
	  ;;
	  ((and (equalp telescop "kp21m")
		(equalp detector "t2ka"))
	   (make-instance 'kpno-kp21m-t2ka))
	  ;;
	  ((and (equalp telescop "kp09m")
		(equalp detector "t1ka"))
	   (make-instance 'kpno-kp09m-t1ka))
	  ;;
	  ((and (equalp telescop "kp21m")
		(equalp detector "t1ka"))
	   (make-instance 'kpno-kp21m-t1ka))
	  ;;
	  ;; this one is ugly because there is no real ID info besides TI2
	  ((and (equalp detector "TI2") 
		(equalp (cf:read-fits-header fits-file "ORIGIN")
			"KPNO-IRAF")
		(<= naxis1 832) (<= naxis2 832) ;; this is a small chip
		(or
		 ;; sometimes there is a KPNO header
		 (equalp observatory "KPNO")
		 (and ;; or we find some some unusual headers
		  (cf:read-fits-header fits-file "OTIME")
		  (cf:read-fits-header fits-file "DARKTIME")
		  (cf:read-fits-header fits-file "CAMTEMP")
		  (cf:read-fits-header fits-file "DEWTEMP")
		  (cf:read-fits-header fits-file "ST"))))
	   (cond ((eql binning 2)
		  (make-instance 'kpno-ti2-400x400))
		 ((eql binning 1)
		  (make-instance 'kpno-ti2-800x800))))
	  ;;
	  ((and (equalp detector "TI3")
		(equalp (cf:read-fits-header fits-file "ORIGIN")
			"KPNO-IRAF")
		(<= naxis1 832) (<= naxis2 832) ;; this is a small chip
		;; find some unusual headers
		(cf:read-fits-header fits-file "TVFILT")
		(cf:read-fits-header fits-file "OTIME")
		(cf:read-fits-header fits-file "DARKTIME")
		(cf:read-fits-header fits-file "CAMTEMP")
		(cf:read-fits-header fits-file "DEWTEMP")
		(cf:read-fits-header fits-file "ST"))
	   (cond ((eql binning 2)
		  (make-instance 'kpno-ti3-400x400))
		 ((eql binning 1)
		  (make-instance 'kpno-ti3-800x800))))
	  ;;
	  	  ;;
	  ;; RCA2 is a bit better defined
	  ((and (equalp detector "RCA1")
		(equalp observatory "KPNO")
		(<= naxis1 832) (<= naxis2 832)) ;; this is a small chip
	   (cond ((eql binning 2)
		  (make-instance 'kpno-rca1-400x400))
		 ((eql binning 1)
		  (make-instance 'kpno-rca1-800x800))))
	  ;;
	  ;; RCA2 is a bit better defined
	  ((and (equalp detector "RCA2")
		(equalp observatory "KPNO")
		(<= naxis1 832) (<= naxis2 832)) ;; this is a small chip
	   (cond ((eql binning 2)
		  (make-instance 'kpno-rca2-400x400))
		 ((eql binning 1)
		  (make-instance 'kpno-rca2-800x800))))
	  )))
	   

(%add-instrument-id-function '%kpno-old-camera-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod get-critical-headers-for-instrument ((inst %kpno-old-camera)
						fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("ORIGIN" "OTIME" "DARKTIME" "CAMTEMP" "DEWTEMP" "ST" "TVFILT"
      "DATA-TYP" "IMAGETYP" "UT" "DATE-OBS" "RA" "DEC")
    (call-next-method))
   :test 'equalp))


;; make filter-pairs a list like  '((1 . "U") (2 . "B") ...)
;; COMMENT    Filter 1 = U
;; COMMENT    Filter 2 = B
;; COMMENT    Filter 3 = V
;; COMMENT    Filter 4 = R
;; COMMENT    Filter 5 = I
;; COMMENT    Filter 6 = 5009/45
(defun %parse-kpno-old-filter-table-v1 (comment-list)
  (loop with filter-pairs = nil
	for (comment string) in comment-list
	for nfilt = (search "Filter " string :test 'equalp)
	for n=  =  (search "=" string)
	when (and (equalp comment "COMMENT") ;; always true
		  nfilt n= (> n= nfilt))
	  do (let ((nfilt (parse-integer string :start (+ nfilt 6) :junk-allowed t))
		   (filtstr
		     (string-trim " " (subseq string (1+ n=)))))
	       (push (cons nfilt filtstr) filter-pairs))
	finally
	   ;; Now we try to fix the filter map for the rare cases where
	   ;; the filters are ubvri (not UBVRI), but no capital filters
	   ;; are present. This is interpreted as a typo at the telescope
	   ;; so ubvri is really UBVRI
	   (when (and (every ;; every lowercase filter is in our list
		       (lambda (filter)  (find filter filter-pairs :key #'cdr :test #'equal))
		       '("u" "b" "v" "r" "i"))
		      (notany ;; no uppercase filter is in our list
		       (lambda (filter)  (find filter filter-pairs :key #'cdr :test #'equal))
		       '("U" "B" "V" "R" "I")))
	     ;;
	     (loop for pair in filter-pairs
		   when (find (cdr pair) '("u" "b" "v" "r" "i") :test 'equal)
		     do (setf (cdr pair) (string-upcase (cdr pair)))))
	   ;;
	   (return filter-pairs)))

;; old version of the form
;;
;; COMMENT  Filters: 1  b6649
;; COMMENT           2  Harris B KP1569
;; COMMENT           3  Harris V KP1421
;; COMMENT           4  Harris R KP1422
;; COMMENT           5  KP247 Sii 6719
;; COMMENT           6  KP376 Ha  6567
;; COMMENT           7  KP797 Nii 6589
;; COMMENT           8  b6737

(defun %parse-kpno-old-filter-table-v0 (comment-list)
  (loop with n = 0
	for fnum = nil
	for filt = nil
	for (comment string) in comment-list
	for filter-num
	  = (cond
	      ;; a line with Filters:
	      ((setf n (search "Filters:" string :test 'equalp))
		   (multiple-value-bind (nf nc)
		       (parse-integer string :start (+ n 8) :junk-allowed t)
		     (when (and nf (<= 1 nf 8))
		       (setf fnum nf)
		       (setf filt (string-trim " " (subseq string nc))))))
	      ;; otherwise it's a line without Filters:
	      (t
	       (multiple-value-bind (nf nc)
		   (parse-integer string :start 0 :junk-allowed t)
		 (when (and nf (<= 1 nf 8))
		   (setf fnum nf)
		   (setf filt (string-trim " " (subseq string nc)))))))
	when (and fnum filt)
	  collect (cons fnum filt) into filt-pairs
	finally
	   (return 
	     (loop for pair in filt-pairs
		   for n = (car pair) and name = (cdr pair)
		   ;; used https://www.noao.edu/kpno/filters/4Inch_List.html
		   for true-name = (cond ((search "Harris B" name) "B")
					 ((search "Harris V" name) "V")
					 ((search "Harris R" name) "R")
					 ((search "KP1591" name)   "I")
					 ((search "KP1539" name)   "I")
					 ((search "KP1469" name)   "I")
					 (t ;; otherwise, just keep the name
					  (substitute  #\_ #\space name)))
		   collect (cons n true-name)))))

(defun %parse-kpno-old-filter-table (comment-list)
  (or (%parse-kpno-old-filter-table-v1 comment-list)
      (%parse-kpno-old-filter-table-v0 comment-list)))
  

(defmethod get-standard-filter-for-instrument ((inst %kpno-old-camera)
					       fits-file)
  (let* ((filter   (cf:read-fits-header fits-file "FILTER")))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; if FILTER is not defined, then try to parse FILTERS using
    ;; the key sometimes found in the COMMENT headers
    (when (not filter)
      (let* ((filters  (cf:read-fits-header fits-file "FILTERS"))
	     ;; ensure FILTERS is a string - don't trust old headers
	     (filters-string  (if filters (format nil "~A" filters)))
	     (filters-int (ignore-errors (parse-integer filters-string))) ;; integer form of filters
	     (comment-list (cf:read-fits-header-list fits-file
						     :keyword-wildcard "COMMENT"))
	     (filter-pairs
	       (ignore-errors ;; be safe
		(%parse-kpno-old-filter-table comment-list))))
	(cond ((member filters-string '("U" "B" "V" "R" "I") :test 'equal)
	       (setf filter filters))
	      (filters-int
	       (setf filter (cdr (assoc filters-int filter-pairs)))))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
    (cond ((equal filter "U") :uj)
	  ((equal filter "B") :bj)
	  ((equal filter "V") :vj)
	  ((equal filter "R") :rc)
	  ((equal filter "I") :ic)
	  ;;
	  (t (nth-value 0 (intern
			   (format nil "KPNO-FILTER-~A" (string-upcase filter))
			   :keyword)))))) ;; just intern the string - yuck


(defmethod get-exptime-for-instrument ((inst kpno-south-444x400) fits-file)
  (declare (ignorable inst))
  (%gethead-or-error fits-file "OTIME"))

(defmethod get-object-for-instrument ((inst %kpno-old-camera) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defun %kpno-old-cam-type-finder (fits-file keyword)
  (let ((data-type (%gethead-or-error  fits-file keyword)))
    (cond ((or (search "BIAS" data-type :test 'equalp)
	       (equalp "zero" data-type)) ;; for t1ka camera
	       :BIAS)
	  ;; can be "SKY FLAT" or "DOME FLAT"
	  ((search "FLAT" data-type :test 'equalp) :FLAT)
	  (t :OBJECT))))

(defmethod get-object-type-for-instrument ((inst kpno-south-444x400) fits-file)
  (declare (ignore inst))
  (%kpno-old-cam-type-finder fits-file "DATA-TYP"))


(defmethod get-object-type-for-instrument
    ((inst %kpno-t1ka) fits-file)
  (declare (ignore inst))
  (%kpno-old-cam-type-finder fits-file "IMAGETYP"))

(defmethod get-object-type-for-instrument
    ((inst %kpno-t2ka) fits-file)
  (declare (ignore inst))
  (%kpno-old-cam-type-finder fits-file "IMAGETYP"))

(defmethod get-object-type-for-instrument
    ((inst %kpno-TI2) fits-file)
  (declare (ignore inst))
  (%kpno-old-cam-type-finder fits-file "IMAGETYP"))

(defmethod get-object-type-for-instrument
    ((inst %kpno-TI3) fits-file)
  (declare (ignore inst))
  (%kpno-old-cam-type-finder fits-file "IMAGETYP"))

(defmethod get-object-type-for-instrument
    ((inst %kpno-rca1) fits-file)
  (declare (ignore inst))
  (%kpno-old-cam-type-finder fits-file "IMAGETYP"))

(defmethod get-object-type-for-instrument
    ((inst %kpno-rca2) fits-file)
  (declare (ignore inst))
  (%kpno-old-cam-type-finder fits-file "IMAGETYP"))



  


;; and we need to fix this to be YYYY-MM-DD
(defun %fix-old-kpno-date-string (dstring)
  (cond ((not (find #\/ dstring))
	 dstring) ;; string is OK new-style date
	;;
	(t
	 (astro-time:parse-dd/mm/yy-string dstring :output :string))))


(defmethod get-mjd-start-for-instrument ((inst %kpno-old-camera) fits-file)
  (let* ((ut-string (%gethead-or-error fits-file "UT"))
	 (date-string-raw  (%gethead-or-error fits-file "DATE-OBS"))
	 (date-string (%fix-old-kpno-date-string date-string-raw))
	 (full-ut-string  (format nil "~AT~A" date-string ut-string))	 
	 (mjd-start 
	   (multiple-value-bind (year month day hour min sec)
	       (astro-time:parse-ut-date-and-time-string full-ut-string)
	     (astro-time:calendar-date-to-mjd year month day hour min sec))))
    mjd-start))



;; generic version OK
;;(defmethod get-mjd-mid-for-instrument ((inst %kpno-old-camera) fits-file) ...  )



(defmethod get-gain-for-instrument ((inst %kpno-old-camera)
				    fits-file  &key extension)
  (declare (ignore inst extension fits-file))
  nil)

(defmethod get-gain-for-instrument ((inst kpno-south-444x400)
				    fits-file  &key extension)
  (declare (ignore inst extension fits-file))
 nil #+nil 0.50) ;; obtained from image stats on one night


(defmethod get-gain-for-instrument ((inst %kpno-t1ka)
				    fits-file  &key extension)
  (declare (ignore inst extension))
  (%gethead-or-error fits-file "GAIN"))



(defmethod get-gain-for-instrument ((inst kpno-kp09m-t2ka)
				    fits-file  &key extension)
  (declare (ignore inst extension))
  (%gethead-or-error fits-file "GAIN"))

(defmethod get-gain-for-instrument ((inst kpno-kp21m-t2ka)
				    fits-file  &key extension)
  (declare (ignore inst extension))
  (%gethead-or-error fits-file "GAIN"))


;; use general methods because we assume that whole image is valid
;; (and images can be trimmed)
(defmethod get-trimsec-for-instrument ((inst %kpno-old-camera)
				       fits-file &key extension)
  (declare (ignore inst fits-file extension))
  (call-next-method))

(defmethod get-datasec-for-instrument
    ((inst %kpno-old-camera) fits-file &key extension)
  (declare (ignore inst fits-file extension))
  (call-next-method))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-pixel-scale-for-instrument  ((inst %kpno-old-camera)
					    fits-file &key extension)
  (declare (ignore inst extension))
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-pixel-scale-for-instrument ((inst kpno-south-444x400)
					   fits-file &key extension)
  (declare (ignore inst extension))
  0.554d0) ;; from fit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-pixel-scale-for-instrument ((inst kpno-kp09m-t1ka)
					   fits-file &key extension)
  (declare (ignore inst extension))
  0.680d0) ;; from fit of 2.1, in analogy to t2ka detector

(defmethod get-pixel-scale-for-instrument ((inst kpno-kp21m-t1ka)
					   fits-file &key extension)
  (declare (ignore inst extension))
  0.305d0) ;; from fit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-pixel-scale-for-instrument ((inst kpno-kp09m-t2ka)
					   fits-file &key extension)
  (declare (ignore inst extension))
  0.680d0) ;; from fit

(defmethod get-pixel-scale-for-instrument ((inst kpno-kp21m-t2ka)
					   fits-file &key extension)
  (declare (ignore inst extension))
  0.304d0) ;; from fit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-pixel-scale-for-instrument ((inst kpno-TI2-400x400)
					   fits-file &key extension)
  (declare (ignore inst extension))
  0.388d0) ;; from fit

(defmethod get-pixel-scale-for-instrument ((inst kpno-TI2-800x800)
					   fits-file &key extension)
  (declare (ignore inst extension))
  (* 0.5d0 0.388d0)) ;; from fit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-pixel-scale-for-instrument ((inst kpno-TI3-400x400)
					   fits-file &key extension)
  (declare (ignore inst extension))
  0.388d0) ;; from fit

(defmethod get-pixel-scale-for-instrument ((inst kpno-TI3-800x800)
					   fits-file &key extension)
  (declare (ignore inst extension))
  (* 0.5d0 0.388d0)) ;; from fit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; we guess that RCA1 is same as RCA2
(defmethod get-pixel-scale-for-instrument ((inst kpno-rca1-400x400)
					   fits-file &key extension)
  (declare (ignore inst extension))
  (* 2 1.47d0)) ;; from fit

(defmethod get-pixel-scale-for-instrument ((inst kpno-rca1-800x800)
					   fits-file &key extension)
  (declare (ignore inst extension))
  1.47d0) ;; from fit - big fat pixels

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-pixel-scale-for-instrument ((inst kpno-rca2-400x400)
					   fits-file &key extension)
  (declare (ignore inst extension))
  (* 2 1.47d0)) ;; from fit

(defmethod get-pixel-scale-for-instrument ((inst kpno-rca2-800x800)
					   fits-file &key extension)
  (declare (ignore inst extension))
  1.47d0) ;; from fit - big fat pixels

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod get-initial-wcs-for-instrument ((inst %kpno-old-camera) fits-file
					   &key extension)
  (declare (ignore extension))
  (let* ((rastr (%gethead-or-error fits-file "RA"))
	 (decstr (%gethead-or-error fits-file "Dec"))
	 (pix-scale/deg
	   (/ (get-pixel-scale-for-instrument inst fits-file) 3600d0))
	 (naxis1    (%gethead-or-error fits-file "NAXIS1"))
	 (naxis2    (%gethead-or-error fits-file "NAXIS2"))
	 (ra (ra-dec:hms-string->deg rastr))
	 (dec (ra-dec:dms-string->deg decstr))
	 ;; don't trust epoch in header?  It seems that epoch is somtimes that
	 ;; of the observations
	 (current-epoch-year
	   (astro-time:mjd-to-decimal-year
	    (get-mjd-mid-for-instrument inst fits-file)))
	 (epoch (%gethead-or-error fits-file "EPOCH"))
	 (equinox (cf:read-fits-header fits-file "EQUINOX")))

    ;; kpno-kp09m-t2ka favors EQUINOX so use that
    (when (and (typep inst 'kpno-kp09m-t2ka)
	       equinox)
      (setf epoch equinox))
    
    ;; precess to current era - if EPOCH=0 assume that epoch is the
    ;; time of observation.  This is just a guess.
    (multiple-value-setq (ra dec)
      (precess-ra-dec-to-j2000
       (if (zerop epoch) current-epoch-year epoch)
       ra dec))
    

    (cond ((typep inst 'kpno-south-444x400)
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
	  ((or (typep inst 'kpno-kp09m-t1ka)
	       (typep inst 'kpno-kp21m-t1ka))
	   (wcs:make-wcs-radec-tan
	    :crval1 ra
	    :crval2 dec
	    :crpix1 (* 0.5d0 naxis1)
	    :crpix2 (* 0.5d0 naxis2)
	    :cd1_1  0d0
	    :cd2_2  0d0
	    :cd1_2  (* +1 pix-scale/deg)
	    :cd2_1  (* +1 pix-scale/deg)
	    :equinox 2000d0))
	  ;;
	  ((or (typep inst 'kpno-kp09m-t2ka)
	       (typep inst 'kpno-kp21m-t2ka))
	   (wcs:make-wcs-radec-tan
	    :crval1 ra
	    :crval2 dec
	    :crpix1 (* 0.5d0 naxis1)
	    :crpix2 (* 0.5d0 naxis2)
	    :cd1_1  0d0
	    :cd2_2  0d0
	    :cd1_2  (* -1 pix-scale/deg)
	    :cd2_1  (* -1 pix-scale/deg)
	    :equinox 2000d0))
	  ;;
	  ((typep inst '%kpno-ti) ;; ti2 and ti3 so far
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
	  ;; %kpno-rac2 has its special method
	  
	  )
	  ;;
	  ))

;; this one normally has RA=0,DEC=0, undefined EPOCH
(defmethod get-initial-wcs-for-instrument ((inst %kpno-rca) fits-file
					   &key extension)
  (declare (ignore extension))
  (let* ((rastr (%gethead-or-error fits-file "RA"))
	 (decstr (%gethead-or-error fits-file "Dec"))
	 (naxis1    (%gethead-or-error fits-file "NAXIS1"))
	 (naxis2    (%gethead-or-error fits-file "NAXIS2"))
	 (pix-scale/deg
	   (/ (get-pixel-scale-for-instrument inst fits-file) 3600d0))
	 (ra (ra-dec:hms-string->deg rastr))
	 (dec (ra-dec:dms-string->deg decstr))
	 ;; don't trust epoch in header?  It seems that epoch is somtimes that
	 ;; of the observations
	 (epoch (cf:read-fits-header fits-file "EPOCH"))
	 (equinox (cf:read-fits-header fits-file "EQUINOX")))

    (when (and (zerop ra) (zerop dec))
      (error "Cannot compute initial WCS.  RA and DEC have dummy values of 0 - this is common with KPNO-RCA2"))

    (when (and (not epoch) (not equinox))
      (error "EPOCH or EQUINOX of coordinates is undefined.  This must be fixed for KPNO-RCA2"))

    (when (not (floatp (setf epoch (or epoch equinox))))
      (error "EPOCH/EQUINOX is not a string in KPNO-RCA2"))
    
    ;; precess to current era - if EPOCH=0 assume that epoch is the
    ;; time of observation.  This is just a guess.
    (multiple-value-setq (ra dec)
      (precess-ra-dec-to-j2000 epoch ra dec))
    
    (wcs:make-wcs-radec-tan
     :crval1 ra
     :crval2 dec
     :crpix1 (* 0.5d0 naxis1)
     :crpix2 (* 0.5d0 naxis2)
     :cd1_1  0d0
     :cd2_2  0d0
     :cd1_2  (* -1 pix-scale/deg)
     :cd2_1  (* +1 pix-scale/deg)
     :equinox 2000d0)))


(defmethod insert-initial-wcs-for-instrument ((inst %kpno-old-camera) fits-file
					       &key extension)
  (declare (ignore extension))
  (when t ;(not (cf:read-wcs fits-file))
    (cf:write-wcs (get-initial-wcs-for-fits fits-file)
		  fits-file)))



