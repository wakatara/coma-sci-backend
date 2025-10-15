#|

imager on SNIFS - normally used for guide camera

|#

(in-package instrument-id)


(defclass/inst uh88-snifs-imager (imaging-instrument onechip)
  ((name :initform "UH88-SNIFS-IMAGER") ;; a string
   (aperture :initform 1.22)  
   (observatory :initform "MKO")))





(defun %uh88-snifs-imager-identify-instrument (fits-file)
  (let ((instrume (string-trim " " (cf:read-fits-header fits-file "INSTRUME")))
	(desc (string-trim " " (cf:read-fits-header fits-file "DESCRIP"))))
    (when (and (equal instrume "SNIFS")
	       (equal desc "AcqImage"))
      (make-instance 'uh88-snifs-imager))))
	

(%add-instrument-id-function '%uh88-snifs-imager-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst uh88-snifs-imager) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("INSTRUME" "DESCRIP" "IMAGETYP" "DATE-OBS"
      "SECPIX1" "SECPIX2" "EQUINOX")
    (call-next-method))
   :test 'equalp))

(defmethod get-standard-filter-for-instrument ((inst uh88-snifs-imager) fits-file)
  (let ((filter (%gethead-or-error fits-file "FILTER")))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not inserted into UH88-SNIFS-IMAGER image ~A" fits-file))
    ;; not sure if all of these exist
    (cond ((equalp filter "Bessell U") :uj)
	  ((equalp filter "Bessell B") :bj)
	  ((equalp filter "Bessell V") :vj)
	  ((equalp filter "Cousins R") :rc)
	  ((equalp filter "Cousins I") :ic)
	  ;;
	  ((equalp filter "SDSS u") :usdss)
	  ((equalp filter "SDSS g") :gsdss)
	  ((equalp filter "SDSS r") :rsdss)
	  ((equalp filter "SDSS i") :isdss)
	  ((equalp filter "SDSS z") :zsdss)
	  
	  ;;;
	  (t NIL))))

;; exposures seem to be negative?!
(defmethod get-exptime-for-instrument ((inst uh88-snifs-imager) fits-file)
   (%gethead-or-error fits-file "EXPTIME"))

  

(defmethod get-object-for-instrument ((inst uh88-snifs-imager) fits-file)
  (let ((obj (%gethead-or-error fits-file "OBJECT")))
    (if (not (equal obj "Unknown"))
	obj
	nil)))


;; not sure if BIAS and FLAT exist
(defmethod get-object-type-for-instrument ((inst uh88-snifs-imager) fits-file)
  (let ((obj (%gethead-or-error  fits-file  "IMAGETYP")))
    (cond ((search obj "BIAS" :test 'equalp) :BIAS)
	  ((search obj "FLAT" :test 'equalp) :FLAT)
	  ((search obj "OBJECT" :test 'equalp) :OBJECT)
	  (t :UNKNOWN))))



;; the problem is that the DATE-OBS is given as MM/DD/YY not DD/MM/YY -
;; is this always the case? sometimes?
;;  FIXME - assume universal use of M/D/Y?
(defmethod get-mjd-start-for-instrument ((inst uh88-snifs-imager) fits-file)
  (let* ((date-string  
	   (string-trim " " (%gethead-or-error fits-file "DATE-OBS")))
	 (mjd-start 
	   (astro-time:parse-ut-date-and-time-string-to-mjd date-string)))
    mjd-start))


(defmethod get-mjd-mid-for-instrument ((inst uh88-snifs-imager) fits-file)
  (+ (get-mjd-start-for-instrument inst fits-file)
     (* 0.5d0
	(/ (get-exptime-for-instrument inst fits-file) (* 24 3600)))))

(defmethod get-gain-for-instrument ((inst uh88-snifs-imager) fits-file  &key extension)
  (declare (ignore extension))
  ;; there are 4 gains but they're similar enough
  (%gethead-or-error fits-file "CCD0GAIN")) 
  

(defmethod get-pixel-scale-for-instrument ((inst uh88-snifs-imager)
					   fits-file &key extension)
  (declare (ignorable inst fits-file extension))
  (let ((secpix1 (%gethead-or-error fits-file "SECPIX1"))
	(secpix2 (%gethead-or-error fits-file "SECPIX2")))
    (when (not (= secpix1 secpix2))
      (error "Differing x,y pixel scales ~A ~A in ~A" secpix1 secpix2 fits-file))
    secpix1))
    

#+nil ;; hope we get only reduced images from the pipeline
(defmethod get-datasec-for-instrument ((inst uh88-snifs-imager) fits-file &key extension)
  (declare (ignore inst extension))
  ...)


;; See if a WCS was inserted; if not, try an RA,DEC, either decimal degrees or xx:mm:ss
(defmethod get-initial-wcs-for-instrument ((inst uh88-snifs-imager) fits-file
					   &key extension)
  (declare (ignore extension))
  (or (cf:read-wcs fits-file) ;; if WCS was inserted use that
      ;; else try ra,dec as degrees or HH:MM:SS DD:MM:SS
      (let* ((ra-raw (%gethead-or-error fits-file "RA"))
	     (dec-raw (%gethead-or-error fits-file "DEC"))
	     (equinox (%gethead-or-error fits-file "EQUINOX"))
	     (pixel-scale (/ (get-pixel-scale-for-instrument inst fits-file)
			     3600d0)) 
	     (ra (ra-dec:hms-string->deg ra-raw))
	     (dec (ra-dec:dms-string->deg dec-raw))
	     (naxis1 (%gethead-or-error fits-file "NAXIS1"))
	     (naxis2 (%gethead-or-error fits-file "NAXIS2")))

	(when (not (= equinox 2000d0))
	  (multiple-value-setq (ra dec)
	    (precess-ra-dec-to-j2000 equinox ra dec)))
	  

	(wcs:make-wcs-radec-tan
	 :crval1 ra
	 :crval2 dec
	 :crpix1 (* 0.5d0 naxis1)
	 :crpix2 (* 0.5d0 naxis2)
	 :cd1_1  (- pixel-scale)
	 :cd2_2  (- pixel-scale)
	 :cd1_2  0d0
	 :cd2_1  0d0
	 :equinox 2000d0)))) 
		    
(defmethod insert-initial-wcs-for-instrument ((inst uh88-snifs-imager) fits-file &key extension)
  (declare (ignore extension))
  (when (not (cf:read-wcs fits-file))
    (let ((wcs (get-initial-wcs-for-instrument inst fits-file)))
      (when (not wcs)
	(error "No initial WCS to write"))
      (cf:write-wcs wcs fits-file))))
					      
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fix-gap-in-snifs (fits-file &key (xgap 40))
  "Move the right amplifer XGAP to the right to compensate
for the missing chip gap in reduced data, putting NANs in the gap. The data 
on the extreme right of right chip are lost."
  (declare (type (integer 0 1000) xgap))
  (cf:with-open-fits-file (fits-file ff :mode :io)
    (let* ((imsec (cf:read-image-section ff))
	   (data  (cf:image-section-data imsec))
	   (nx (array-dimension data 1))
	   (ny (array-dimension data 0))
	   (nx/2 (ash nx -1))
	   (n0 (- nx/2 1)))
      (declare (type (unsigned-byte 20) nx ny nx/2 n0))
      (declare (type (simple-array single-float (* *)) data)
	       (optimize speed)) 
      (loop for ix of-type (unsigned-byte 20) from (- nx 1) downto (+ n0 xgap)
	    for ix2 of-type (unsigned-byte 20) = (- ix xgap)
	    do (loop for iy of-type (unsigned-byte 20) below ny
		     do (setf (aref data iy ix)
			      (aref data iy ix2))
			(setf (aref data iy ix2) float-utils:*single-float-nan*)))
      (cf:write-fits-header ff "GAPFIX" t
			    :comment  "Inserted chip gap into reduced data.")
      (cf:write-back-image-section imsec))))
      




