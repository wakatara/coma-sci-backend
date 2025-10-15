#|

FOCAM - somewhat documented here:

http://www.cfht.hawaii.edu/Instruments/Imaging/FOCAM/


|#



(in-package instrument-id)



;; examples: /Volumes/PDS/CDROMS/VALIDATED/CTIO_03-92_RAW/n1.bias.fits
(defclass/inst %cfht-focam (imaging-instrument onechip)
  ((name :initform "CFHT focam parent class")
   (aperture :initform 3.58)
   (observatory :initform "MKO"))) 

(defclass/inst cfht-focam-rca2 (%cfht-focam)
  ((name :initform "CFHT focam with rca2 CCD")))

(defclass/inst cfht-focam-unknown (%cfht-focam)
  ((name :initform "CFHT focam with UNKNOWN CCD")))

(defun %cfht-focam-identify-instrument (fits-file)
  (let ((instrument (cf:read-fits-header fits-file "INSTRUME"))
	(focusid (cf:read-fits-header fits-file "FOCUSID"))
	(detector (cf:read-fits-header fits-file "DETECTOR")))
    (when (and (or (equalp focusid  "Prime")
		   (equalp focusid  "Unknown")) ;; older data
	       (equalp instrument "focam"))
      (cond ((equalp detector "rca2 CCD")
	     (make-instance 'cfht-focam-rca2))
	    ;;
	    (t
	     (make-instance 'cfht-focam-unknown))))))
    

(%add-instrument-id-function '%cfht-focam-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst %cfht-focam) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("INTTIME" "UTIME" "ESCALE" "RA" "DEC")
    (call-next-method))
   :test 'equalp))

;; In these images, there is a FILTERS keyword representing the
;; "filter bolt positons" but that sometimes represents an actual
;; FILTER.  There is no universal map of the position to actual
;; filters, so we assume that if FILTERS is not a filter, then the
;; headers have been updated with a legitimate filter with a FILTER
;; keyword, or else return NIL.
(defmethod get-standard-filter-for-instrument ((inst %cfht-focam) fits-file)
  (let* ((filter (cf:read-fits-header fits-file "FILTER")) ;; like "4 R"
	 (fixed-filter (if (>= (length filter) 3)
			   (string-trim " " (subseq filter 1)))))
			 
    (flet ((get-filter (filt)
	     (cond ((equalp filt "U") :uj)
		   ((equalp filt "B") :bj)
		   ((equalp filt "V") :vj)
		   ((equalp filt "R") :rc)
		   ((equalp filt "I") :ic)
		   ;;
		   (t NIL))))
      (get-filter fixed-filter))))




(defmethod get-exptime-for-instrument ((inst %cfht-focam) fits-file)
  (declare (ignorable inst))
  (%gethead-or-error fits-file "INTTIME"))

(defmethod get-object-for-instrument ((inst %cfht-focam) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst %cfht-focam) fits-file)
  (let ((obj-type (%gethead-or-error  fits-file  "OBSTYPE")))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ;; can be "SKY FLAT" or "DOME FLAT"
	  ((search "FLAT" obj-type :test 'equalp) :FLAT)
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))


(defmethod get-mjd-start-for-instrument ((inst %cfht-focam) fits-file)
  (let* ((ut-string (%gethead-or-error fits-file "UTIME"))
	 (date-string-raw  (%gethead-or-error fits-file "DATEOBS"))
	 (date-string (astro-time:parse-yy/mm/dd-string date-string-raw
							:output :string))
	 (full-ut-string  (format nil "~AT~A" date-string ut-string))	 
	 (mjd-start 
	   (astro-time:parse-ut-date-and-time-string-to-mjd full-ut-string)))
    mjd-start))



;; generic version OK
;;(defmethod get-mjd-mid-for-instrument ((inst %cfht-focam) fits-file) ...  )


;; we THINK this is the ESCALE header (e-/ADCU)
(defmethod get-gain-for-instrument ((inst cfht-focam-rca2)
				    fits-file  &key extension)
  (declare (ignore inst extension))
  (%gethead-or-error fits-file "ESCALE"))



;; use general methods because we assume that whole image is valid
;; (and images can be trimmed)
(defmethod get-trimsec-for-instrument ((inst %cfht-focam)
				       fits-file &key extension)
  (declare (ignore inst fits-file extension))
  (call-next-method))

(defmethod get-datasec-for-instrument  ((inst %cfht-focam) fits-file &key extension)
  (declare (ignore inst fits-file extension))
  (call-next-method))



(defmethod get-pixel-scale-for-instrument  ((inst cfht-focam-rca2)
					    fits-file &key extension)
  (declare (ignore inst extension fits-file))
  0.205d0) ;; from astrometry.net fit
  



;; UH88 starts with a WCS 
(defmethod get-initial-wcs-for-instrument ((inst %cfht-focam) fits-file
					   &key extension)
  (declare (ignore extension))

  (let* ((rastr (%gethead-or-error fits-file "RA"))
	 (decstr (%gethead-or-error fits-file "Dec"))
	 (pix-scale/deg (/ (get-pixel-scale-for-instrument inst fits-file)
			   3600d0))
	 (naxis1    (%gethead-or-error fits-file "NAXIS1"))
	 (naxis2    (%gethead-or-error fits-file "NAXIS2"))
	 (ra (ra-dec:hms-string->deg rastr))
	 (dec (ra-dec:dms-string->deg decstr))
	 ;; don't trust epoch in header?  It seems that epoch is that
	 ;; of the observations
	 (current-epoch-year
	   (astro-time:mjd-to-decimal-year
	    (get-mjd-mid-for-instrument inst fits-file)))
	 (epoch (%gethead-or-error fits-file "EPOCH")))

    ;; precess to current era - if EPOCH=0 assume that epoch is the
    ;; time of observation.  This is just a guess.
    (multiple-value-setq (ra dec)
      (precess-ra-dec-to-j2000
       (if (zerop epoch) current-epoch-year epoch)
       ra dec))


    (cond ((typep inst 'cfht-focam-rca2)
	   (wcs:make-wcs-radec-tan
	    :crval1 ra
	    :crval2 dec
	    :crpix1 (* 0.5d0 naxis1)
	    :crpix2 (* 0.5d0 naxis2)
	    :cd1_1  (* +1 pix-scale/deg)
	    :cd2_2  (* -1 pix-scale/deg)
	    :cd1_2  0d0
	    :cd2_1  0d0
	    :equinox 2000d0))

	  (t
	   nil))))





(defmethod insert-initial-wcs-for-instrument ((inst %cfht-focam) fits-file
					       &key extension)
  (declare (ignore extension))
  (when (not (cf:read-wcs fits-file))
    (cf:write-wcs (get-initial-wcs-for-fits fits-file)
		  fits-file)))



