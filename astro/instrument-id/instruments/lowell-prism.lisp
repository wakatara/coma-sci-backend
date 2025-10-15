

(in-package instrument-id)

(defclass/inst lowell-prism (imaging-instrument onechip)
  ((name :initform "LOWELL-PRISM")
   (observatory :initform "lowell")
   (aperture :initform 1.8288) ;; 72 inches
   (saturation-level :initform 65535)
   (non-linear-level :initform 40000)))


  

(defun %lowell-prism-identify-instrument (fits-file)
  (when (and (equalp (cf:read-fits-header fits-file "TELESCOP") "PERKINS")
	     (equalp (cf:read-fits-header fits-file "DETECTOR")
		     "Prism 2048x2048 CCD"))
    (make-instance 'lowell-prism)))

(%add-instrument-id-function '%lowell-prism-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst lowell-prism) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("FILTNME1" "FILTNME2" "FILTNME3" "OBSTYPE" "TELRA" "TELDEC"
      "PIXSCAL" "ADELX_01" "ADELX_02")
    (call-next-method))
   :test 'equalp))

(defmethod get-standard-filter-for-instrument ((inst lowell-prism) fits-file)
  (let* ((filter1 (%gethead-or-error fits-file "FILTNME1"))
	 (filter2 (%gethead-or-error fits-file "FILTNME2"))
	 (filter3 (%gethead-or-error fits-file "FILTNME3"))
	 ;; different filters can be in different wheels, so we demand one filter
	 (filter-list (remove-if (lambda (x) (or (equalp x "Open") (equalp x "Empty")))
				 (list filter1 filter2 filter3)))
	 (filter ;; ONE active filter, or no filter, or error
	   (cond ((= (length filter-list) 0) "none")
		 ((> (length filter-list) 1)
		  (error "More than one Lowell-Prism filter in FILTNME1=~A, FILTNME2=~A, FILTNME3=~A is active; cannot determine filter" filter1 filter2 filter3))
		 (t (first filter-list)))))
    (cond ((equalp filter "U") :uj)
	  ((equalp filter "B") :bj)
	  ((equalp filter "V") :vj)
	  ((equalp filter "R") :rc)
	  ((equalp filter "I") :ic)
	  ((equalp filter "none") :open)
	  ;;;
	  (t NIL))))

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst lowell-prism) fits-file) ... )

(defmethod get-object-for-instrument ((inst lowell-prism) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst lowell-prism) fits-file)
  (let ((obj-type (%gethead-or-error fits-file  "OBSTYPE")))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "FLAT") :FLAT)
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))

(defmethod get-mjd-start-for-instrument ((inst lowell-prism) fits-file)
  (let* ((date-string  (%gethead-or-error fits-file "DATE-OBS"))
	 (mjd-start
	   (multiple-value-bind (year month day hour min sec)
	       (astro-time:parse-ut-date-and-time-string date-string)
	     (astro-time:calendar-date-to-mjd year month day hour min sec))))
    mjd-start))


;; generic version OK
;(defmethod get-mjd-mid-for-instrument ((inst lowell-prism) fits-file) ...  )

(defmethod get-gain-for-instrument ((inst lowell-prism) fits-file
				    &key (extension nil))
  (declare (ignore extension))
  ;; this is on the whiteboard at Lowell obs room - the header
  ;; AGAIN_01 is unreliable
  2.72)

;(defmethod get-chip-id-for-instrument ((inst lowell-prism) fits-file) ..)

;; there are multiple sizes of lowell, with different trimsecs
;; hope there's just these 2
(defmethod get-datasec-for-instrument ((inst lowell-prism) fits-file &key extension)
  (declare (ignore extension))
  (let ((naxis1 (%gethead-or-error fits-file "NAXIS1"))
	(naxis2 (%gethead-or-error fits-file "NAXIS2")))
    (cond
      ;; later version?
      ((and (= naxis1 2138) (= naxis2 2064))
       (vector 54 2101 2 2063))
      ;; later version 2x2 if such a thing exists
      ((and (= naxis1 (ash 2138 -1)) (= naxis2 (ash 2064 -1)))
	(vector (ash 54 -1) (ash 2101 -1) 1 (ash 2063 -1)))
      ;;
      ;; older version?
      ((and (= naxis1 2104) (= naxis2 2048))
       (vector 23 2064 1 2048))
      ;; older version binned 2x2 if such a thing exists
      ((and (= naxis1 (ash 2104 -1)) (= naxis2 (ash 2048 -1)))
       (vector 12 (ash 2064 -1)  1 (ash 2048 -1)))
      ;; hail mary - take the whole chip
      (t
       (vector 1 naxis1 1 naxis2)))))
	  

(defmethod get-initial-wcs-for-instrument
    ((inst lowell-prism) fits-file &key (extension))
  (declare (ignore extension))
  (let* 
       ((ra (or (ra-dec:hms-string->deg (%gethead-or-error fits-file "TELRA"))
		(error "TELRA header not found in ~A" fits-file)))
	(dec (or (ra-dec:dms-string->deg (%gethead-or-error fits-file "TELDEC"))
		 (error "TELDEC header not found in ~A" fits-file)))
	;; for some time, the pixscale was completely wrong at 2.54
	(pixscale/arcsec-claimed
	  (or (%gethead-or-error fits-file "PIXSCAL")
	      (error "PIXSCAL header not found in ~A" fits-file)))
	(binning (%gethead-or-error fits-file "ADELX_01")) 
	;;   replace with nominal value when bogus
	(pixscale/arcsec (if (< 2.53 pixscale/arcsec-claimed 2.55)
			     (/ 0.387d0 binning)
			     pixscale/arcsec-claimed))	
	;;
	(pixscale (/ pixscale/arcsec 3600))
	(pcenter  (* 0.5d0 (%gethead-or-error fits-file "NAXIS1"))) ;; for binning
	(wcs
	  (wcs:make-wcs-radec-tan 
	   :crval1 ra :crval2 dec
	   :crpix1 pcenter
	   :crpix2 pcenter
	   :cd1_1 (- pixscale)
	   :cd2_2 pixscale
	   :equinox 2000d0
	   )))
    wcs))

;; note that the WCS inside lowell prism is actually a bogus linear one (at least
;; in some old images)
(defmethod  get-pixel-scale-for-instrument ((inst lowell-prism) fits-file
					    &key extension)
  (err-if-not-image-at-extension inst fits-file "get-pixel-scale" extension)
  (let* ((pixscale/arcsec-claimed
	   (or (%gethead-or-error fits-file "PIXSCAL")
	       (error "PIXSCAL header not found in ~A" fits-file)))
	 (binning (%gethead-or-error fits-file "ADELX_01")) 
	 ;;   replace with nominal value when bogus
	 (pixscale/arcsec (if (< 2.53 pixscale/arcsec-claimed 2.55)
			      (/ 0.387d0 binning)
			      pixscale/arcsec-claimed)))
    pixscale/arcsec))



(defmethod insert-initial-wcs-for-instrument ((inst lowell-prism) fits-file
					      &key extension)
  (declare (ignore extension))
  (cf:delete-fits-header fits-file "CRDELT1") ;; the detault values are wrong
  (cf:delete-fits-header fits-file "CRDELT2")
  (cf:write-wcs (get-initial-wcs-for-instrument inst fits-file) fits-file))



