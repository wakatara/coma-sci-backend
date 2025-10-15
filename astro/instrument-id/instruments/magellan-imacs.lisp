#|

Magellan IMACS in imaging mode, which can have one of three cameras

see http://www.lco.cl/telescopes-information/magellan/instruments/imacs/user-manual/the-imacs-user-manual

|#

(in-package instrument-id)

;; parent class with the two mosaic cameras
(defclass/inst %magellan-imacs (imaging-instrument onechip)
  ((name :initform "Magellan IMACS parent class") ;; a string
   (gain-keyword :initform "EGAIN")
   (aperture :initform 8.4)
   (observatory :initform "LCO")))

;; class for each camera
(defclass/inst magellan-imacs-mosaic1 (%magellan-imacs) ;; defunct
  ((name :initform "Magellan IMACS with Mosaic1 camera")))

(defclass/inst magellan-imacs-mosaic2 (%magellan-imacs)
  ((name :initform "Magellan IMACS with Mosaic2 camera")))

(defclass/inst magellan-imacs-mosaic3 (%magellan-imacs)
  ((name :initform "Magellan IMACS with Mosaic3 camera")))



(defun %magellan-imacs-identify-instrument (fits-file)
  (when (and (member (cf:read-fits-header fits-file "INSTRUME")
		     ;; long-camera is a guess
		     '("IMACS Short-Camera" "IMACS Long-Camera")
		     :test 'equalp)
	     (equalp (cf:read-fits-header fits-file "TELESCOP")
		     "Baade_Mag_1"))
    (let ((dewar (cf:read-fits-header fits-file "DEWAR"))
	  (chip-id (cf:read-fits-header fits-file "CHIP")))
    (cond ((equalp dewar "Mosaic1")
	   (make-instance 'magellan-imacs-mosaic1
			  :chip-id chip-id))
	  ((equalp dewar "Mosaic2")
	   (make-instance 'magellan-imacs-mosaic2
			  :chip-id chip-id))
	  ((equalp dewar "Mosaic3")
	   (make-instance 'magellan-imacs-mosaic3
			  :chip-id chip-id))
	  (t
	   (error "Uknown IMACS dewar ~A" dewar))))))
	  

(%add-instrument-id-function '%magellan-imacs-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument ((inst %magellan-imacs) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("INSTRUME" "TELESCOP" "DEWAR" "CHIP" "EXPTYPE" "TIME-OBS" "DATE-OBS"
      "EGAIN" "SCALE" "RA-D" "DEC-D" "ROTANGLE" "CHOFFX" "CHOFFY")
    (call-next-method))
   :test 'equalp))

;; see http://www.lco.cl/telescopes-information/magellan/instruments/imacs/user-manual/the-imacs-user-manual
(defmethod get-standard-filter-for-instrument ((inst %magellan-imacs) fits-file)
  (let ((filter (%gethead-or-error fits-file "FILTER")))
    (cond
      ;; this is 480 to 780 nm when gri is 400 to 820 or so
      ((equal filter "WB4800-7800") :gri-cfht-megacam) ;; BOGUS
      ;; the following are from manual 
      ((equal filter "Bessel B") :bj) 
      ((equal filter "Bessel V") :vj)
      ((equal filter "Bessel R") :rc)
      ((equal filter "CTIO I") :ic) ;; just a guess
	  ;;;
      (t NIL))))


  
;; generic version OK
;(defmethod get-exptime-for-instrument ((inst %magellan-imacs) fits-file) ... )

(defmethod get-object-for-instrument ((inst %magellan-imacs) fits-file)
  (%gethead-or-error fits-file "OBJECT"))

(defmethod get-object-type-for-instrument ((inst %magellan-imacs) fits-file)
  (let ((obj-type (%gethead-or-error  fits-file  "EXPTYPE")))
    (cond ((equalp obj-type "BIAS") :BIAS) ;; no idea
	  ((equalp obj-type "FLATFIELD") :FLAT) ;; if this is right
	  ((equalp obj-type "OBJECT") :OBJECT) ;; but this is known
	  (T :OTHER))))




(defmethod get-mjd-start-for-instrument ((inst %magellan-imacs) fits-file)
  (let* ((ut-string (%gethead-or-error fits-file "TIME-OBS"))
	 (date-string  (%gethead-or-error fits-file "DATE-OBS"))
	 (full-ut-string  (format nil "~AT~A" date-string ut-string))	 
	 (mjd-start 
	   (multiple-value-bind (year month day hour min sec)
	       (astro-time:parse-ut-date-and-time-string full-ut-string)
	     (astro-time:calendar-date-to-mjd year month day hour min sec))))
    mjd-start))



;; generic version OK
;(defmethod get-mjd-mid-for-instrument ((inst %magellan-imacs) fits-file) ...  )

(defmethod instrument-gain-keyword ((inst %magellan-imacs))  "EGAIN")
;(defmethod get-gain-for-instrument ((inst %magellan-imacs) fits-file)   ...)


;;(defmethod get-trimsec-for-instrument ((inst %magellan-imacs) fits-file &key extension) ..

;; (defmethod get-datasec-for-instrument ((inst %magellan-imacs) fits-file &key extension)....

;; (defmethod get-chip-id-for-instrument ((inst %magellan-imacs) fits-file &key (extension)) .... 

(defmethod get-pixel-scale-for-instrument  ((inst %magellan-imacs)
					    fits-file &key extension)
  (declare (ignore inst extension))
  (%gethead-or-error fits-file "SCALE"))


;; Magellan fails to give WCS, and the field is rotated.
;; f--k them and the horse they rode in on.
;; here's a reference:
;;   https://github.com/dnidever/PHOTRED/blob/master/pro/wcsfit_imacs.pro
(defmethod get-initial-wcs-for-instrument ((inst %magellan-imacs) fits-file
					   &key extension)
  (declare (ignore extension))
  (let* ((ra (%gethead-or-error fits-file "RA-D")) ;; decimal deg
	 (dec (%gethead-or-error fits-file "DEC-D"))
	 (rotangle (%gethead-or-error fits-file "ROTANGLE"))
	 (chip-id  (%gethead-or-error fits-file "CHIP"))
	 (naxis1   (%gethead-or-error fits-file "NAXIS1"))
	 (naxis2   (%gethead-or-error fits-file "NAXIS2"))
	 (pscale   (%gethead-or-error fits-file "SCALE"))
	 (pscale-deg (/ pscale 3600d0))
	 ;; chip offsets arcsec of center of chip to center of array
	 (choffx   (%gethead-or-error fits-file "CHOFFX"))	
	 (choffy   (%gethead-or-error fits-file "CHOFFY"))
	 ;; chips are oriented differently - this is somewhat tested for MOSAIC2
	 (xflip    (if (<= chip-id 4) -1 +1))
	 (yflip    (if (>= chip-id 5) -1 +1))
	 ;; the angle we add to the rotangle to get true angle
	 ;; this is explained exactly nowhere but is left as an exercise
	 ;; for the user
	 (angle-bullshit (- 180d0 46.23d0 90))
	 (true-rotangle (* (/ pi 180) (- rotangle angle-bullshit)))
	 (sinrot (sin true-rotangle))
	 (cosrot (cos true-rotangle)))
    (wcs:make-wcs-radec-tan
     :crval1 ra
     :crval2 dec
     :crpix1 (+  (* 0.5d0 naxis1) ;; start at chip center
		 (/ choffx pscale xflip -1)) ;; and add minus the offset 
     :crpix2 (+  (* 0.5d0 naxis2) ;; start at chip center
		 (/ choffy pscale yflip -1)) ;; and add minus the offset
     :cd1_1  (* pscale-deg xflip cosrot -1)
     :cd1_2  (* pscale-deg yflip sinrot +1)
     :cd2_1  (* pscale-deg xflip sinrot +1)
     :cd2_2  (* pscale-deg yflip cosrot +1)
     :equinox 2000d0)))
     

      


(defmethod insert-initial-wcs-for-instrument ((inst %magellan-imacs) fits-file
					       &key extension)
  (declare (ignore extension))
  (when (or t
	    (not (cf:read-wcs fits-file)))
    (cf:write-wcs (get-initial-wcs-for-fits fits-file)
		  fits-file)))



