


(in-package imred)


;; reduction plan suitable for hct-hfosc
;;
(defclass reduction-plan-hct-hfosc (reduction-plan)
  ((inst-id-type :initform 'instrument-id:hct-hfosc)
   (trimsec :initform #(1 2048 1 2048))
   (zero-name :initform "BIAS")  ;
   (flat-basename :initform "FLAT")
   (max-flat-counts :initform 35000) ;; hope this is OK
   (input-fits-patch-function :initform 'hct-hfosc-input-fits-patch-function)
   (output-fits-patch-function :initform 'hct-hfosc-output-fits-patch-function)))


(defmethod get-reduction-plan-for-instrument ((inst instrument-id:hct-hfosc))
  (declare (ignorable inst))
  'reduction-plan-hct-hfosc)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun %fix-hfosc-gain (fits-file)
  (let* ((gainm (%gethead-or-error fits-file "GAINM"))
	 (gain (cond ((equalp gainm "HIGH")
		      1.22) ;; actually 1.21 for Amp B, but close enough
		     ((equalp gainm "LOW")
		      5.6)
		     (t
		      (error "Unknown gain keyword ~A for ~A" gainm fits-file)))))
    (cf:write-fits-header fits-file "GAIN" gain 
			  :comment "Gain: Table 1.2 of manual"))
  t)

#+nil
(defun %fix-hfosc-objtyp (fits-file)
  (let* ((object (string-upcase (format nil "~A" ;; sometimes object can be a number - ugh
					(%gethead-or-error fits-file "OBJECT" ))))
	 (objtyp
	   (cond ((search "BIAS" object) "BIAS")
		 ((search "FLAT" object) "FLATFIELD")
		 (t "OBJECT"))))
    (cf:write-fits-header fits-file "IMGTYP" objtyp 
			  :comment "BIAS/FLAT/OBJECT"))
  t)

;; new version - use IMRED, which already has all the heuristics for guessing the image type
;; based on common errors
(defun %fix-hfosc-objtyp (fits-file)
  (let ((imgtyp (instrument-id:get-object-type-for-fits fits-file)))
    (cf:write-fits-header fits-file "IMGTYP" (format nil "~A" imgtyp)
			  :comment "BIAS/FLAT/OBJECT/OTHER")))

(defun %fix-hfosc-date (fits-file)
    (let* ((date-string  (%gethead-or-error fits-file "DATE-OBS"))
	   (sec-into-day (%gethead-or-error fits-file "UT")) ;; seconds into day
	   (exptime  (%gethead-or-error fits-file "EXPTIME"))
	   (mjd-day-start ;; mjd of date-string
	     (multiple-value-bind (year month day hour min sec)
		 (astro-time:parse-ut-date-and-time-string date-string)
	       (astro-time:calendar-date-to-mjd year month day hour min sec)))
  	   (mjd-start (+ mjd-day-start ;; start of obs
			     (/ sec-into-day #.(* 3600 24d0))))
	   (mjd-midpt (+ mjd-start (/ exptime (* 24d0 3600))))
	   (full-ut-string (astro-time:mjd-to-ut-string mjd-start)))
      (cf:write-fits-header  fits-file  "UTDATE" full-ut-string
			     :comment "UT of start of observation")
      (cf:write-fits-header  fits-file  "MJD" mjd-start 
			     :comment "start MJD of obs")
      (cf:write-fits-header  fits-file "MJDSTART" mjd-start 
			     :comment "start MJD of obs")
      (cf:write-fits-header  fits-file "MJDMID" mjd-midpt 
			     :comment "midpoint MJD of obs"))
  t)


(defun %fix-hfosc-wcs (fits-file)
    (let* 
       ((ra-string  (%gethead-or-error fits-file "RA"))
	(dec-string (%gethead-or-error fits-file "DEC"))
	(ra (ra-dec:hms-string->deg ra-string))
	(dec (ra-dec:dms-string->deg dec-string))
	;; not in the headers, so we use manual
	(pixscale/arcsec 0.296d0)
	(pixscale (/ pixscale/arcsec 3600d0))
	(wcs
	  (wcs:make-wcs-radec-tan 
	   :crval1 ra :crval2 dec
	   :crpix1 1024d0 :crpix2 1024d0 
	   :cd1_1 (+ pixscale)
	   :cd2_2 (- pixscale)
	   :equinox 2000d0
	   )))
      (cf:delete-fits-header fits-file "CDELT1")
      (cf:delete-fits-header fits-file "CDELT2")
      (cf:write-fits-header fits-file "PIXSCALE" pixscale
			    :comment "pix in arcsec from manual")
      (cf:write-wcs wcs fits-file)
      t))
      
		   



;; the patching may often fail because HSC frequently omits crucial headers,
;; so we try to generate warnings instead of failures
(defmacro %with-hfosc-patch-warn (description fits &body body)
  `(multiple-value-bind (result error) ;; result is just T/NIL
       (ignore-errors (progn ,@body))
     (when (not result)
       (imred-log-warn ;; expects variable REDUCTION-PLAN to be defined in scope
	(format nil "~A patching of HSC file ~A failed: ~A" ,description ,fits error)))
     result))



	  
  



(defun hct-hfosc-input-fits-patch-function (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
  (%with-hfosc-patch-warn "Input-fits GAIN" fits-file (%fix-hfosc-gain fits-file))
  (%with-hfosc-patch-warn "Input-fits OBJTYP" fits-file  (%fix-hfosc-objtyp fits-file)))

;; not really clear why these all can't be moved to the INPUT patch
;; function
(defun hct-hfosc-output-fits-patch-function (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
  (%with-hfosc-patch-warn "Output-fits date" fits-file (%fix-hfosc-date fits-file))
  (%with-hfosc-patch-warn "Output-fits WCS" fits-file (%fix-hfosc-wcs fits-file)))

