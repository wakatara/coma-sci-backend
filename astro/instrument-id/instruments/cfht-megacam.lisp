

(in-package instrument-id)

;; parent class of megacam
(defclass %cfht-megacam (preprocessed imaging-instrument)
  ((name :initform "CFHT-MegaCam") ;; a string
   (aperture :initform 3.58)
   (observatory :initform "T14") ;; new exact MPC location
   ;; by default, turn off preprocessing unless Elixir tests positive
   (preprocessed-p    :initform nil )
   (wcs-origin        :initform nil) ;; will be "CFHT-ELIXIR" if processed
   ;;  NOTE - Elixir photometric calibration uses Landolt stars across many
   ;;         frames, not frame-by-frame fitting, so we don't trust it.
   ;;         Hence there is no PHOT-CALIB-ORIGIN for Elixir, ever
   (phot-calib-origin :initform nil)))


(defclass/inst cfht-megacam-one-chip (%cfht-megacam onechip)
  ())

(defclass/inst cfht-megacam-one-chip/fzip (cfht-megacam-one-chip fzipped)
  ())

(defclass/inst cfht-megacam-array (%cfht-megacam multichip)
  ())

(defclass/inst cfht-megacam-array/fzip (cfht-megacam-array fzipped)
  ())

;; a one chip file with the image as an extension, eg when extracted
;; from a compressed chip array.  It must have proper cfht headers in
;; primary extension (DETECTOR, FILTER, OBJECT, OBSTYPE, MJDATE,
;; EXTNAME, EXPTIME TSECA, TSECB)
;; and an extra header EXTRACTED.CHIP
(defclass/inst cfht-megacam-one-chip-ext (cfht-megacam-one-chip)
  ())

(defclass/inst cfht-megacam-one-chip-ext/fzip (cfht-megacam-one-chip-ext fzipped)
  ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|

We don't use build on photoometric calibration because CFHT seems to
use Landolt stars rather than frame-by-frame photometry.  So it will
be sensitive to clouds.  

|#

#+do-not-use-cfht-zp
(defmethod get-calibrated-zeropoint-for-instrument ((inst %cfht-elixir) fits-file
						     &key extension
						       (flux-units :adu)
						       (exposure   :one-second))
  (declare (type (member :adu :electrons) flux-units)
	   (type (member :one-second :exptime) exposure))
  (let ((next (if (typep inst 'onechip)
		  (get-image-extension-for-onechip-instrument inst fits-file)
		  extension)))
    (when (not next)
      (error "Extension not specified for multichip fits in get-calibrated-zeropoint-for-instrument"))
      
  (cf:maybe-with-open-fits-file (fits-file ff)
    (cf:with-fits-extension (ff next)
      (let ((c  (cf:read-fits-header ff "PHOT_C"))
	    (cs (cf:read-fits-header ff "PHOT_CS"))
	    (k  (cf:read-fits-header ff "PHOT_K"))
	    (airmass (cf:read-fits-header ff "AIRMASS"))
	    (gain (cf:read-fits-header ff "GAIN"))
	    (exptime (cf:read-fits-header ff "EXPTIME"))
	    zp zperr)
	;; from MegaCam header
	;; Mag = -2.5 log(flux) +
	;;       + 2.5 log (exptime) + C + K (1-AIRMASS)
	;;       + [color terms we won't have so we ignore]
	(when (and c k cs airmass exptime gain)
	  ;; WARNING - these corrections are probably WRONG (flipped, or sign)
	  ;; and we don't know if the flux is in ADU or electrons!
	  (let ((exptime-corr- (if (eq exposure :one-second)
				  0d0
				  (* 2.5d0 (log (max 0.00001 exptime) 10))))
		(gain-corr (if (eq flux-units :adu)
			      0d0
			      (log (max 0.000001d0 gain) 10d0))))
	    ;;
	    (setf zp (+ c exptime-corr gain-corr
			(* k (- 1d0 airmass))))
	    (setf zperr cs)))
	(if zperr
	    (values zp zperr)
	    ;; fall back to our own calib
	    (call-next-method)))))))
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod patch-extracted-extension-headers ((inst cfht-megacam-array)
					      fits-file extname/extnum output-fits-file)
  (cf:with-open-fits-file (output-fits-file ff-out :mode :io)
    (let* ((extension (cf:fits-file-num-hdus ff-out))
	   (cerror  (cf:read-fits-header  ff-out "CERROR" :extension extension)))
      ;; UGH - CFHT Elixir is bad at quantifying WCS fit.
      (when (and cerror (realp cerror) (< cerror 0.5))
	(cf:write-fits-header ff-out "WCSFITOK" t
			      :comment "from CERROR<0.5 - not a good test."
			      :extension extension)))))




(defmethod get-critical-headers-for-instrument ((inst %cfht-megacam) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("DETECTOR" "FILTER" "OBJECT" "OBSTYPE" "MJDATE"
		"MJD-OBS" "RA" "DEC" "RA_DEG" "DEC_DEG"  "HA"
		"OBJRA" "OBJDEC" "DATE-OBS" "UTC-OBS" "SIDTIME" "LST-OBS"
		"EXPTIME" "TSECA" "TSECB" "OBSID"
		"ORIGIN" "EXTNAME" "EXTVER" "DETECTOR" "OBSTYPE"
		"SHUTOPEN" "SHUTCLOS" "MJDATE" "TSECA" "TSECB")
    (call-next-method))
   :test 'equalp))

(defmethod get-image-extension-for-onechip-instrument
    ((inst  cfht-megacam-one-chip-ext) fits-file)
  (declare (ignore inst fits-file))
  2)  ;; 2ND ext has the image

;; fzip puts data into 2nd extension
(defmethod get-image-extension-for-onechip-instrument
    ((inst  cfht-megacam-one-chip/fzip) fits-file)
  (declare (ignore inst fits-file))
  2)  ;; 2ND ext has the image



(defun %get-megacam-fits-chip-id (fits-file &key (extension 1))
  (or (cf:read-fits-header fits-file "EXTNAME" :extension extension)
      ;; older images didn't have extname, but EXTVER should work
      (let ((extver (cf:read-fits-header fits-file "EXTVER" :extension extension)))
	(format nil "ccd~2,'0D" extver))))




;; after determining INST, determine if FITS-FILE was processed
;; by ELIXIR and if WCS was fit.
(defun %megacam-note-elixir-for-inst (inst fits-file)
  (declare (type %cfht-megacam inst))
    (cf:maybe-with-open-fits-file (fits-file ff)
      (let* ((next (cf:fits-file-num-hdus ff))
	     (n-hdu  (if (= next 1) 1 2))
	     ;; the elixir version
	     (el-sys (cf:read-fits-header ff "EL_SYS" :extension n-hdu))
	     ;; the operations performed by imred
	     (imred-op (cf:read-fits-header ff "IMRED_OP" :extension n-hdu))
	     (processed-p
	       (and imred-op
		    (search "BIAS" imred-op)
		    (search "FLAT-FIELD" imred-op)))
	     ;; the 'scatter of astrometric solution' header seems to be
	     ;; the only indicator of an astrometric fit
	     (elix-cerror   (cf:read-fits-header ff "CERROR" :extension n-hdu))
	     (elix-string (when el-sys
			    (format nil "ELIXIR-~A" el-sys))))
	;;
	(when processed-p
	  (setf (preprocessed-p inst) t)
	  (when elix-cerror
	    (setf (preproc-wcs-origin inst) elix-string))
	  ;; the photometry is always per-night so we don't use it
	  (setf (preproc-phot-calib-origin inst) nil))

	inst)))
	     
			      
    
  

(defun %megacam-identify-instrument (fits-file)
  (cf:maybe-with-open-fits-file (fits-file ff)
    (let* ((fzipped  (fits-file-is-fzipped fits-file))
	   (nhdu (cf:fits-file-num-hdus ff))
	   ;; detector might be in ext 2 for certain old chip extractions
	   (detector (or (cf:read-fits-header fits-file "DETECTOR" :extension 1)
			      (if (> nhdu 1)
				  (cf:read-fits-header fits-file "DETECTOR" :extension 2))))
	   (has-primary (eql 0 (cf:read-fits-header ff "NAXIS" :extension 1)))
	   (chip-id (when (<= nhdu 2)
		      (%get-megacam-fits-chip-id ff :extension nhdu))))
      (when (equalp detector "MegaCam")
	(let ((inst
		(cond ((member (1- nhdu) '(36 40)) ;; old and new
		       (if fzipped
			   (make-instance 'cfht-megacam-array/fzip)
			   (make-instance 'cfht-megacam-array)))
		      ((= nhdu 1)
		       (if fzipped
			   (make-instance 'cfht-megacam-one-chip/fzip
					  :chip-id chip-id)
			   (make-instance 'cfht-megacam-one-chip
					  :chip-id chip-id)))
		      ((and (= nhdu 2) has-primary)
		       (if fzipped
			   (make-instance 'cfht-megacam-one-chip-ext/fzip :chip-id chip-id)
			   (make-instance 'cfht-megacam-one-chip-ext :chip-id chip-id)))
		      (t
		       (error "Unrecognized Megacam fits file ~A" fits-file)))))
	  (%megacam-note-elixir-for-inst inst fits-file)
	  inst)))))
	    
	
				  

#+nil ;; old version
(defun %megacam-identify-instrument (fits-file)
  (let* ((fzipped  (fits-file-is-fzipped fits-file))
	 ;; if a mosaic or un-fzipped, detector is in ext 1
	 (detector-ext1 (cf:read-fits-header fits-file "DETECTOR" :extension 1))
	 ;; if no detector in ext0, and fzipped, might be fzipped single file
	 (ext (if (and fzipped (not detector-ext1)) 2 1)))
    ;;
    (when (equalp (or detector-ext1 (cf:read-fits-header fits-file "DETECTOR" :extension ext))
		  "MegaCam")
      (let* ((naxis (%gethead-or-error fits-file "NAXIS" :extension ext))
	     (nextend (cf:read-fits-header fits-file "NEXTEND" :extension ext))
	     (extname (cf:read-fits-header fits-file "EXTNAME" :extension ext))
	     (extracted-chip
	       (or ;; old version before generalizing extraction
		(cf:read-fits-header fits-file "EXTRACTED.CHIP" :extension ext)
		extname)))
	(print (list fzipped detector-ext1 ext naxis nextend extracted-chip ) )
	(ext ((and (equalp naxis 0) 
		    (or (equalp nextend 36)
			(equalp nextend 40))) ;; 41 was older, or something?
	       (if fzipped
		   (make-instance 'cfht-megacam-array/fzip)
		   (make-instance 'cfht-megacam-array)))
	      ((and (equalp naxis 0) extracted-chip)
	       (if fzipped
		   (make-instance 'cfht-megacam-one-chip-ext/fzip)
		   (make-instance 'cfht-megacam-one-chip-ext)))
	      ((equalp naxis 2)
	       (if fzipped
		   (make-instance 'cfht-megacam-one-chip/fzip
				  :chip-id (%get-megacam-fits-chip-id fits-file))
		   (make-instance 'cfht-megacam-one-chip
				  :chip-id (%get-megacam-fits-chip-id fits-file))))
	      (t
	       (error "Unknown Megacam fits file ~A" fits-file)))))))

(%add-instrument-id-function '%megacam-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-standard-filter-for-instrument ((inst %cfht-megacam) fits-file)
  (let ((filter (%gethead-info inst fits-file "FILTER")))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not found in image ~A" fits-file))
    (cond ((eql 0 (search "u." filter)) :usdss)
	  ((eql 0 (search "g." filter)) :gsdss)
	  ((eql 0 (search "r." filter)) :rsdss)
	  ((eql 0 (search "i." filter)) :isdss)
	  ((eql 0 (search "z." filter)) :zsdss)
	  ;; the undocumented wideband filter spanning g,r,i
	  ((eql 0 (search "gri." filter)) :gri-cfht-megacam)
	  ;; fixme - there are more filters
	  (t NIL))))


(defmethod get-exptime-for-instrument ((inst %cfht-megacam) fits-file)
  (%gethead-info inst fits-file "EXPTIME" :throw-error nil))


(defmethod get-object-for-instrument ((inst %cfht-megacam) fits-file)
  (%gethead-info inst fits-file "OBJECT" :throw-error nil))

(defmethod get-object-type-for-instrument ((inst %cfht-megacam) fits-file)
  (let ((obj-type (%gethead-info inst  fits-file "OBSTYPE" :throw-error nil)))
    (cond ((equalp obj-type "BIAS") :BIAS)
	  ((equalp obj-type "FLAT") :FLAT)
	  ((equalp obj-type "OBJECT") :OBJECT)
	  (T :OTHER))))

(defmethod get-mjd-start-for-instrument ((inst %cfht-megacam) fits-file)
  (let ((stime1 (%gethead-info inst fits-file "SHUTOPEN" :throw-error nil)))
    (cond ((not stime1)
	   (%gethead-or-error fits-file "MJDATE")) ;; less accurate
	  (t
	   (cond
	     ((floatp stime1)
	      stime1)
	     ((stringp stime1)
	      (astro-time:parse-ut-date-and-time-string-to-mjd stime1))
	     (t
	      (error
	       "CFHT SHUTOPEN header <~A> is neither MJD float nor UT string."
	       stime1)))))))


;; apparently this is the most accurate version in the headers
(defmethod get-mjd-mid-for-instrument ((inst %cfht-megacam) fits-file)
  (let* ((stime1 (%gethead-info inst fits-file "SHUTOPEN" :throw-error nil))
	 (stime2 (%gethead-info inst fits-file "SHUTCLOS" :throw-error nil))
	 ;; sometimes SHUTOPEN, SHUTCLOS is 'Null'
	 (utstime1 (ignore-errors
		    (astro-time:parse-ut-date-and-time-string-to-mjd stime1)))
	 (utstime2 (ignore-errors
		    (astro-time:parse-ut-date-and-time-string-to-mjd stime2))))
    
    (cond ((not (and  utstime1 utstime2))
	   (+ (%gethead-or-error fits-file "MJDATE") ;; less accurate
	      (/ (get-exptime-for-instrument inst fits-file) 24d0 3600d0 2)))
	  (t
	   (* 0.5d0
	      (+ utstime1 utstime2))))))
	       




;(defmethod get-gain-for-instrument ((inst %cfht-megacam) fits-file)   )

;; megacam has two trimsecs, one for each amp, that have no separation between them.
(defmethod get-datasec-for-instrument ((inst %cfht-megacam) fits-file
				       &key extension)
  (let* ((the-extension
	   (or extension
	       (cond
		 ((typep inst 'cfht-megacam-one-chip/fzip) 2)
		 ((typep inst 'cfht-megacam-one-chip) 1)
		 ((typep inst 'cfht-megacam-one-chip-ext/fzip) 2)
		 ((typep inst 'cfht-megacam-one-chip-ext) 2)
		 ((typep inst 'cfht-megacam-array)
		      (err-if-not-image-at-extension 
		       inst fits-file
		       "get-datasec-for-instrument (cfht-megacam)" extension)
		      extension))))
	 ;;
	 (tsa (cf:parse-image-section-string
		(%gethead-or-error
		 fits-file "TSECA"
		 :extension the-extension)))
	 (tsb (cf:parse-image-section-string
	       (%gethead-or-error fits-file "TSECB"
				  :extension the-extension))))
    (vector (aref tsa 0) (aref tsb 1)
	    (aref tsa 2) (aref tsb 3)))) 

;(defmethod get-chip-id-for-instrument ((inst cfht-megacam-one-chip) fits-file) ..)



