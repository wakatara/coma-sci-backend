;; reduction plan for GMOS-N array consisting of full or half-chips

;; HALF-CHIPS are present when the image has not been pre-processed to merge chips

;; FULL-CHIPS are when there has been pre-processing



(in-package imred)


      
(defclass %reduction-plan-gmos-array (reduction-plan) ;; parent class
  ((inst-id-type :initform nil)
   (trim :initform t) 
   (zero-name :initform "BIAS")  ;
   (flat-basename :initform "FLAT")
   ;; filter2 has the normal broadband filters
   (flat-name   :initform "FLAT")
   (min-flat-counts :initform 4000)  ;; saturation is 100K e-
   (max-flat-counts :initform 80000) ;; saturation is 100K e-
   (min-final-flat-value :initform 0.0001)
   (input-fits-patch-function :initform
			      'gmos-array-input-fits-patch-function)
   (output-fits-patch-function :initform
			       'gmos-array-output-fits-patch-function)))

(defclass reduction-plan-gmos-full-chip-array (%reduction-plan-gmos-array)
  ((inst-id-type :initform 'instrument-id::%gmos-full-chip-array)))

(defclass reduction-plan-gmos-frac-chip-array (%reduction-plan-gmos-array)
  ((inst-id-type :initform 'instrument-id::%gmos-frac-chip-array)))


(defmethod get-reduction-plan-for-instrument
    ((inst instrument-id::%gmos-frac-chip-array))
  (declare (ignorable inst)) 
  'reduction-plan-gmos-frac-chip-array)



(defmethod get-reduction-plan-for-instrument
    ((inst instrument-id::%gmos-full-chip-array))
  (declare (ignorable inst)) 
  'reduction-plan-gmos-full-chip-array)


;; the four outermost extensions are vignetted, so we fudge them to the
;; median flux value of the center, so they make it through the pipeline.
(defun %fudge-gmos-hamamatsu-outriggers (fits-file)
  (let ((center-ext 7)
	(outrigger-exts '(2 3 12 13)))  ;; edges of side chips
    (cf:with-open-fits-file (fits-file ff :mode :io)
      (let ((center-med (progn
			  (cf:move-to-extension ff center-ext)
			  (imutils:compute-sampled-image-median-and-sigma
			   (cf:image-section-data
			    (cf:read-image-section ff))
			   30000))))
	(loop for ext in outrigger-exts
	      for imsec = (progn
			    (cf:move-to-extension ff ext)
			    (cf:read-image-section ff))
	      for data of-type imutils:image =  (cf:image-section-data imsec)
	      do (imutils:fill-image data center-med)
		 (cf:write-back-image-section imsec)
		 (cf:write-fits-comment
		  ff
		  "This extension is VIGNETTED and the flat is SIMULATED") )))))
			    
	

;; the 'full-chip' files should have been processed to gain=1 already
(defun gmos-array-input-fits-patch-function (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
  (when (and (eq (type-of (instrument-id:identify-instrument fits-file))
		 'instrument-id::%gmos-full-chip-array)
	      (not (= (cf:read-fits-header fits-file "GAIN") 1)))
    (error "Gain is not 1.0 in ~A - should have been set to 1 in pre-processing"
	   fits-file))
  (let* ((obstype (cf:read-fits-header fits-file "OBSTYPE"))
	 (object  (cf:read-fits-header fits-file "OBJECT"))
	 (true-obstype
	   (cond ((and (equalp obstype "OBJECT")
		       (equalp object  "Twilight"))
		  "FLAT")
		 (t
		  obstype))))
    (when (not (equalp obstype true-obstype))
      (cf:write-fits-comment fits-file
			     (format nil "Changed OBSTYPE from ~A to ~A"
				     obstype true-obstype)))

    ;; if a Hamamatsu chip, then fuge outboard amps of raw flats to have
    ;; same flux as center; otherwise vignetting will cause reduction to fail
    (when (and (typep (instrument-id:identify-instrument fits-file)
		      'instrument-id:%gmos-hamamatsu)
	       (eq (instrument-id:get-object-type-for-fits fits-file) :flat))
      (%fudge-gmos-hamamatsu-outriggers fits-file))
    
    (cf:write-fits-header fits-file "OBSTYPE" true-obstype)))
    
	     

  
(defun %make-gmos-output-fits-name (gmos-fits suffix)
  (let* ((str (namestring gmos-fits))
	 (ndot (position #\. str  :from-end t))
	 (base (if ndot
		   (subseq str 0 ndot)
		   str)))
    (concatenate 'string base suffix)))
  
(defun gmos-array-output-fits-patch-function (fits-file reduction-plan)
  ;; this one has more graceful failure modes because the processing is complicated
  ;; processing of one file doesn't halt entire pipeline
  (let ((current-action nil)) ;; variable to hold what we're doing
    (flet ((patch-it () ;; toplevel function to wrap in ignore-errors
	     ;; create the MJD headers
	     (let* ((date-string  (%gethead-or-error fits-file "DATE-OBS"))
		    (ut-string    (%gethead-or-error fits-file "UTSTART"))
		    (exptime      (%gethead-or-error fits-file "EXPTIME"))
		    (full-ut-string  (concatenate 'string date-string "T" ut-string))
		    (mjd-start
		      (multiple-value-bind (year month day hour min sec)
			  (astro-time:parse-ut-date-and-time-string full-ut-string)
			(astro-time:calendar-date-to-mjd year month day hour min sec)))
		    (mjd-midpt (+ mjd-start (/ exptime (* 24d0 3600))))
		    (id (instrument-id:identify-instrument fits-file)))
	       
	       (setf current-action "FIXING-HEADERS")
	       (cf:write-fits-header  fits-file  "UTDATE" full-ut-string
				      :comment "UT of start of observation")
	       (cf:write-fits-header  fits-file  "MJD" mjd-start 
				      :comment "start MJD of obs")
	       (cf:write-fits-header  fits-file "MJDSTART" mjd-start 
				      :comment "start MJD of obs")
	       (cf:write-fits-header  fits-file "MJDMID" mjd-midpt 
			   :comment "midpoint MJD of obs")
	       
	       ;; set the sky levels inside and across chips to be equal, using
	       ;; thin boundary strips\
	       (setf current-action "EQUALIZING-SKYLEVELS-BY-EXTENSION")
	       (cond ((typep id 'instrument-id:%gmos-e2v)
		      (%gmos-array-equalize-skylevels-e2v fits-file))
		     ((typep id 'instrument-id:%gmos-hamamatsu)
		      (%gmos-array-equalize-skylevels-hamamatsu fits-file))
		     (t
		      (error "GMOS fits is neither E2V nor Hamamatsu"))))
	     
	     
	     ;; finally combine the chips and mosaic
	     
	     (setf current-action "COMBINING-GMOS-CHIP-EXTENSIONS")
	     (let ((combined-chip-file 
		     (%make-gmos-output-fits-name fits-file "c.fits")))
	       
	       (gmos-proc:combine-gmos-chips 
		fits-file
		:fits-out combined-chip-file
		:overwrite t
		:debias nil
		:fix-relative-gain nil
		:normalize-gain nil
		:processed t ;; tell it we're working on processed 1/2 chips
		:only-central-chip nil)

	       (setf current-action "MOSAIC GMOS CHIPS")
	       (gmos-proc:mosaic-gmos-chips
		combined-chip-file
		(%make-gmos-output-fits-name combined-chip-file "_mosaic.fits")
		:overwrite t))
	     t))

      (multiple-value-bind (val err)
	  (ignore-errors (patch-it))
	(when (not val)
	  (logger:writelog
	   (reduction-plan-logger reduction-plan)
	   (format nil "ERROR in final patch function performing ~A: ~A" current-action err)))))))
			   

	 
 


(defun %gmos-array-equalize-skylevels-e2v (fits-file)
  (cf:with-open-fits-file (fits-file ff :mode :io)
    (flet ((get-ext (n-ext)
	     (cf:move-to-extension ff n-ext)
	     (cf:read-image-section ff)))
      (let ((imsec1-l (get-ext 3))
	    (imsec1-r (get-ext 2))
	    (imsec2-l (get-ext 5))
	    (imsec2-r (get-ext 4))
	    (imsec3-l (get-ext 6))
	    (imsec3-r (get-ext 7)))
	;;
	;; rescale center chip halves
	(gmos-proc:fix-relative-gain-between-amps-using-boundary
	 (cf:image-section-data imsec2-l)
	 (cf:image-section-data imsec2-r)
	 :nwidth 5 ;; this strip can be thicker because no gradients in center
	 :frac-vignetted 0.02
	 :rescale-side :right) ;; side not important for center chip
	;;
	;; right side of left chip with left side of center chip
	(gmos-proc:fix-relative-gain-between-amps-using-boundary
	 (cf:image-section-data imsec1-r)
	 (cf:image-section-data imsec2-l)
	 :nwidth 3  
	 :frac-vignetted 0.05
	 :rescale-side :left) ;; sidecar is scaled
	;;
	;; left side of left chip with right side of left chip
	(gmos-proc:fix-relative-gain-between-amps-using-boundary
	 (cf:image-section-data imsec1-l)
	 (cf:image-section-data imsec1-r)
	 :nwidth 3 
	 :frac-vignetted 0.12 ;; there's vignetting here as the FOV narrows
	 :rescale-side :left) ;; far side of sidecar is scaled
	;;
	;; left side of right chip with left side of center chip
	(gmos-proc:fix-relative-gain-between-amps-using-boundary
	 (cf:image-section-data imsec2-r)
	 (cf:image-section-data imsec3-l)
	 :nwidth 3
	 :frac-vignetted 0.05
	 :rescale-side :right) ;; sidecar is scaled
	;;
	;; right side of right chip with left side of right chip
	(gmos-proc:fix-relative-gain-between-amps-using-boundary
	 (cf:image-section-data imsec3-l)
	 (cf:image-section-data imsec3-r)
	 :nwidth 3
	 :frac-vignetted 0.12 ;; there's vignetting here as the FOV narrows
	 :rescale-side :right)  ;; far side of sidecar is scaled

	(loop for imsec in (list imsec1-l imsec1-r
				 imsec2-l imsec2-r
				 imsec3-l imsec3-r)
	      for iext in '(3 2 5 4 6 7) 
	      do (cf:move-to-extension ff iext)
		 (cf:write-back-image-section imsec))))))


(defun %gmos-array-equalize-skylevels-hamamatsu (fits-file)
  (cf:with-open-fits-file (fits-file ff :mode :io)
    (let ((imsec-list (loop for i from 2 to 13
			    collect 
			    (progn (cf:move-to-extension ff i)
				   (cf:read-image-section ff))))
	  ;; list of scalings as (ext-left ext-right side-corrected width )
	  ;; chips extensions are:
	  ;; [2 3 4 5] [6 7 8 9] [10 11 12 13]
	  (slist
	    '(
	      ;; make center chip uniform
	      (6  7 :right  5) ;; use a wider strip in the img center (no gradients)
	      (7  8 :right  5)
	      (8  9 :right  5)
	      ;; normalize left chip using center, then rest of left
	      (5  6 :left   10)  ;; there can be a band of NaNs at edge of chip
	      (4  5 :left   3)
	      (3  4 :left   3)
	      (2  3 :left   3)
	      ;; normalize right chip using center, then rest of right
	      (9  10 :right 10)  ;; there can be a band of NaNs at edge of chip
	      (10 11 :right 3)
	      (11 12 :right 3)
	      (12 13 :right 3))))

      (loop for sset in slist
	    for il = (first sset) and ir = (second sset) and side = (third sset)
	    and nwidth = (fourth sset)
	    for imsec-l = (nth (- il 2) imsec-list)
	    for imsec-r = (nth (- ir 2) imsec-list)
	    do
	       (gmos-proc:fix-relative-gain-between-amps-using-boundary 
		(cf:image-section-data imsec-l)
		(cf:image-section-data imsec-r) 
		:frac-vignetted 0.05 ;; use a single value
		:rescale-side side
		:nwidth nwidth))
	

      (loop for imsec in imsec-list
	    for iext from 2 to 13
	    do (cf:move-to-extension ff iext)
	       (cf:write-back-image-section imsec)))))
