;; reduction plan suitable for GMOS-N central chip fields
;; after processing by gmos-proc:combine-gmos-chips
;;


(in-package imred)



(defclass reduction-plan-gmos-n-e2v-chip2 (reduction-plan)
  ((inst-id-type :initform 'instrument-id:gmos-n-e2v-full-chip)
   (trimsec :initform #(1 1024 1 2304)) ;; already trimmed
   (trim :initform NIL)
   (zero-name :initform "BIAS")  ;
   (flat-basename :initform "FLAT")
   ;; filter2 has the normal broadband filters
   (flat-name   :initform "FLAT")
   (max-flat-counts :initform 80000) ;; saturation is 100K e- 
   (input-fits-patch-function :initform
			      'gmos-n-e2v-chip2-input-fits-patch-function)
   (output-fits-patch-function :initform
			       'gmos-n-e2v-chip2-output-fits-patch-function)))


(defmethod get-reduction-plan-for-instrument ((inst instrument-id:gmos-n-e2v-full-chip))
  (if (equalp (instrument-id:chip-id inst) "CHIP2") 
      'reduction-plan-gmos-n-e2v-chip2
      (error "GMOS single chips can be reduced only as CHIP2")))



(defun gmos-n-e2v-chip2-input-fits-patch-function (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
  (when (not (= (cf:read-fits-header fits-file "GAIN") 1))
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
    (cf:write-fits-header fits-file "OBSTYPE" true-obstype)))
    
	     

  

  
(defun gmos-n-e2v-chip2-output-fits-patch-function (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
  ;; create the MJD headers
  (let* ((date-string  (%gethead-or-error fits-file "DATE-OBS"))
	 (ut-string    (%gethead-or-error fits-file "UTSTART"))
	 (exptime      (%gethead-or-error fits-file "EXPTIME"))
	 (full-ut-string  (concatenate 'string date-string "T" ut-string))
	 (mjd-start
	   (multiple-value-bind (year month day hour min sec)
	       (astro-time:parse-ut-date-and-time-string full-ut-string)
	     (astro-time:calendar-date-to-mjd year month day hour min sec)))
	 (mjd-midpt (+ mjd-start (/ exptime (* 24d0 3600)))))
    
    
    (cf:write-fits-header  fits-file  "UTDATE" full-ut-string
			   :comment "UT of start of observation")
    (cf:write-fits-header  fits-file  "MJD" mjd-start 
			   :comment "start MJD of obs")
    (cf:write-fits-header  fits-file "MJDSTART" mjd-start 
			   :comment "start MJD of obs")
    (cf:write-fits-header  fits-file "MJDMID" mjd-midpt 
			   :comment "midpoint MJD of obs"))

  )
	 
  
