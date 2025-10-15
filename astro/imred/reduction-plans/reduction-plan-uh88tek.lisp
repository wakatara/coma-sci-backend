

(in-package imred)


;; reduction plan suitable for UH 88
;;
(defclass reduction-plan-uh88-tek (reduction-plan)
  ((inst-id-type :initform 'instrument-id:%uh88-tek)
   (trimsec :initform #(25 2040 7 2040))
   (max-flat-counts :initform 26000)
   (min-flat-counts :initform 4000)
   (zero-name :initform "BIAS")  ;
   (flat-basename :initform "FLAT")
   (output-fits-patch-function :initform 'uh88-final-fits-patch-function)))

(defmethod get-reduction-plan-for-instrument ((inst instrument-id:%uh88-tek))
  (declare (ignorable inst))
  'reduction-plan-uh88-tek)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass reduction-plan-uh88-uh88-ll-hi-z-7-5-2-ccd (reduction-plan)
  ((inst-id-type :initform 'instrument-id:uh88-ll-hi-z-7-5-2-ccd)
   (trimsec :initform #(3 1023 4 2048))
   (max-flat-counts :initform 26000)
   (min-flat-counts :initform 4000)
   (zero-name :initform "BIAS")  ;
   (flat-basename :initform "FLAT")
   (output-fits-patch-function :initform 'uh88-final-fits-patch-function)))

(defmethod get-reduction-plan-for-instrument ((inst instrument-id:uh88-ll-hi-z-7-5-2-ccd))
  (declare (ignorable inst))
  'reduction-plan-uh88-uh88-ll-hi-z-7-5-2-ccd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass reduction-plan-uh88-orbit-ccd (reduction-plan)
  ((inst-id-type :initform 'instrument-id:uh88-orbit-ccd)
   (trimsec :initform #(21 2066 2 2048))
   (max-flat-counts :initform 26000)
   (min-flat-counts :initform 4000)
   (zero-name :initform "BIAS")  ;
   (flat-basename :initform "FLAT")
   (output-fits-patch-function :initform 'uh88-final-fits-patch-function)))

(defmethod get-reduction-plan-for-instrument ((inst instrument-id:uh88-orbit-ccd))
  (declare (ignorable inst))
  'reduction-plan-uh88-orbit-ccd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the function to patch an output fits file
;; * create MJD header

(defun uh88-final-fits-patch-function (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
  ;; create the MJD headers
  (let* ((ut-string (%gethead-or-error fits-file "UT"))
	 ;; UH88 had bogus dates DD/MM/YY in the past, so we fix it
	 ;;    using existing instrument-id routine
	 (date-string  (instrument-id::%fix-old-uh88-date-string
			(%gethead-or-error fits-file "DATE-OBS")))
	 (exptime (%gethead-or-error fits-file "EXPTIME"))
	 (full-ut-string  (format nil "~AT~A" date-string ut-string))	 
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
  ;; do other stuff here

  )
	 
  
