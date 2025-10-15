;; reduction plan suitable for Surpime-Cam - this assumes that 
;; SuprimeCam routines have already been run to merge chips and
;; perform overscan subtraction
;;
;;  (require 'suprime-cam-fix)
;;  (suprime-cam-fix:merge-chips-in-directory 
;;    	     "/dir/with/suprime-cam/images/"
;;           :outdir "/output/dir")




(in-package imred)



;;
(defclass reduction-plan-hypersuprime-cam (reduction-plan)
  ((inst-id-type :initform 'instrument-id:subaru-hypersuprime-cam-one-chip)
   (trim ;; don't trim - this is done before running imred
    :initform NIL)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; to set invalid pixels when we read raw pixels
   (saturation-value ;; in ADU in raw frame
    :initform 65535) ;; generally, 16 bit
   ;; input value marking invalid pixels
   (invalid-pixel-function
    :initform (lambda (x) (declare (ignore x)) nil)
    ;; it seems zero happen?
    ;;(lambda (x) (declare (type single-float x)) (= x 0.0)))
    )
   ;; output value for null (no information) pixels
   (output-null-pixel-value  
    :initform (make-single-float-nan))
   (zero-name :initform "BIAS")  ;
   (flat-basename :initform "FLAT_")
   (min-flat-counts  ;; in ADU
    :initform 5000)
   (max-flat-counts  ;; in ADU
    :initform 24000)
   (output-fits-patch-function
    :initform 'hypersuprime-cam-final-fits-patch-extension)))


(defmethod get-reduction-plan-for-instrument
    ((inst instrument-id:subaru-hypersuprime-cam-array))
  (declare (ignorable inst))
  'reduction-plan-hypersuprime-cam)


(defmethod get-reduction-plan-for-instrument
    ((inst instrument-id:subaru-hypersuprime-cam-one-chip))
  (declare (ignorable inst))
  'reduction-plan-hypersuprime-cam)
  





;; the function to patch an output fits file
;; * create MJD header


;; patch for a single extension fits file (chip)
(defun hypersuprime-cam-final-fits-patch-extension (fits-file reduction-plan)
  (declare (ignore reduction-plan))
  ;; create the MJD headers
  (cf:with-open-fits-file ((fullfile fits-file) ff :mode :io)
    (let* ((ut-string (%gethead-or-error fits-file "UT"))
	   (date-string  (%gethead-or-error fits-file "DATE-OBS"))
	   (full-ut-string  (format nil "~AT~A" date-string ut-string))
	   (mjd-start (%gethead-or-error fits-file "MJD-STR"))
	   (mjd-end (%gethead-or-error fits-file "MJD-END"))
	   (mjd-midpt (* 0.5d0 (+ mjd-start mjd-end))))
      (progn
	(cf:write-fits-header  ff  "UTDATE" full-ut-string
			       :comment "UT of start of observation")
	(cf:write-fits-header  ff  "MJD" mjd-start 
			       :comment "start MJD of obs")
	(cf:write-fits-header  ff "MJDSTART" mjd-start 
			       :comment "start MJD of obs")
	(cf:write-fits-header  ff "MJDMID" mjd-midpt 
			       :comment "midpoint MJD of obs")))))
		 

;; multi-extension version (full array - we may never see this in practice)
(defun hypersuprime-cam-final-fits-patch-function (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
  ;; create the MJD headers
  (cf:with-open-fits-file ((fullfile fits-file) ff :mode :io)
    (loop 
       for iext below 112 ;; 112 extensions (chips)
       for ext = (format nil "CHIP~D" iext)
       do 
	 (cf:move-to-extension ff ext)
	 (let* ((ut-string (%gethead-or-error fits-file "UT"))
		(date-string  (%gethead-or-error fits-file "DATE-OBS"))
		(full-ut-string  (format nil "~AT~A" date-string ut-string))
		(mjd-start (%gethead-or-error fits-file "MJD-STR"))
		(mjd-end (%gethead-or-error fits-file "MJD-END"))
		(mjd-midpt (* 0.5d0 (+ mjd-start mjd-end))))
	   (progn
	     (cf:write-fits-header  ff  "UTDATE" full-ut-string
				    :comment "UT of start of observation")
	     (cf:write-fits-header  ff  "MJD" mjd-start 
				    :comment "start MJD of obs")
	     (cf:write-fits-header  ff "MJDSTART" mjd-start 
				    :comment "start MJD of obs")
	     (cf:write-fits-header  ff "MJDMID" mjd-midpt 
				    :comment "midpoint MJD of obs"))))))
		 
		 
	 
  
 
