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
(defclass reduction-plan-suprime-cam (reduction-plan)
  ((inst-id-type :initform 'instrument-id:subaru-suprime-cam-one-chip)
   (trim ;; don't trim - this is done before running imred
    :initarg :trim 
    :initform NIL
    :accessor reduction-plan-trim)
   (flat-name 
    :initarg :flat-name
    :initform "SKYFLAT" ;; 
    :accessor reduction-plan-flat-name)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; to set invalid pixels when we read raw pixels
   (saturation-value ;; in ADU in raw frame
    :initarg :saturation-value
    :initform 65535 ;; generally, 16 bit
    :accessor reduction-plan-saturation-value)
   ;; input value marking invalid pixels
   (invalid-pixel-value
    :initarg :invalid-pixel-value 
    :initform 65535
    :accessor reduction-plan-invalid-pixel-value)
   ;; output value for null (no information) pixels
   (output-null-pixel-value  ;; FIXME: this SHOULD be NaN but we don't trust IEEE floats in all lisps
    :initarg :output-null-pixel-value
    :initform (make-single-float-nan)
    :accessor reduction-plan-output-null-pixel-value)
   (zero-name :initform "BIAS")  ;
   (flat-basename :initform "SKYFLAT_")
   (min-flat-counts  ;; in ADU
    :initarg :min-flat-counts
    :initform 5000
    :accessor reduction-plan-min-flat-counts)
   (max-flat-counts  ;; in ADU
    :initarg :max-flat-counts
    :initform 24000 
    :accessor reduction-plan-max-flat-counts)
   (output-fits-patch-function :initform 'suprime-cam-final-fits-patch-function)))


(defmethod get-reduction-plan-for-instrument
    ((inst instrument-id:subaru-suprime-cam-array))
  (declare (ignorable inst))
  'reduction-plan-suprime-cam)

(defmethod get-reduction-plan-for-instrument
    ((inst instrument-id:subaru-suprime-cam-one-chip))
  (declare (ignorable inst))
  'reduction-plan-suprime-cam)



;; the function to patch an output fits file
;; * create MJD header

(defun suprime-cam-final-fits-patch-function (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
  ;; create the MJD headers
  (cf:with-open-fits-file ((fullfile fits-file) ff :mode :io)
    (loop 
       for iext below 10
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
		 
		 
	 
  
 
