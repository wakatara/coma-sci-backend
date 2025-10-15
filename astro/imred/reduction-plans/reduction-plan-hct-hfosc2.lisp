


(in-package imred)


;; reduction plan suitable for hct-hfosc2
;;
(defclass reduction-plan-hct-hfosc2 (reduction-plan)
  ((inst-id-type :initform 'instrument-id:hct-hfosc2)
   (trimsec :initform #(1 2048 1 2048))
   (zero-name :initform "BIAS")  ;
   (flat-basename :initform "FLAT")
   ;; hope this is OK - they seem to go high, and images are 32 bits
   ;; stars don't saturate at 200K counts
   (max-flat-counts :initform 80000) 
   (input-fits-patch-function :initform 'hct-hfosc2-input-fits-patch-function)
   (output-fits-patch-function :initform 'hct-hfosc2-output-fits-patch-function)))


(defmethod get-reduction-plan-for-instrument ((inst instrument-id:hct-hfosc2))
  (declare (ignorable inst))
  'reduction-plan-hct-hfosc2)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the patching may often fail because HSC frequently omits crucial headers,
;; so we try to generate warnings instead of failures
(defmacro %with-hfosc2-patch-warn (description fits &body body)
  `(multiple-value-bind (result error) ;; result is just T/NIL
       (ignore-errors (progn ,@body))
     (when (not result)
       (imred-log-warn ;; expects variable REDUCTION-PLAN to be defined in scope
	(format nil "~A patching of HSC file ~A failed: ~A" ,description ,fits error)))
     result))

;; sometimes bzero and bscale are in the zeroth header, but in hfosc2 they are needed
;; in 2nd extension.  This results in an image being extremely negative
(defun %fix-hfosc2-missing-bzero+bscale (fits-file)
  (cf:with-open-fits-file (fits-file ff :mode :io)
    (when (= (cf:fits-file-num-hdus ff) 2) ;; only 2 extension files modified
      (cf:move-to-extension ff 1)
      (let ((bscale1 (cf:read-fits-header ff "BSCALE"))
	    (bzero1 (cf:read-fits-header ff "BZERO")))
	(cf:move-to-extension ff 2)
	(let ((bscale2 (cf:read-fits-header ff "BSCALE"))
	      (bzero2 (cf:read-fits-header ff "BZERO")))
	  (when (not bscale2)
	    (cf:write-fits-comment ff "imred: patching missing BSCALE in extension 2 from extension 1")
	    (cf:write-fits-header ff "BSCALE" bscale1))
	  (when (not bzero2)
	    (cf:write-fits-comment ff "imred: patching missing BZERO in extension 2 from extension 1")
	    (cf:write-fits-header ff "BZERO" bzero1)))))))
	    

(defun %fix-hfosc2-gain (fits-file)
  (cf:write-fits-header
   fits-file
   "GAIN" (instrument-id:get-gain-for-fits fits-file)
   :comment "Gain: guessed from dubious header"))

;; if object type is not there
(defun %fix-hfosc2-objtyp (fits-file)
  (when (not (cf:read-fits-header fits-file "IMAGETYP"))
    (let ((objtype (instrument-id:get-object-type-for-fits fits-file)))
      (cf:write-fits-header fits-file "IMAGTYP"
			    (format nil "~A" objtype)
			    :comment "BIAS/FLAT/OBJECT - guessed from OBJECT"))))




(defun hct-hfosc2-input-fits-patch-function (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
  (%with-hfosc2-patch-warn "Input fits Missing BZERO,BSCALE" fits-file
			   (%fix-hfosc2-missing-bzero+bscale fits-file))
  (%with-hfosc2-patch-warn "Input-fits GAIN" fits-file (%fix-hfosc2-gain fits-file))
  (%with-hfosc2-patch-warn "Input-fits OBJTYP" fits-file (%fix-hfosc2-objtyp fits-file)))


(defun hct-hfosc2-output-fits-patch-function (fits-file reduction-plan)
  (declare (ignorable fits-file reduction-plan))
  )

