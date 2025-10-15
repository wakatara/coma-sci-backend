

(in-package imred)


;; reduction plan suitable for VLT FORS
;;
(defclass %reduction-plan-vlt-fors (reduction-plan)
  ((trimsec :initform #(0 0 0 0))
   (bias-group-func
    :initform  (lambda (fits)
		 (concatenate 'string "-" (%gethead-or-error fits "EXTNAME"))))
   (flat-group-func
    :initform (lambda (fits)
		(format nil
			"~A-~A"
			(or 
			 (instrument-id:get-standard-filter-for-fits fits)
			 (error "Could not get filter for ~A" fits))
			(%gethead-or-error fits "EXTNAME"))))
   (fringe-group-func
    :initform
     (lambda (fits)
		(format nil
			"~A-~A"
			(or 
			 (instrument-id:get-standard-filter-for-fits fits)
			 (error "Could not get filter for ~A" fits))
			(%gethead-or-error fits "EXTNAME"))))
   ;; flats are taken with high counts
   (min-flat-counts    :initform 5000)
   (max-flat-counts    :initform 55000)

   ;; FORS seems to set super-saturated pixels to 0
   ;; because of bias offset, normal pixels should not be 0
   (invalid-pixel-function
    :initform (lambda (x) (declare (type single-float x)
				   (optimize speed))
		(or (= x 65535.0)
		    (= x 0.0))))
   ;;
   (output-fits-patch-function :initform nil)))


;; these four are not tested (FORS1 and old FORS2 with e2v)
(defclass reduction-plan-vlt-fors1-tek-chip1 (%reduction-plan-vlt-fors)
  ((trimsec :initform (instrument-id:get-trimsec-for-instrument
		       (make-instance 'instrument-id:vlt-fors1-tek-chip1)
		       nil))))

(defclass reduction-plan-vlt-fors1-tek-chip2 (%reduction-plan-vlt-fors)
  ((trimsec :initform (instrument-id:get-trimsec-for-instrument
		       (make-instance 'instrument-id:vlt-fors1-tek-chip2)
		       nil))))

(defclass reduction-plan-vlt-fors2-e2v-chip1 (%reduction-plan-vlt-fors)
  ((trimsec :initform (instrument-id:get-trimsec-for-instrument
		       (make-instance 'instrument-id:vlt-fors2-e2v-chip1)
		       nil))))

(defclass reduction-plan-vlt-fors2-e2v-chip2 (%reduction-plan-vlt-fors)
  ((trimsec :initform (instrument-id:get-trimsec-for-instrument
		       (make-instance 'instrument-id:vlt-fors2-e2v-chip2)
		       nil))))


;; the following is the only one that is tested
(defclass reduction-plan-vlt-fors2-mit/ll-chip1 (%reduction-plan-vlt-fors)
  ((trimsec :initform (instrument-id:get-trimsec-for-instrument
		       (make-instance 'instrument-id:vlt-fors2-mit/ll-chip1)
		       nil))))

(defclass reduction-plan-vlt-fors2-mit/ll-chip2 (%reduction-plan-vlt-fors)
  ((trimsec :initform (instrument-id:get-trimsec-for-instrument
		       (make-instance 'instrument-id:vlt-fors2-mit/ll-chip2)
		       nil))))




(defmethod get-reduction-plan-for-instrument ((inst instrument-id:vlt-fors1-tek-chip1))
  (declare (ignorable inst))
  'reduction-plan-vlt-fors1-tek-chip1)

(defmethod get-reduction-plan-for-instrument ((inst instrument-id:vlt-fors1-tek-chip2))
  (declare (ignorable inst))
  'reduction-plan-vlt-fors1-tek-chip2)


(defmethod get-reduction-plan-for-instrument ((inst instrument-id:vlt-fors2-e2v-chip1))
  (declare (ignorable inst))
  'reduction-plan-vlt-fors2-e2v-chip1)

(defmethod get-reduction-plan-for-instrument ((inst instrument-id:vlt-fors2-e2v-chip2))
  (declare (ignorable inst))
  'reduction-plan-vlt-fors2-e2v-chip2)


(defmethod get-reduction-plan-for-instrument ((inst instrument-id:vlt-fors2-mit/ll-chip1))
  (declare (ignorable inst))
  'reduction-plan-vlt-fors2-mit/ll-chip1)

(defmethod get-reduction-plan-for-instrument ((inst instrument-id:vlt-fors2-mit/ll-chip2))
  (declare (ignorable inst))
  'reduction-plan-vlt-fors2-mit/ll-chip2)





  
