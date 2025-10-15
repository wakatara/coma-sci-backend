

(in-package imred)


;; reduction plan suitable for Keck LRIS V1 (with one chip)
;;
(defclass %reduction-plan-keck-lris-v1 (reduction-plan)
  ((trimsec :initform #(0 0 0 0))
   (bias-group-func
    :initform  (lambda (fits) (declare (ignore fits)) ""))
   (flat-group-func
    :initform (lambda (fits)
		(format nil
			"~A"
			(or 
			 (instrument-id:get-standard-filter-for-fits fits)
			 (error "Could not get filter for ~A" fits)))))
   (fringe-group-func
    :initform
     (lambda (fits)
		(format nil
			"~A"
			(or 
			 (instrument-id:get-standard-filter-for-fits fits)
			 (error "Could not get filter for ~A" fits)))))
   ;; flats are taken with high counts
   (min-flat-counts    :initform 5000)
   (max-flat-counts    :initform 45000)

   (invalid-pixel-function
    :initform (lambda (x) (declare (type single-float x)
				   (optimize speed))
		(>= x 65535.0)))
   ;;
   (output-fits-patch-function :initform nil)))


;; the only one we're sure of
(defclass reduction-plan-keck-lris-red-v1-2 (%reduction-plan-keck-lris-v1)
  ;; there's a big vignetted region
  ((trimsec :initform #(230 1871 1 2048))))

#+nil ;; not sure if this even exists
(defclass reduction-plan-keck-lris-blue-v1-2 (%reduction-plan-keck-lris-v1)
  ;; there's a big vignetted region - PROBABLY WRONG WRONG WRONG
  ((trimsec :initform #(230 1871 1 2048))))



(defmethod get-reduction-plan-for-instrument ((inst instrument-id:keck-lris-red-v1-2))
  (declare (ignorable inst))
  'reduction-plan-keck-lris-red-v1-2)

#+nil ;; this doesn't exist
(defmethod get-reduction-plan-for-instrument ((inst instrument-id:keck-lris-blue-v1-2))
  (declare (ignorable inst))
  'reduction-plan-keck-lris-blue-v1-2)








  
