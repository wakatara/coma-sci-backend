

(in-package imred)


;; reduction plan suitable for UH 88
;;
(defclass reduction-plan-wht-acam (reduction-plan)
  ((inst-id-type :initform 'instrument-id:wht-acam)
   (max-flat-counts :initform 26000)
   (min-flat-counts :initform 4000)
   (zero-name :initform "BIAS")  ;
   (flat-basename :initform "FLAT")
   (output-fits-patch-function
    :initform 'wht-acam-final-fits-patch-function)))

(defmethod get-reduction-plan-for-instrument ((inst instrument-id:wht-acam))
  (declare (ignorable inst))
  'reduction-plan-wht-acam)



(defun wht-acam-final-fits-patch-function (fits-file reduction-plan)
  (declare (ignorable fits-file reduction-plan))
  t)
