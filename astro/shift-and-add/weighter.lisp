
#|

Parent class of image weighters, which define a way of creating image weights.


A weighter is called as

(run-weight-generation image-weighter saaplan fits-list)




|#

(in-package shift-and-add)


(defclass image-weighter ()
  ())

(defgeneric run-weight-generation (image-weighter saaplan fits-list)
  (:documentation "Create a set of weight images for fits-list."))


(defmethod run-weight-generation ((image-weighter image-weighter)
				  (saaplan saaplan)
				  fits-list)
  (error "Cannot run-weight-generation for parent class image-weighter."))
