
#|

generate trailed images by stacking shifted versions of an image

|#


(in-package imutils)

(defun trail-image (im dx dy ntrail &key (interp-method :linear)
		    (norm 1.0))
  "Trail an image by stacking it over a motion of -DX/2,-DY/2 to
+DX/2,+DY/2 using NTRAIL co-additions, each time resampling using
INTERP-METHOD."
  (declare (type image im)
	   (type single-float dx dy norm)
	   (type (unsigned-byte 20) ntrail))
  (when (< ntrail 2)  (error "NTRAIL must be >= 2"))
  ;;
  (let ((im-out (%dup-image-nocopy im))
	(im-sh  (%dup-image-nocopy im))
	(nt (ash ntrail -1))) 
    (loop
       with xform = (build-xform-linear) ;; unit xform
       for it from (- nt) to nt
       for ddx = (/ (* dx 0.5 it) nt)
       for ddy = (/ (* dy 0.5 it) nt)
       do
	 (setf (xform-linear-x0 xform) ddx)
	 (setf (xform-linear-y0 xform) ddy)
	 (resample-image-with-linear-xform 
	  im im-sh xform 
	  :interp-method interp-method)
	 (im+ im-out im-sh 
	      :image-out im-out 
	      :b-scale (/ norm (+ 1.0 (* 2 nt)))))

    im-out))
	      
  



