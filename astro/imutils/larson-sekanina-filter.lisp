
#|

Apply Larson-Sekanina filter to an image - (1984 AJ)


New flux F' is 

  F'(r,a ; dr,da) = 2 F(r,a) - F(r-dr,a+da) - F(r-dr, a-da)

where r is radius from a given x0,y0 and a is the polar angle, and dr,da are parameters

|#

(in-package imutils)

(defun larson-sekanina-filter (im x0 y0 dr da &key (im-out nil) (interpolation :linear))
  "Apply Larson-Sekanina filter such that
  
     F'(r,a ; dr,da) = 2 F(r,a) - F(r-dr,a+da) - F(r-dr, a-da)

where r,a are polar coordinates around x0,y0 and dr,da are perturbations to them.

IM-OUT may be same as IM.  The interpolation for the offset/rotated image can be 
:LINEAR or :LANCZOS2."
  (declare (type image im)
	   (type single-float x0 y0 dr da)
	   (type (or image null) im-out)
	   (type (member :lanczos2 :linear) interpolation)
	   (optimize speed))

  (let ((im (if (eq im im-out)
		(copy-image im)
		im))
	(im-out (or im-out (make-same-size-image im)))
	(nx (array-dimension im 1))
	(ny (array-dimension im 1))
	(da/rad (* da #.(float (/ pi 180) 1.0))))

    (declare (type image im im-out)
	     (type (signed-byte 30) nx ny)
	     (type single-float da/rad))

    (assert (equalp (array-dimensions im) (array-dimensions im-out)))

    (loop
      for ix of-type fixnum below nx
      for x of-type single-float = (- ix x0)
      do (loop
	   for iy of-type fixnum below ny
	   for y of-type single-float = (- iy y0)
	   for r of-type single-float = (sqrt (the (float 0.0) (+ (* x x) (* y y))))
	   for a of-type single-float = (atan x y) ;; so that x is at a=0
	   for r- of-type single-float = (- r dr)
	   for a- of-type single-float = (- a da/rad)
	   for a+ of-type single-float = (+ a da/rad)
	   ;; convert r-,a+,a- back into rectangular coordinates
	   for cosa+ of-type single-float = (cos a+)
	   for sina+ of-type single-float = (sin a+)
	   for cosa- of-type single-float = (cos a-)
	   for sina- of-type single-float = (sin a-)
	   ;; true x,y coordinates in image of the two offset points
	   for x+ of-type single-float = (+ x0 (* r- cosa+))
	   for y+ of-type single-float = (+ y0 (* r- sina+))
	   for x- of-type single-float = (+ x0 (* r- cosa-))
	   for y- of-type single-float = (+ y0 (* r- sina-))
	   do (setf (aref im-out iy ix) 0.0) ;; default value
	      (cond ((eq interpolation :lanczos2)
		     (multiple-value-bind (f+ inside+)
			 (lanczos2-interpolate-image im x+ y+)
		       (multiple-value-bind (f- inside-)
			   (lanczos2-interpolate-image im x- y-)
			 (when (and inside+ inside-)
			     (setf (aref im-out iy ix)
				   (- (* 2 (aref im iy ix))
				      f+ f-))))))
		    ;;
		    ((eq interpolation :linear)
		     (multiple-value-bind (f+ inside+)
			 (linear-interpolate-image im x+ y+)
		       (multiple-value-bind (f- inside-)
			   (linear-interpolate-image im x- y-)
			 (when (and inside+ inside-)
			     (setf (aref im-out iy ix)
				   (- (* 2 (aref im iy ix))
				      f+ f-)))))))))
    im-out))
		    
	   
    
    
    
