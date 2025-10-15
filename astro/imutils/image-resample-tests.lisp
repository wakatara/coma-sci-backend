
;; test routines for image resampling

(eval-when (load eval compile)
  (require 'stats)
  (require 'pgplot))


(in-package imutils)


(defparameter *im-in* nil)
(defparameter *im-out* nil)
(defparameter *im-in2* nil)
 

(defparameter *xform* nil)
(defparameter *xform-inv* nil)

(defun test-resample-linear (&key (xscale 1.0) (yscale 1.0) (angle-deg 0.0) (x0 0.0) (y0 0.0)
			     (nx-in 101) (ny-in 101) (nx-out 101) (ny-out 101)
			     (interp-method :linear))
  "Transform, and transform back, and plot all 3 images"
  (let* ((im-in (make-test-pattern-image :nx nx-in :ny ny-in))
	 (im-out (make-array (list ny-out nx-out) :element-type 'single-float))
	 (im-in2 (make-array (list ny-in nx-in) :element-type 'single-float))
	 (xform (build-xform-linear :xscale xscale :yscale yscale :angle-deg angle-deg
				    :x0 x0 :y0 y0))
	 (xform-inv nil))

    (setf (aref im-in 
		(ash ny-in -1) (ash nx-in -1))
		15.0)
    (make-xform-linear-map-centers im-in im-out xform)

    (setf xform-inv (invert-xform-linear xform))
    (setf *xform* xform)
    (setf *xform-inv* xform-inv)

    (resample-image-with-linear-xform im-in im-out xform :interp-method interp-method)

    (resample-image-with-linear-xform im-out im-in2 xform-inv :interp-method interp-method)

    (setf *im-in* im-in
	  *im-out* im-out
	  *im-in2* im-in2)

    (let ((p1 (pgplot:open-device :x11)))
      (pgplot:image p1 im-in :type :gray)
      (pgplot:box p1)
      (pgplot:toplabel p1 "Input"))
    
    (let ((p2 (pgplot:open-device :x11)))
      (pgplot:image p2 im-out :type :gray)
      (pgplot:box p2)
      (pgplot:toplabel p2 "Output"))

    (let ((p3 (pgplot:open-device :x11)))
      (pgplot:image p3 im-in2 :type :gray)
      (pgplot:box p3)
      (pgplot:toplabel p3 "Inverse applied to Output"))))



(defun test-resample-functional (&key 
				 (xscale 1.0) (yscale 1.0) (angle-deg 45.0) 
				 (nx-in 101) (ny-in 101) (nx-out 101) (ny-out 101)
				 (interp-method :linear))
  "Transform, and transform back, using a swirl function and plot all 3 images"
  (let* ((im-in (make-test-pattern-image :nx nx-in :ny ny-in))
	 (im-out (make-array (list ny-out nx-out) :element-type 'single-float))
	 (im-in2 (make-array (list ny-in nx-in) :element-type 'single-float))
	 (xform (make-xform-functional ))
	 (xform-inv (make-xform-functional ))
	 (val1 0.0)
	 (val2 (* 1.1 (stats:max-of-elements im-in))))

    (flet ((make-swirl-func (angle x0 y0 x1 y1)
	     (declare (type single-float angle x0 y0 x1 y1)
		      (optimize speed))
	     (lambda (xf)
	       (declare (type xform-functional xf))
	       (let* ((x (xform-functional-xin xf))
		      (y (xform-functional-yin xf))
		      (rmax x0)
		      (xx (- x x0))
		      (yy (- y y0))
		      (r (sqrt (the (float 0.0) (+ (expt xx 2) (expt yy 2)))))
		      (theta (* angle (/ r rmax)))
		      (cost (cos (* #.(float (/ pi 180) 1.0) theta)))
		      (sint (sin (* #.(float (/ pi 180) 1.0) theta)))
		      (xxout (+ x1 (* cost xx)    (* sint yy)))
		      (yyout (+ y1 (* -1 sint xx) (* cost yy))))
		 (declare (type single-float x y rmax xx yy theta cost sint xxout yyout))
		 (setf (xform-functional-xout xf) xxout)
		 (setf (xform-functional-yout xf) yyout)
		 nil))))

      (setf (xform-functional-func xform) 
	    (make-swirl-func angle-deg
			     (/ (1- nx-out) 2.0) (/ (1- ny-out) 2.0)
			     (/ (1- nx-in) 2.0) (/ (1- ny-in) 2.0)))
      (setf (xform-functional-func xform-inv) 
	     (make-swirl-func (- angle-deg)
			     (/ (1- nx-out) 2.0) (/ (1- ny-out) 2.0)
			     (/ (1- nx-in) 2.0) (/ (1- ny-in) 2.0)))
      
      (resample-image-with-functional-xform im-in im-out xform :interp-method interp-method)
      (resample-image-with-functional-xform im-out im-in2 xform-inv :interp-method interp-method)
      (setf *im-in* im-in
	    *im-out* im-out
	    *im-in2* im-in2)

      (let ((p1 (pgplot:open-device :x11)))
	(pgplot:image p1 im-in :type :gray :val1 val1 :val2 val2)
	(pgplot:box p1)
	(pgplot:toplabel p1 "Input"))
      
      (let ((p2 (pgplot:open-device :x11)))
	(pgplot:image p2 im-out :type :gray :val1 val1 :val2 val2)
	(pgplot:box p2)
	(pgplot:toplabel p2 "Output"))
      
      (let ((p3 (pgplot:open-device :x11)))
	(pgplot:image p3 im-in2 :type :gray :val1 val1 :val2 val2)
	(pgplot:box p3)
	(pgplot:toplabel p3 "Inverse applied to Output")))))
 

	  