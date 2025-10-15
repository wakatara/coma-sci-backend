

#|

Resample images, defining transformations that map pixel indices to pixel indices

For example, to rotate an image around its center by 30 degrees and shrink by 0.5:

    (defun rotate-image (image-in)
      (let ((xform (build-xform-linear :angle 30.0 :x-scale 0.5 :y-scale 0.5))
            (image-out (make-array (array-dimensions image) :element-type 'single-float)))
        ;; ensure that xform maps center to center
        (make-xform-linear-map-centers image-in image-out)
        (resample-image-with-linear-xform im-in im-out xform :interp-method :lanczos3)
        image-out))
        

For a complete example, see routines TEST-RESAMPLE-LINEAR and 
TEST-RESAMPLE-FUNCTIONAL at end of this file



At present:

   * Transforms supported:

        * Linear       using XFORM-LINEAR structure with 2x2 matrix and offset
                       The linear xform is invertible
        * Functional   using XFORM-FUNCTIONAL, which contains a function of one 
                       argument, the XFORM-FUNCTIONAL itself.  The function converts
                       the XIN,XIN slots of the XFORM-FUNCTIONAL structure to
                       the XOUT,YOUT slots.  It defines a map from the output image
                       to the input, so that XIN,YIN is really a pixel coordinate location 
                       in the output image.   This is because scanning is done across
                       the output image.

   * Nearest pixel, bilinear interpolation, and Lanczos2,3 supported.
 
   * Does not perform any flux rescaling to compensate for pixel scale changes.


There are -WITH-FLAGS versions of the transform functions to support
logical OR'ing of a parallel flag array.



|#

(in-package imutils)


#|

Note that the linear transform is similar to the usual computer graphics transform

x'     |a  b  x0|  x
y'  =  |c  d  y0|  y
1      |0  0  1 |  1

where an extra 3rd dimension is always 1, and [[a,b],[c,d]] 
is the matrix inside XFORM-LINEAR


|#

(defstruct xform-linear 
  ;; 2x2 matrix
  (matrix (make-array '(2 2) :element-type 'single-float
		      :initial-contents '((1.0 0.0) (0.0 1.0)))
	  :type (simple-array single-float (2 2)))
  ;; positions added to x,y AFTER applying matrix 
  (x0 0.0 :type single-float)
  (y0 0.0 :type single-float))



(defun apply-xform-linear (xform-linear x y)
  "Apply a linear transform to X,Y."
  (declare (type xform-linear xform-linear)
	   (type single-float x y))
  (let* ((xform-matrix (xform-linear-matrix xform-linear))
	 (x0 (xform-linear-x0 xform-linear))
	 (y0 (xform-linear-y0 xform-linear))
	 (a11 (aref xform-matrix 0 0))
	 (a12 (aref xform-matrix 0 1))
	 (a21 (aref xform-matrix 1 0))
	 (a22 (aref xform-matrix 1 1)))
    (values
     (+ x0 (* a11 x) (* a12 y))
     (+ y0 (* a21 x) (* a22 y)))))

(defun invert-xform-linear (xform-linear)
  "Given a linear transformation XFORM-LINEAR, create the inverse transformation."
  (declare (type xform-linear xform-linear)
	   (optimize speed))
  (let* ((xform-matrix (xform-linear-matrix xform-linear))
	 (x0 (xform-linear-x0 xform-linear))
	 (y0 (xform-linear-y0 xform-linear))
	 (a11 (aref xform-matrix 0 0))
	 (a12 (aref xform-matrix 0 1))
	 (a21 (aref xform-matrix 1 0))
	 (a22 (aref xform-matrix 1 1))
	 (det  (- (* a11 a22) (* a12 a21)))
	 (a11-new (+ (/ a22 det)))
	 (a22-new (+ (/ a11 det)))
	 (a12-new (- (/ a12 det)))
	 (a21-new (- (/ a21 det)))
	 (xform-linear-new (make-xform-linear))
	 (xform-matrix-new (xform-linear-matrix xform-linear-new)))
    (setf
     (aref xform-matrix-new 0 0) a11-new
     (aref xform-matrix-new 1 1) a22-new
     (aref xform-matrix-new 0 1) a12-new
     (aref xform-matrix-new 1 0) a21-new
     (xform-linear-x0 xform-linear-new) (- (+ (* a11-new x0) (* a12-new y0)))
     (xform-linear-y0 xform-linear-new) (- (+ (* a21-new x0) (* a22-new y0))))
    ;;
    xform-linear-new))
    


(defun build-xform-linear (&key (xscale 1.0) (yscale 1.0) (angle-deg 0.0) 
			   (x0 0.0) (y0 0.0))
  "Build a transform that, in the following order:
   
      1. Scales by xscale, yscale
      2. rotates counterclockwise by ANGLE-DEG.
      3. adds X0,Y0
"

  (let* ((xform-linear (make-xform-linear :x0 x0 :y0 y0))
	 (angle (float (* angle-deg (/ pi 180)) 1.0))
	 (xform-matrix (xform-linear-matrix xform-linear))
	 (ca (cos angle))
	 (sa (sin angle)))
    (setf (aref xform-matrix 0 0) (* ca xscale))
    (setf (aref xform-matrix 1 0) (* +1 sa xscale))
    (setf (aref xform-matrix 0 1) (* -1 sa yscale))
    (setf (aref xform-matrix 1 1) (* ca yscale))
    xform-linear))
    
			   
(defun make-xform-linear-map-pixels-by-changing-center (xin yin xout yout xform-linear)
  "Modify a XFORM-LINEAR so that it maps XIN,YIN to XOUT, YOUT"
 (let* ((xform-matrix (xform-linear-matrix xform-linear))
	(a11 (aref xform-matrix 0 0))
	(a12 (aref xform-matrix 0 1))
	(a21 (aref xform-matrix 1 0))
	(a22 (aref xform-matrix 1 1)))
   (format t "Making ~A,~A map to ~A,~A~%" xin yin xout yout)
   (setf (xform-linear-x0 xform-linear)
	 (- xout (+ (* a11 xin) (* a12 yin))))
   (setf (xform-linear-y0 xform-linear)
	 (- yout (+ (* a21 xin) (* a22 yin))))
   xform-linear))


(defun make-xform-linear-map-centers (im-in im-out xform-linear)
  "Modify a XFORM-LINEAR so that it maps image centers to each other; 
eg, for at rotation around the center.

Specifically, if an image has size N and indices from 0 to N-1,
the center is at (N-1)/2.0"
  (make-xform-linear-map-pixels-by-changing-center
   (/ (- (array-dimension im-in 1) 1) 2.0)
   (/ (- (array-dimension im-in 0) 1) 2.0)
   (/ (- (array-dimension im-out 1) 1) 2.0)
   (/ (- (array-dimension im-out 0) 1) 2.0)
   xform-linear))
    	
	   


(defun make-xform-mapping-three-points (x1a y1a x1b y1b 
					x2a y2a x2b y2b
					x3a y3a x3b y3b)
  "Use the transformation of 3 points to fully define a transform.

Make a transform that maps 
    x1a y1a -->  x1b y1b
    x2a y2a -->  x2b y2b
    x3a y3a -->  x3b y3b"
  (let ((ma (make-array '(3 3) :element-type 'double-float :initial-element 1d0))
	(mb (make-array '(3 3) :element-type 'double-float :initial-element 1d0))
	;; the transform
	(mt (make-array '(3 3) :element-type 'double-float :initial-element 0d0)))
    ;; use the extra-dimension representation of the transforms
    (setf (aref ma 0 0) (float x1a 1d0)
	  (aref ma 1 0) (float y1a 1d0)
	  ;;
	  (aref ma 0 1) (float x2a 1d0)
	  (aref ma 1 1) (float y2a 1d0)
	  ;;
	  (aref ma 0 2) (float x3a 1d0)
	  (aref ma 1 2) (float y3a 1d0)
	  ;;
	  (aref ma 2 2) 1d0)
    ;;
     (setf (aref mb 0 0) (float x1b 1d0)
	   (aref mb 1 0) (float y1b 1d0)
	   ;;
	   (aref mb 0 1) (float x2b 1d0)
	   (aref mb 1 1) (float y2b 1d0)
	   ;;
	   (aref mb 0 2) (float x3b 1d0)
	   (aref mb 1 2) (float y3b 1d0)
	   ;;
	   (aref mb 2 2) 1d0)
     ;;
     ;; now we have the matrix equation  Mb=Mt*Ma so we invert
     ;; Ma and compute Mt=Mb*Inverse(Ma)
     (matrix:invert-matrix-destructive-dbl ma)
     (matrix:matrix-multiply-dbl   mb ma   :target-matrix mt)
     ;;
     (let ((2matrix (make-array '(2 2) :element-type 'single-float))
	   (x0 (float (aref mt 0 2) 1.0))
	   (y0 (float (aref mt 1 2) 1.0)))
       (setf (aref 2matrix 0 0) (float (aref mt 0 0) 1.0)
	     (aref 2matrix 0 1) (float (aref mt 0 1) 1.0)
	     (aref 2matrix 1 0) (float (aref mt 1 0) 1.0)
	     (aref 2matrix 1 1) (float (aref mt 1 1) 1.0))
       (make-xform-linear :matrix 2matrix :x0 x0 :y0 y0))))
	     
    
    
  




(defun resample-image-with-linear-xform (im-in im-out xform-linear
					 &key 
					 (invert-xform nil)
					 (interp-method :linear))
						 
  "Fill the new image IM-OUT with the linear resampled version of
the old image IN-IN.

Arguments are:


   IM-IN, IM-OUT  - input and output single float arrays

   XFORM-LINEAR   - a linear transform structure based on a 2x2 
                    transform array array #((a11 a12) 
                                            (a21 a22)) 
                    and offsets x0,y0

XFORM-LINEAR is defined such that output positions xi,yi are
transfomed from input xo,yo as

  xo = a11.xi + a12.yi + x0
  xo = a21.xi + a22.yi + y0

Only pixels that are INSIDE the source image as defined by the 
transform have an effect; otherwise the output image is untouched.

If INVERT-XFORM is set, then the transformation specifies input positions 
in terms of output.

INTERP-METHOD is :NEAREST, :LINEAR, :LANCZOS2, :LANCZOS3, or :LANCZOS4
They are all lightly tested.

One should not rebin from small pixels to big without smoothing the 
small-pixel image; otherwise, each big pixel gets a contribution from a small
subsample of the small pixels covering it.

Image interpolation is through LINEAR-INTERPOLATE-IMAGE or
LANCZOSn-INTEPOLATE-IMAGE so that edges are handled by
extrapolating the image off the edge using the edge value."

  (declare (type image im-in im-out)
	   (type (member :nearest :linear :lanczos2 :lanczos3 :lanczos4) interp-method)
	   (type xform-linear xform-linear))
  (resample-image-with-linear-xform-with-flags im-in  nil
					       im-out nil
					       xform-linear
					       :invert-xform invert-xform
					       :interp-method interp-method))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; a tranformation that contains a function that takes XFORM-FUNCTIONAL as an argument,
;; and computes XOUT,YOUT from XIN,YIN 
(defstruct xform-functional
  (func nil :type (or null function)) ;; a function invoked as (FUNC THIS-XFORM-FUNCTIONAL)
  (xin 0.0 :type single-float)  
  (yin 0.0 :type single-float)
  (xout 0.0 :type single-float)
  (yout 0.0 :type single-float)
  (data nil)) ;; arbitrary user data  


	
(defun resample-image-with-functional-xform (im-in im-out xform-functional
					     &key 
					     (interp-method :linear))
						 
  "Given

   IM-IN, IM-OUT     - input and output single float arrays
   XFORM-FUNCTIONAL  - a functional transform structure that defines how
                       XIN,YIN in the output image map to XOUT,YOUT in the
                       input image

fill the new image IM-OUT with the resampled version of the old one.

Only pixels that are INSIDE the source image as defined by the 
transform have an effect; otherwise the output image is untouched.

INTERP-METHOD is :NEAREST, :LINEAR, :LANCZOS2, :LANCZOS3, or LANCZOS4
Lanczos is not well tested.

One should not rebin from small pixels to big without smoothing the 
small-pixel image; otherwise, each big pixel gets a contribution from a small
subsample of the small pixels covering it's center region.

Image interpolation is through LINEAR-INTERPOLATE-IMAGE or
LANCZOSn-INTEPOLATE-IMAGE so that edges are handled by
extrapolating the image off the edge using the edge value."

  (declare (type image im-in im-out)
	   (type (member :nearest :linear :lanczos2 :lanczos3 :lanczos4) interp-method)
	   (type xform-functional xform-functional))
  (resample-image-with-functional-xform-with-flags im-in  nil
						   im-out nil
						   xform-functional
						   :interp-method interp-method))

	 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; versions with flags follow - these are used by the simple version,
;; with NULL flags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resample-image-with-linear-xform-with-flags
    (im-in flags-in im-out flags-out xform-linear
     &key 
     (invert-xform nil)
     (interp-method :linear))
						 
  "Fill the new image IM-OUT with the linear resampled version of
the old image IN-IN.

Arguments are:


   IM-IN, IM-OUT  - input and output single float arrays
   FLAGS-IN, FLAGS-OUT  - logical OR flag images 
                          (may both be NIL for no flagging)
   XFORM-LINEAR   - a linear transform structure based on a 2x2 
                    transform array array #((a11 a12) 
                                            (a21 a22)) 
                    and offsets x0,y0

XFORM-LINEAR is defined such that output positions xi,yi are
transfomed from input xo,yo as

  xo = a11.xi + a12.yi + x0
  xo = a21.xi + a22.yi + y0

Only pixels that are INSIDE the source image as defined by the 
transform have an effect; otherwise the output image is untouched.

If INVERT-XFORM is set, then the transformation specifies input positions 
in terms of output.

INTERP-METHOD is :NEAREST, :LINEAR, :LANCZOS2, :LANCZOS3, or :LANCZOS4
They are all lightly tested.

One should not rebin from small pixels to big without smoothing the 
small-pixel image; otherwise, each big pixel gets a contribution from a small
subsample of the small pixels covering it.

Image interpolation is through LINEAR-INTERPOLATE-IMAGE or
LANCZOSn-INTEPOLATE-IMAGE so that edges are handled by
extrapolating the image off the edge using the edge value."

  (declare (type image im-in im-out)
	   (type (or null flag-image) flags-in flags-out)
	   (type (member :nearest :linear :lanczos2 :lanczos3 :lanczos4)
		 interp-method)
	   (type xform-linear xform-linear))

  (when (and (or flags-in flags-out)
	     (not (and flags-in flags-out)))
    (error "Both or neither or FLAGS-IN, FLAGS-OUT may be NIL"))
  (when flags-in
    (when (not (equalp (array-dimensions im-in)
		       (array-dimensions flags-in)))
      (error "Input image and its flag do not match in size"))
    (when (not (equalp (array-dimensions im-out)
		       (array-dimensions flags-out)))
      (error "Output image and its flag do not match in size")))

  ;; the default xform is defined as mapping IN->OUT, but when
  ;; scanning across OUT pixels, we need OUT->IN map
  (when (not invert-xform) ;; yes, this is correct - invert when
			   ;; INVERT-XFORM is NIL
    (setf xform-linear (invert-xform-linear xform-linear)))

  (let* ((xform-matrix (xform-linear-matrix xform-linear))
	 (x0 (xform-linear-x0 xform-linear))
	 (y0 (xform-linear-y0 xform-linear))
	 (a11 (aref xform-matrix 0 0))
	 (a12 (aref xform-matrix 0 1))
	 (a21 (aref xform-matrix 1 0))
	 (a22 (aref xform-matrix 1 1))
	 (nxo (array-dimension im-out 1))
	 (nyo (array-dimension im-out 0)))
    (declare (type single-float x0 y0 a11 a12 a21 a22)
	     (type (simple-array single-float (2 2)) xform-matrix)
	     (type (unsigned-byte 24) nxo nyo))
    ;; macro to put the interp method test outside the loop, and
    ;; possibly improve code locality
    (macrolet ((do-the-loop (interp-func)
		`(locally (declare (optimize speed))
		   (loop 
		      for iy of-type (unsigned-byte 24) below nyo
		      do
			(loop  ;; inner loop over x for a bit more speed/locality
			   for ix of-type (unsigned-byte 24) below nxo
			   do
			     (let ((xi (+ x0 (* a11 ix) (* a12 iy)))
				   (yi (+ y0 (* a21 ix) (* a22 iy))))
			       (declare (type single-float xi yi))
			       (multiple-value-bind (value inside? flagval)
				   (,interp-func im-in flags-in xi yi)
				 (declare (type single-float value)
					  (type imflag flagval))
				 (when inside?
				   ;; flagging is optional
				   (setf (aref im-out iy ix) value)
				   (when flags-out
				     (setf (aref flags-out iy ix) flagval))))))))))
      
      (cond ((eq interp-method :nearest)
	     (do-the-loop nearest-pixel-interpolate-image-with-flags))
	    ((eq interp-method :linear)
	     (do-the-loop linear-interpolate-image-with-flags))
	    ((eq interp-method :lanczos2)
	     (do-the-loop lanczos2-interpolate-image-with-flags))
	    ((eq interp-method :lanczos3)
	     (do-the-loop lanczos3-interpolate-image-with-flags))
	    ((eq interp-method :lanczos4)
	     (do-the-loop lanczos4-interpolate-image-with-flags))
	    ))))
 



	
(defun resample-image-with-functional-xform-with-flags
    (im-in flags-in im-out flags-out
     xform-functional
     &key 
     (interp-method :linear))
						 
  "Given

   IM-IN, IM-OUT     - input and output single float arrays
   FLAGS-IN, FLAGS-OUT  - logical OR flag images 
                          (may both be NIL for no flagging)
   XFORM-FUNCTIONAL  - a functional transform structure that defines how
                       XIN,YIN in the output image map to XOUT,YOUT in the
                       input image

fill the new image IM-OUT with the resampled version of the old one.

Only pixels that are INSIDE the source image as defined by the 
transform have an effect; otherwise the output image is untouched.

INTERP-METHOD is :NEAREST, :LINEAR, :LANCZOS2, :LANCZOS3, or LANCZOS4
Lanczos is not well tested.

One should not rebin from small pixels to big without smoothing the 
small-pixel image; otherwise, each big pixel gets a contribution from a small
subsample of the small pixels covering it's center region.

Image interpolation is through LINEAR-INTERPOLATE-IMAGE or
LANCZOSn-INTEPOLATE-IMAGE so that edges are handled by
extrapolating the image off the edge using the edge value."

  (declare (type image im-in im-out)
	   (type (or null flag-image) flags-in flags-out)
	   (type (member :nearest :linear :lanczos2 :lanczos3 :lanczos4)
		 interp-method)
	   (type xform-functional xform-functional))

  (when (not (xform-functional-func xform-functional))
    (error "XFORM-FUNCTIONAL has NIL in the FUNC slot - no transformation function provided"))

  (when (and (or flags-in flags-out)
	     (not (and flags-in flags-out)))
    (error "Both or neither or FLAGS-IN, FLAGS-OUT may be NIL"))
  (when flags-in
    (when (not (equalp (array-dimensions im-in)
		       (array-dimensions flags-in)))
      (error "Input image and its flag do not match in size"))
    (when (not (equalp (array-dimensions im-out)
		       (array-dimensions flags-out)))
      (error "Output image and its flag do not match in size")))

  (let* ((nxo (array-dimension im-out 1))
	 (nyo (array-dimension im-out 0))
	 (xform-functional (copy-xform-functional xform-functional)) ;; make it threadsafe
	 (func (xform-functional-func xform-functional)))
    (declare (type (unsigned-byte 24) nxo nyo)
	     (type xform-functional xform-functional)
	     (type function func))
    ;; macro to put the interp method test outside the loop, and
    ;; possibly improve code locality
    (macrolet ((do-the-loop (interp-func)
		`(locally (declare (optimize speed))
		   (loop 
		      with xi of-type single-float and yi of-type single-float 
		      for iy of-type (unsigned-byte 24) below nyo
		      do
			(loop  ;; inner loop over x for a bit more speed/locality
			   for ix of-type (unsigned-byte 24) below nxo
			   do
			     (setf (xform-functional-xin xform-functional) (float ix 1.0))
			     (setf (xform-functional-yin xform-functional) (float iy 1.0))
			     (funcall func xform-functional)
			     (setf xi (xform-functional-xout xform-functional))
			     (setf yi (xform-functional-yout xform-functional))
			     (multiple-value-bind (value inside? flagval)
				 (,interp-func im-in flags-in xi yi)
			       (declare (type single-float value)
					(type imflag flagval))
			       (when inside?
				 (setf (aref im-out iy ix) value)
				 (when flags-out
				   (setf (aref flags-out iy ix) flagval)))))))))
	      ;;
	 (cond ((eq interp-method :nearest)
		(do-the-loop nearest-pixel-interpolate-image-with-flags))
	       ((eq interp-method :linear)
		(do-the-loop linear-interpolate-image-with-flags))
	       ((eq interp-method :lanczos2)
		(do-the-loop lanczos2-interpolate-image-with-flags))
	       ((eq interp-method :lanczos3)
		(do-the-loop lanczos3-interpolate-image-with-flags))
	       ((eq interp-method :lanczos4)
		(do-the-loop lanczos3-interpolate-image-with-flags))
	       ))))

	      
	  
 
