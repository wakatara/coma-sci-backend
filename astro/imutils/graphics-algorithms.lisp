(in-package imutils)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cohen-Sutherland line clip algo from
;;  https://en.wikipedia.org/wiki/Cohen%E2%80%93Sutherland_algorithm
(defconstant +ch-inside+ 0)
(defconstant +ch-left+   1)
(defconstant +ch-right+  2)
(defconstant +ch-bottom+ 4)
(defconstant +ch-top+    8)

(defun cohen-sutherland-clip-line (x0 y0 x1 y1 xmin xmax ymin ymax)
  "Clip a line from X0,Y0 to X1,Y1 to fit into viewport XMIN,XMAX,YMIN,YMAX.
Return new (VALUES X0 Y0 X1 Y1 OUT-OF-BOUNDS) where OUT-OF-BOUNDS is true if
the entire line was outside the viewport."
  (flet ((compute-outcode (x y)
	   (declare (type single-float x y))
	   (let ((code +ch-inside+))
	     (declare (type (unsigned-byte 4) code))
	     (if (< x xmin)
		 (setf code (logior code +ch-left+))
		 (if (> x xmax)
		     (setf code (logior code +ch-right+))))
	     (if (< y ymin)
		 (setf code (logior code +ch-bottom+))
		 (if (> y ymax)
		     (setf code (logior code +ch-top+))))
	     code)))
    
    (let ((outcode0 (compute-outcode x0 y0))
	  (outcode1 (compute-outcode x1 y1))
	  (out-of-bounds nil))
      (declare (type (unsigned-byte 4) outcode0 outcode1))
      (loop
	(cond ((zerop (logior outcode0 outcode1))
	       (return))
	      ((plusp (logand outcode0 outcode1))
	       (setf out-of-bounds t)
	       (return))
	      (t
	       (let ((outcodeout (max outcode0 outcode1))
		     (x 0.0) (y 0.0))
		 (declare (type (unsigned-byte 4) outcodeout)
			  (type single-float x y))
		 ;; safe from division by zero
		 (cond ((plusp (logand outcodeout +ch-top+))
			(setf x (+ x0 (/ (* (- x1 x0) (- ymax y0))
					 (- y1 y0))))
			(setf y ymax))
		       ((plusp (logand outcodeout +ch-bottom+))
			(setf x (+ x0 (/ (* (- x1 x0) (- ymin y0))
					 (- y1 y0))))
			(setf y ymin))
		       ((plusp (logand outcodeout +ch-right+))
			(setf y (+ y0 (/ (* (- y1 y0) (- xmax x0))
					 (- x1 x0))))
			(setf x xmax))
		       ((plusp (logand outcodeout +ch-left+))
			(setf y (+ y0 (/ (* (- y1 y0) (- xmin x0))
					 (- x1 x0))))
			(setf x xmin)))
		 ;;
		 (cond ((= outcodeout outcode0)
			(setf x0 x)
			(setf y0 y)
			(setf outcode0 (compute-outcode x0 y0)))
		       (t
			(setf x1 x)
			(setf y1 y)
			(setf outcode1 (compute-outcode x1 y1))))))))
      ;;
      (values x0 y0 x1 y1 out-of-bounds))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
					     
		   
    
			     

(defun draw-line (im x0 y0 x1 y1 pixel-value &key (modify t))
  "Use Bresenham's algorithm to draw a line in image IM from X0,Y0 to X1,Y1 
using PIXEL-VALUE.

PIXEL-VALUE may be a float, or a single-float function (LAMBDA IPIX IX
IY) that is given which pixel IPIX from clipped starting point CX0,CY0
is being drawn, and the IX,IY index values.  The clipped starting point will
not be X0,Y0 if the line extends off the image.

If MODIFY is NIL, then don't make the line - in this case, any functionality
is supplied by a function PIXEL-VALUE."
  (declare (type image im)
	   (type (or single-float (function (fixnum fixnum fixnum) single-float))
		     pixel-value)
	   (type single-float x0 y0 x1 y1)
	   (optimize speed))
  ;; clip to array bounds
  (multiple-value-bind (cx0 cy0 cx1 cy1 out-of-bounds)
      (cohen-sutherland-clip-line
       x0 y0 x1 y1
       0.0  (1- (float (array-dimension im 1) 1.0))
       0.0  (1- (float (array-dimension im 0) 1.0)))
    (declare (type (single-float -1e18 1e18) cx0 cy0 cx1 cy1))
    (when (not out-of-bounds)
      (flet ((bresenham-line (nx0 ny0 nx1 ny1)
	       (declare (type (unsigned-byte 30) nx0 ny0 nx1 ny1)
			(optimize speed))
	       (let* ((dist-x (abs (- nx0 nx1)))
		      (dist-y (abs (- ny0 ny1)))
		      (steep (> dist-y dist-x))
		      (backwards nil)
		      (pix-is-float (floatp pixel-value)))
		 (when steep
		   (psetf nx0 ny0 ny0 nx0
			  nx1 ny1 ny1 nx1))
		 (when (> nx0 nx1)
		   (psetf nx0 nx1 nx1 nx0
			  ny0 ny1 ny1 ny0)
		   (setf backwards t)) ;; line is drawn from X1,Y1 to X0,Y0
		 (let* ((dx (- nx1 nx0))
			(dy (abs (- ny0 ny1)))
			(err (floor dx 2))
			(y-step (if (< ny0 ny1) 1 -1))
			(y ny0))
		   (declare (type fixnum dx dy err y-step y))
		   (loop
		     with npix = (1+ (- nx1 nx0))
		     with d-ipix = (if backwards -1 1)
		     ;; IPIX counts how many points we are from X0,Y0;
		     ;; takes into account if drawing is BACKWARDS
		     for ipix of-type fixnum = (if backwards (1- npix) 0)
		       then (+ ipix d-ipix)
		     for x of-type fixnum upfrom nx0 to nx1
		     do 
			(if steep ;; x,y are reversed for steep line
			    ;; compute the value to be filled in, but fill it in
			    ;; only if MODIFY is T
			    (let ((value (if pix-is-float
					     pixel-value
					     (funcall pixel-value ipix x y))))
			      (when modify
				(setf (aref im x y) (the single-float value))))
			    (let ((value (if pix-is-float
					     pixel-value
					     (funcall pixel-value ipix y x))))
			      (when modify
				(setf (aref im y x) (the single-float value)))))
			;;
			(setf err (- err dy))
			(when (< err 0)
			  (incf y y-step)
			  (incf err dx)))))))
	;;
	(bresenham-line
	 (max (round cx0) 0) ;; max,min just in case float rounding put us out of bounds
	 (max (round cy0) 0)
	 (min (round cx1) (1- (array-dimension im 1)))
	 (min (round cy1) (1- (array-dimension im 0))))))))
    

  
	   
 
