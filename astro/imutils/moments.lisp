
#|

compute 0th, 1st, and 2nd moments of images, in some
radius around a point. Integrals are precise over the bin.


|#

(in-package imutils)


;; a helper function to test whether any pixels are flagged in aperture
(defun flagged-pix-in-aperture-p (imflag x0 y0 rap)
  "Loop over pixels in flag image IMFLAG over an aperture of size RAP
centered on X0,Y0.  Return 
  (VALUES IFLAG NFLAGGED)
where IFLAG is the logical OR of any flagged pixels, and NFLAGGED
is the number of flagged pixels"
  (declare (type flag-image imflag)
	   (type (single-float 0.0 1e6) x0 y0 rap)
	   (optimize speed))
  (loop 
     with nflagged of-type (unsigned-byte 28) = 0
     with iflag of-type  (unsigned-byte 28) = 0
     with rap2 of-type single-float = (* rap rap)
     with ixmin of-type simindex = (floor (- x0 rap))
     with ixmax of-type simindex = (ceiling (+ x0 rap))
     with iymin of-type simindex = (floor (- y0 rap))
     with iymax of-type simindex = (ceiling (+ y0 rap))  
     initially
       (when (not (and (>= ixmin 0) (< ixmax (array-dimension imflag 1))
		       (>= iymin 0) (< iymax (array-dimension imflag 0))))
	 (error "Out of bounds computation of moment - R too big?"))
     for iy of-type imindex from iymin to iymax
     for y of-type single-float = (- y0 iy) 
     do
       (loop 
	  for ix of-type imindex from ixmin to ixmax
	  for x of-type single-float = (- x0 ix) 
	  for r2 of-type single-float = (+ (* x x) (* y y))
	  when (<= r2 rap2)
	  do 
	    (let ((fval (aref imflag iy ix)))
	      (when (not (zerop fval))
		(incf nflagged)
		(setf iflag (logior iflag fval)))))
     finally
       (return (values iflag nflagged))))


(defun zeroth-moment (im x0 y0 rap backd)
  "Compute zeroth moment of an image (area) are x0, y0 for pixels
inside a radius RAP"
  (declare (type image im)
	   (type (single-float 0.0 1e6) x0 y0 rap)
	   (type (single-float -1e6 1e6) backd)
	   (optimize speed))

  (loop 
     with rap2 of-type single-float = (* rap rap)
     with mom0 of-type single-float = 0.0
     with ixmin of-type simindex = (floor (- x0 rap))
     with ixmax of-type simindex = (ceiling (+ x0 rap))
     with iymin of-type simindex = (floor (- y0 rap))
     with iymax of-type simindex = (ceiling (+ y0 rap))  
     initially
       (when (not (and (>= ixmin 0) (< ixmax (array-dimension im 1))
		       (>= iymin 0) (< iymax (array-dimension im 0))))
	 (error "Out of bounds computation of moment - R too big?"))
     for iy of-type imindex from iymin to iymax
     for y of-type single-float = (- y0 iy) 
     do
       (loop 
	  for ix of-type imindex from ixmin to ixmax
	  for x of-type single-float = (- x0 ix) 
	  for r2 of-type single-float = (+ (* x x) (* y y))
	  when (<= r2 rap2)
	  do (incf mom0 (- (aref im iy ix) backd)))
     finally
       (return mom0)))
	    

 
(defun first-moments (im x0 y0 rap backd)
  "Compute first moments of an image (area) are x0, y0 for pixels
inside a radius r.

Return (VALUES <x> <y> <1>)"
  (declare (type image im)
	   (type (single-float 0.0 1e6) x0 y0 rap)
	   (type (single-float -1e6 1e6) backd)
	   (optimize debug))

  (loop 
     with rap2 of-type single-float = (* rap rap)
     with mom0 of-type single-float = 0.0
     with mom1x of-type single-float = 0.0
     with mom1y of-type single-float = 0.0
     with ixmin of-type simindex = (floor (- x0 rap))
     with ixmax of-type simindex = (ceiling (+ x0 rap))
     with iymin of-type simindex = (floor (- y0 rap))
     with iymax of-type simindex = (ceiling (+ y0 rap))   
     initially
       (when (not (and (>= ixmin 0) (< ixmax (array-dimension im 1))
		       (>= iymin 0) (< iymax (array-dimension im 0))))
	 (error "Out of bounds computation of moment - R too big?"))
     for iy of-type simindex from iymin to iymax ;; must be SIMINDEX here
     for y of-type single-float = (- iy y0) 
     do
       (loop 
	  for ix of-type simindex from ixmin to ixmax
	  for x of-type single-float = (- ix x0) 
	  for r2 of-type single-float = (+ (* x x) (* y y))
	  when (<= r2 rap2)
	  do 
	    (let ((imval (- (aref im iy ix) backd)))
	      (incf mom0 imval)
	      ;; the integral of x over a box x-1/2 to x+1/2 is
	      ;; just x
	      (incf mom1x (* imval x))
	      (incf mom1y (* imval y))))
     finally
       (return 
	 (values (+ x0 (/ mom1x mom0))
		 (+ y0 (/ mom1y mom0))
		 mom0))))


(defun first-radial-moment (im x0 y0 rap backd &key (recenter nil)
			    (subpix 1))
  "Compute the first radial moment of IM, and by default 
first RECENTER X0,Y0 using first moments.

Performs SUBPIX = 1,3,5 subpixel quadrature over each pixel
to compute value of R in it, the benefit is unclear, because
the image has already been blurred by being binned.  

Return (VALUES <R^1> <X> <Y> <1>)"
  (declare (type image im)
	   (type (single-float 0.0 1e6) x0 y0 rap)
	   (type (member 1 3 5) subpix) 
	   (type (single-float -1e6 1e6) backd)
	   (optimize speed))
  (let ((x0-adj x0)
	(y0-adj y0))
    (when recenter
      (multiple-value-setq (x0-adj y0-adj)
	(first-moments im x0 y0 rap backd)))
    (loop 
       with rap2 of-type single-float = (* rap rap)
       with mom0 of-type single-float = 0.0
       with mom1r  of-type single-float = 0.0
       with ixmin of-type simindex = (floor (- x0-adj rap))
       with ixmax of-type simindex = (ceiling (+ x0-adj rap))
       with iymin of-type simindex = (floor (- y0-adj rap))
       with iymax of-type simindex = (ceiling (+ y0-adj rap))  
       initially
	 (when (not (and (>= ixmin 0) (< ixmax (array-dimension im 1))
			 (>= iymin 0) (< iymax (array-dimension im 0))))
	   (error "Out of bounds computation of moment - R too big?"))
       for iy of-type simindex from iymin to iymax
       for y of-type single-float = (- iy y0-adj) 
       do
	 (loop 
	    for ix of-type simindex from ixmin to ixmax
	    for x of-type single-float = (- ix x0-adj) 
	    for r2 of-type single-float = (+ (* x x) (* y y))
	    when (<= r2 rap2)
	    do 
	      (let ((imval (- (aref im iy ix) backd))
		    (r ;; integral of r over this pixel
		     (subpixel-quadrature 
		      x y xx yy subpix 
		      (sqrt (+ (expt xx 2) (expt yy 2))))))
		(incf mom0 imval)
		(incf mom1r (* r imval))))
       finally
	 (return 
	   (values 
	    (/ mom1r mom0)
	    x0-adj
	    y0-adj
	    mom0)))))




(defun second-moments (im x0 y0 rap backd)
  "compute second moments of an image at x0,y0
inside a radius RAP, subtracting background level BACKD.
Computes 0th and 1st moments as well.

Return (VALUES <xx> <yy> <xy> <x> <y> <1>)"
  (declare (type image im)
	   (type (single-float 0.0 1e6) x0 y0 rap)
	   (type (single-float -1e6 1e6) backd)
	   (optimize speed))

  (loop 
     ;; do math in double precision because of roundoff hazard at end
     with x0 = (float x0 1d0) and y0 = (float y0 1d0) 
     with rap = (float rap 1d0) and backd = (float backd 1d0)
     with rap2 of-type double-float = (* rap rap)
     with mom0 of-type double-float = 0d0
     with mom1x of-type double-float = 0d0
     with mom1y of-type double-float = 0d0
     with mom2xx of-type double-float = 0d0
     with mom2yy of-type double-float = 0d0
     with mom2xy of-type double-float = 0d0
     with ixmin of-type simindex = (floor (- x0 rap))
     with ixmax of-type simindex = (ceiling (+ x0 rap))
     with iymin of-type simindex = (floor (- y0 rap))
     with iymax of-type simindex = (ceiling (+ y0 rap))  
     initially
       (when (not (and (>= ixmin 0) (< ixmax (array-dimension im 1))
		       (>= iymin 0) (< iymax (array-dimension im 0))))
	 (error "Out of bounds computation of moment - R too big?"))
     for iy of-type simindex from iymin to iymax
     for y of-type double-float = (- iy y0) 
     do
       (loop 
	  for ix of-type simindex from ixmin to ixmax
	  for x of-type double-float = (- ix x0) 
	  for r2 of-type double-float = (+ (* x x) (* y y))
	  when (<= r2 rap2)
	  do 
	    (let ((imval (- (aref im iy ix) backd)))
	      (incf mom0 imval)
	      ;; the integral of x over a box x-1/2 to x+1/2 is x
	      (incf mom1x (* imval x))
	      (incf mom1y (* imval y))
	      ;; the integral of x^2 over this box is x0^2 + 1/12
	      (incf mom2xx (* imval (+ (expt x 2) #.(/ 1d0 12))))
	      (incf mom2yy (* imval (+ (expt y 2) #.(/ 1d0 12))))
	      (incf mom2xy (* imval x y))))
     finally
       ;; normalize all
       (setf mom1x (/ mom1x mom0))
       (setf mom1y (/ mom1y mom0))
       (setf mom2xx (/ mom2xx mom0))
       (setf mom2yy (/ mom2yy mom0))
       (setf mom2xy (/ mom2xy mom0))
       ;; and adjust 2nd moments by 1st moments
       (decf mom2xx (expt mom1x 2))
       (decf mom2yy (expt mom1y 2))
       (decf mom2xy (* mom1x mom1y))
       (return 
	 (values 
	  (float mom2xx  1.0)
	  (float mom2yy  1.0)
	  (float mom2xy  1.0)
	  (float (+ x0 mom1x) 1.0)
	  (float (+ y0 mom1y) 1.0)
	  (float mom0 1.0)))))
	    

(defun gaussian-weighted-second-moments (im x0 y0 rap backd sigma)
  "compute second moments of an image at X0,Y0
inside a radius RAP, subtracting background level BACKD.
Computes 0th and 1st moments as well.

Unlike normal momens, this weights the image by a Gaussian around
X0,Y0 of the form exp(- 0.5 r^2/sigma^2).  The center of the weighting
is precisely Y0,Y0, and is not recomputed in any way.  

It is critical that X0,Y0 are the correct center, because the weight
function is centered on X0,Y0.

Return (VALUES <xx> <yy> <xy> <x> <y> <1>)"
  (declare (type image im)
	   (type (single-float 0.0 1e6) x0 y0 rap sigma)
	   (type (single-float -1e6 1e6) backd)
	   (optimize speed))

  (loop 
     ;; do math in double precision because of roundoff hazard at end
     with x0 = (float x0 1d0) and y0 = (float y0 1d0) 
     with rap = (float rap 1d0) and backd = (float backd 1d0) and sigma = (float sigma 1d0)
     with rap2 of-type double-float = (* rap rap)
     with s2 of-type double-float = (expt sigma 2)
     with mom0 of-type double-float = 0d0
     with mom1x of-type double-float = 0d0
     with mom1y of-type double-float = 0d0
     with mom2xx of-type double-float = 0d0
     with mom2yy of-type double-float = 0d0
     with mom2xy of-type double-float = 0d0
     with ixmin of-type simindex = (floor (- x0 rap))
     with ixmax of-type simindex = (ceiling (+ x0 rap))
     with iymin of-type simindex = (floor (- y0 rap))
     with iymax of-type simindex = (ceiling (+ y0 rap))  
     initially
       (when (not (and (>= ixmin 0) (< ixmax (array-dimension im 1))
		       (>= iymin 0) (< iymax (array-dimension im 0))))
	 (error "Out of bounds computation of moment - R too big?"))
     for iy of-type simindex from iymin to iymax
     for y of-type double-float = (- iy y0) 
     do
       (loop 
	  for ix of-type simindex from ixmin to ixmax
	  for x of-type double-float = (- ix x0) 
	  for r2 of-type double-float = (+ (* x x) (* y y))
	  for w of-type double-float = (exp (* -0.5d0 (/ r2 s2)))
	  when (<= r2 rap2)
	  do 
	    (let ((imval (* w (- (aref im iy ix) backd))))
	      (incf mom0 imval)
	      ;; the integral of x over a box x-1/2 to x+1/2 is x
	      (incf mom1x (* imval x))
	      (incf mom1y (* imval y))
	      ;; the integral of x^2 over this box is x0^2 + 1/12
	      (incf mom2xx (* imval (+ (expt x 2) #.(/ 1.0 12))))
	      (incf mom2yy (* imval (+ (expt y 2) #.(/ 1.0 12))))
	      (incf mom2xy (* imval x y))))
     finally
       ;; normalize all
       (setf mom1x (/ mom1x mom0))
       (setf mom1y (/ mom1y mom0))
       (setf mom2xx (/ mom2xx mom0))
       (setf mom2yy (/ mom2yy mom0))
       (setf mom2xy (/ mom2xy mom0))
       ;; and adjust 2nd moments by 1st moments
       (decf mom2xx (expt mom1x 2))
       (decf mom2yy (expt mom1y 2))
       (decf mom2xy (* mom1x mom1y))
       (return 
	 (values 
	  (float mom2xx 1.0)
	  (float mom2yy 1.0)
	  (float mom2xy 1.0)
	  (float (+ x0 mom1x) 1.0)
	  (float (+ y0 mom1y) 1.0)
	  (float mom0 1.0)))))
	    

	    
#+nil
(defun 2nd-moment-test ()
  (let ((im (make-image 21 21)))
    (loop for ix from -5 to 5
	 do
	 (loop for iy from -5 to 5
	      do
	      (setf (aref im (+ iy 10) (+ ix 10)) 1.0)))
    (format t "This should produce 2nd moments of 10.0833 because
the pixels extend from -5.5 to 5.5, hence integral is
 2/3 5.5^3 / (2x5.5)~%")
    (second-moments im 11.0 11.0 9.0 0.0)))


(defun xy-moments-to-ab-pa (mxx myy mxy)
  "Convert xy 2ND moments MXX MYY MXY to A,B,PA notation,
where A is the major axis, B is the minor, and PA is an angle in
degrees rotating from the +Y axis to the +X axis, with pure +Y being 0
degrees.

Return (VALUES A B PA) "
  (let* ((g1 (+ mxx myy))
	 (g2 (- mxx myy))
	 ;; this will never be a sqrt(negative)
	 (g3 (sqrt (the (real 0) (+ (expt g2 2) (* 4 mxy mxy)))))
	 (theta (/ (atan (* 2 mxy) g2) 2))
	 (pa (* (/ 180 pi) (- (/ pi 2) theta)))
	 (a^2 (/ (+ g1 g3) 2))
	 (b^2 (/ (- g1 g3) 2)))
    (when (or (minusp a^2) (minusp b^2))
      (error "Complex quantity in moments A^2<0 and/or B^2<0."))
    ;; put PA in [-90,90]
    (setf pa (nth-value 1 (floor pa 360))) ;; in [0,360)
    (when (> pa 180) (decf pa 360)) ;; in [-180,180]
    ;; now use 180 symmetry of pa to put in [-90 90]
    (cond ((< pa -90)  (incf pa 180))
	  ((> pa 90)   (decf pa 180)))
    (values
     (sqrt (the (real 0) a^2))
     (sqrt (the (real 0) b^2))
     pa)))





	 
	 
(defun ab-pa-to-xy-moments (a b pa)
  "Convert moments in the form of major,minor axes A,B
and position angle in degrees PA to 
 (VALUES MXX MYY MXY)"
  (let* ((a2 (expt a 2))
	 (b2 (expt b 2))
	 (amb (/ (- a2 b2) 2))
	 (apb (/ (+ a2 b2) 2))
	 (theta (- (/ pi 2) (* (/ pi 180) pa)))
	 (cos2theta (cos (* 2 theta)))
	 (mxx  (+ apb (* amb cos2theta)))
	 (myy  (- apb (* amb cos2theta)))
	 (mxy  (* amb (sin (* 2 theta)))))
    (values mxx myy mxy)))



(defun convert-moments-under-linear-xform (mxx myy mxy
					   &key 
					   (inverse nil)
					   (c11 1) (c12 0)
					   (c21 0) (c22 1))
  "Assuming that moments MXX,MYY,MXY are defined in a coordinate
system X,Y that is transformed to a new system U,C

      U     | c11    c12 |  X
          = |            | 
      V     | c21    c22 |  Y

return the new moments MUU,MVV,MUV.   If INVERSE is true,
then the transform goes in the opposite direction so that Cij
takes (U,C) to (X,Y).

The formula is Muv = C Mxy C^T   where C is the 2x2 transform matrix

Returns (VALUES MUU MVV MUV).

Using CDij for Cij allows one to convert chip moments to WCS moments.
"

  ;; invert matrix if necessary
  (when inverse
    (multiple-value-setq (c11 c12 c21 c22)
      (let ((invdet (/ 1 (- (* c11 c22) (* c12 c21))))) ;; 1/determinant
	(values
	 (* c22 invdet)
	 (- (* c12 invdet))
	 (- (* c21 invdet))
	 (* c11 invdet)))))
  ;;
  ;; multiply 2x2 A*B
  (flet ((matrix-mult-2x2 (a11 a12 a21 a22  b11 b12 b21 b22)
	   (values
	    (+ (* a11 b11) (* a12 b21)) ;;   11
	    (+ (* a11 b12) (* a12 b22)) ;;   12
	    (+ (* a21 b11) (* a22 b21)) ;;   21
	    (+ (* a21 b12) (* a22 b22)))));; 22 
    ;;
    ;; compute D=Mxy*C^T
    (multiple-value-bind (d11 d12 d21 d22)
	(matrix-mult-2x2 mxx mxy mxy myy  c11 c21 c12 c22)
      ;; compute E=C*D
      (multiple-value-bind (muu muv mvu mvv)
	  (matrix-mult-2x2 c11 c12 c21 c22   d11 d12 d21 d22)
	(declare (ignorable mvu))
	(values muu mvv muv)))))

  
	    
	    



					   




	   
    
  
  