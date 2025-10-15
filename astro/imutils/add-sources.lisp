
#|

precise addition of sources to an image, with subpixel resampling

|#


(in-package imutils)




(defun add-gaussian-to-image (image x0 y0  a b theta norm &key 
			      (dist nil) (subpix 1))
  "Add a Gaussian to an image, at X0,Y0, with major axis A and minor axis
B and position angle THETA, and total flux NORM.

NORM is defined as the total photon flux, if the Gaussian were allowed
to extend to infinity. 

SUBPIX is 1,3,5, the order of subpixel resampling.

WARNING - normalization is wrong for small small FWHM unless SUBPIX 
is used.

DIST is how far from X0,Y0 to sample the Gaussian.  If undefined (NIL),
then it is 10x MAX(A,B)"
  (declare (type image image)
	   (type (single-float -1e6 1e6)  x0 y0  a b theta norm)
	   (type (or null (single-float 0.0 1e4)) dist)
	   (type (member 1 3 5) subpix)
	   (optimize speed))
  (loop 
     with ny = (array-dimension image 0)
     with nx = (array-dimension image 1)
     with rdist of-type single-float = 
       (or dist (* 10 (max a b)))
     with rdist2  of-type single-float = (expt rdist 2)
     with ix0 of-type imindex = (max 0 (floor (- x0 rdist)))
     with ix1 of-type imindex = (min (1- nx) (ceiling (+ x0 rdist)))
     with iy0 of-type imindex = (max 0 (floor (- y0 rdist)))
     with iy1 of-type imindex = (min (1- ny) (ceiling (+ y0 rdist)))
     with lead of-type single-float
       = (float 
	  (/ norm  (* a b  #.(float (* 2 pi) 1.0)))
	  1.0)
     with ct of-type single-float = (cos (* 0.0174532925199 theta))
     with st of-type single-float = (sin (* 0.0174532925199 theta))
     for iy of-type imindex from iy0 to iy1
     for y of-type single-float = (- iy y0)
     do
       (loop 
	  for ix  of-type imindex from ix0 to ix1
	  for x of-type single-float = (- ix x0)
	  when (<= (+ (* x x) (* y y)) rdist2) ;; only inside disk, not ix0,ix1,iy0,iy1 rectangle
	  do
	    (let ((pixval 
		   (subpixel-quadrature  
		    x y xx yy subpix 
		    (let* ((u  (+ (* st xx) (* ct yy))) ;; major axis dist
			   (v  (+ (* ct xx) (* st -1 yy))) ;; minor axis dist
			   (z (+  (expt (/ u a) 2) 
				  (expt (/ v b) 2)))
			   (val (* lead  (exp (* -0.5 z)))))
		      val))))
	      (incf (aref image iy ix) pixval)))))





(defun add-moffat-to-image (image x0 y0 a b beta theta norm &key 
			      (dist nil) (subpix 1))
  "Add an elliptical Moffat function to an image, at X0,Y0, with major
axis A and minor axis B and position angle THETA, and total flux NORM.

NORM is defined as the total photon flux, if the function were allowed
to extend to infinity.

Equation is

  F(x,y;x0,y0,a,b,a,b,norm) = NORM*(beta-1)/(pi*a*b)  * [1+R^2]^-beta

where R^2=u^2/a^2 + v^2/b^2 and
  u=sin(theta)*(x-x0)+cos(theta)*(y-y0)
  v=cos(theta)*(x-x0)-con(theta)*(y-y0)

SUBPIX is 1,3,5, the order of subpixel resampling.

WARNING - normalization is wrong for small small FWHM unless SUBPIX 
is used.

DIST is how far from X0,Y0 to sample the Gaussian.  If undefined (NIL),
then it is 10x MAX(A,B).  The Moffet function extends very far, so that 
this might not be enough, so one might miss about 0.1% of the flux.

A useful rule is that for a=b,  FWHM=a*2*sqrt(2^(1/beta)-1) and beta is often
2.5 so that FWHM=a*1.13050  or a=FWHM*0.88456."

  (declare (type image image)
	   (type (single-float -1e6 1e6)  x0 y0  a b theta beta norm)
	   (type (or null (single-float 0.0 1e4)) dist)
	   (type (member 1 3 5) subpix)
	   (optimize speed))
  (loop 
     with ny = (array-dimension image 0)
     with nx = (array-dimension image 1)
     with rdist of-type single-float = 
       (or dist (* 10 (max a b)))
     with rdist2  of-type single-float = (expt rdist 2)
     with ix0 of-type imindex = (max 0 (floor (- x0 rdist)))
     with ix1 of-type imindex = (min (1- nx) (ceiling (+ x0 rdist)))
     with iy0 of-type imindex = (max 0 (floor (- y0 rdist)))
     with iy1 of-type imindex = (min (1- ny) (ceiling (+ y0 rdist)))
     with lead of-type single-float
       =  (* norm (- beta 1.0) (/ 1.0 3.1415926535 a b))
     with ct of-type single-float = (cos (* 0.0174532925199 theta))
     with st of-type single-float = (sin (* 0.0174532925199 theta))
     for iy of-type imindex from iy0 to iy1
     for y of-type single-float = (- iy y0)
     do
       (loop 
	  for ix  of-type imindex from ix0 to ix1
	  for x of-type single-float = (- ix x0)
	  when (<= (+ (* x x) (* y y)) rdist2) ;; only inside disk, not ix0,ix1,iy0,iy1 rectangle
	  do
	    (let ((pixval 
		   (subpixel-quadrature 
		    x y xx yy subpix 
		    (let* ((u  (+ (* st xx) (* ct yy))) ;; major axis dist
			   (v  (+ (* ct xx) (* st -1 yy))) ;; minor axis dist
			   (r2 (+  (expt (/ u a) 2) 
				   (expt (/ v b) 2)))
			   (val (* lead  (expt (+ 1.0 r2) (- beta)))))
		      val))))
	      (incf (aref image iy ix) pixval)))))

  
(defun add-radial-function-to-image (image x0 y0  func
				     &key 
				       (dist nil) (subpix 1))
  "Add a a radial function (FUNC R) to an image, at X0,Y0.

SUBPIX is 1,3,5, the order of subpixel resampling.

DIST is how far from X0,Y0 to sample the function.  If
undefined (NIL), then it is the whole image."
  (declare (type image image)
	   (type (function (single-float) single-float) func)
	   (type (or null (single-float 0.0 1e4)) dist)
	   (type (member 1 3 5) subpix)
	   (optimize speed))
  (loop 
     with ny = (array-dimension image 0)
     with nx = (array-dimension image 1)
     with rdist of-type single-float = 
       (or dist (* 1.0 (+ 1 (max (array-dimension image 0) (array-dimension image 1)))))
     with rdist2  of-type single-float = (expt rdist 2)
     with ix0 of-type imindex = (max 0 (floor (- x0 rdist)))
     with ix1 of-type imindex = (min (1- nx) (ceiling (+ x0 rdist)))
     with iy0 of-type imindex = (max 0 (floor (- y0 rdist)))
     with iy1 of-type imindex = (min (1- ny) (ceiling (+ y0 rdist)))
     for iy of-type imindex from iy0 to iy1
     for y of-type single-float = (- iy y0)
     do
       (loop 
	  for ix  of-type imindex from ix0 to ix1
	  for x of-type single-float = (- ix x0)
	  when (<= (+ (* x x) (* y y)) rdist2) ;; only inside disk, not ix0,ix1,iy0,iy1 rectangle
	  do
	    (let ((pixval 
		   (subpixel-quadrature  
		    x y xx yy subpix
		    (funcall func (sqrt (+ (* x x) (* y y)))))))
	      (incf (aref image iy ix) pixval)))))
