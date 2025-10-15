

(in-package llsq)



;; orthoginal regression, minimizing the difference between the points
;; and a line

(defun orthogonal-line-fit (xvec yvec)
  "Return a line parameterized by 'z' given by x=x0+cZ, y=y0+sZ
where c,s are the cos and sin of the line's angle (angle is zero for +x,
increasing into +y).

No errors on XVEC, YVEC are supplied.

Returns
   (VALUES THETA/RADIANS X0 Y0 SUMDIST2)
where X0,Y0 are the centroid of the data and SUMDIST2 is a chisqr-like term
representing the summed square distance from the line."

  (declare (type (simple-array double-float (*)) xvec yvec))
  (let ((n (length xvec))
	(x0 0d0)
	(y0 0d0)
	(sxx 0d0)
	(syy 0d0)
	(sxy 0d0))
    (loop for x across xvec
	  for y across yvec
	  do (incf x0 x)
	     (incf y0 y))
    (setf x0 (/ x0 n))
    (setf y0 (/ y0 n))

    ;; subtract off centroid
    (loop for x across xvec
	  for y across yvec
	  for x* = (- x x0)
	  for y* = (- y y0)  
	  do (incf sxx (expt x* 2))
	     (incf syy (expt y* 2))
	     (incf sxy (* x* y*)))

    ;;;the solution of minimum summed distances is given by
    ;;  0 = (cos(theta)^2 - sin(theta)^2) 2 sxy - 2 sin(theta) cos(theta)(sxx-syy)
    ;;  but
    ;;    cos(theta)^2 - sin(theta)^2      = cos (2 theta)
    ;;    sin(theta) cos(theta)            = (1/2) sin(2 theta)
    ;; ==>  0 = cos(2 theta) sxy - (1/2) sin(2 theta) (sxx-syy)
    ;; ==>  sin(2 theta) / cos(2 theta)   = 2 sxy/(sxx-syy)  = tan(2 theta)

    (let* ((sxx-syy (- sxx syy))
	   (tan2theta
	     (if (> (abs sxx-syy) 1d-100)
		 (* 2d0 (/ sxy sxx-syy))
		 ;; the case of tiny sxx-syy, to prevent 1/0
		 (if (minusp sxx-syy)
		     -1d50
		     1d50)))
	   ;; We have a problem - theta can be a maximum or a minimum, so the correct solution
	   ;; (ignoring 180 degree degeneracy) is either theta, or theta+pi/2, so we compute
	   ;; both and see which gives a smaller scatter
	   (theta1 (* 0.5d0 (atan tan2theta)))
	   (theta2 (+ (/ pi 2) theta1)))

      ;; copute goodness of fit for theta1, theta2
      (flet ((compute-chisqr (theta)
	       (loop with st = (sin theta)
		     with ct = (cos theta)
		     for x across xvec
		     for y across yvec
		     for x* = (- x x0)
		     for y* = (- y y0)
		     ;; dot product of orthog vector (-st,ct) and x,y
		     for dist = (+ (* x* (- st))  (* y* ct))
		     sum (* dist dist))))
	(let ((chisqr1 (compute-chisqr theta1))
	      (chisqr2 (compute-chisqr theta2)))
	  ;;(format t "theta1=~,2F Dist1=~,4F~%"  (* (/ 180 pi) theta1) dist1)
	  ;;(format t "theta1=~,2F Dist1=~,4F~%"  (* (/ 180 pi) theta2) dist2)

	  (let ((theta 0d0)
		(chisqr 0d0))

	    ;; pick the correct angle modulo pi/2
	    (if (< chisqr1 chisqr2)
		(progn (setf theta theta1)
		       (setf chisqr chisqr1))
		(progn (setf theta theta2)
		       (setf chisqr chisqr2)))
		
	    ;; put theta into +/- pi
	    (setf theta (nth-value 1 (round theta pi)))
	    
	    (values theta
		    x0 y0
		    chisqr)))))))


;; generate test data for above
#+nil 
(defun make-orthogonal-regression-data (&key (theta 0d0) (x0 10d0) (y0 10d0) (npoints 100) (scat 0.05))
  (let*  ((xvec (make-array npoints :element-type 'double-float))
	  (yvec (make-array npoints :element-type 'double-float))
	  (ct (cos theta))
	  (st (sin theta)))

    (loop for i below npoints
	  for z = -1d0 then (+ z (/ 2d0 npoints)) ;; z goes from -1 to 1
	  for rx = (+  scat (* -2d0 (random scat)))
	  for ry = (+  scat (* -2d0 (random scat)))
	  for x = (+ x0 (* ct z) rx)
	  for y = (+ y0 (* st z) ry)
	  do (setf (aref xvec i) x)
	     (setf (aref yvec i) y))
    ;;
    (values xvec yvec)))
    
  

      
		    
	    
