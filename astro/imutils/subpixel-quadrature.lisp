
#|

Some routines to do subpixel quadrature.  They are not necessarily
efficient, because they do not reuse boundary values on pixels.

This is a set of helper routines designed to be used only inside
IMUTILS.  

|#

(in-package imutils)



;; Integrate EXPRESSION that knows about variables XVAR,YVAR over a
;; pixel centered on X0, Y0.  If SUBPIX is true, then perform a 3x3
;; integration by Milne's rule inside the pixel - this will give
;; considerably greater precision, at the expense of an order of
;; magnitude more evaluation time.

;; This is not terribly efficient, because each pixel contains 9
;; interior points.  We could do better with the comparably accurate
;; Simpson's rule, and reusing the pixel-edge values, so that each
;; pixel would need just 4 new function evaluations.  But this would
;; involve much more bookkeeping.
;;
(defmacro subpixel-quadrature-inside-3x3
    (x0 y0  xvar yvar expression &key (float-type 'single-float))
  `(loop 
      with %sum of-type ,float-type = ,(coerce 0 float-type)
      for %iy of-type (integer -3 3) from -1 to 1
      for ,yvar  of-type ,float-type = (+ ,y0 (* ,(coerce 0.25d0 float-type) %iy))
      for %wy of-type (integer -2 2) = (if (= %iy 0) -1 2) ;; weight
      do 
	(loop
	   for %ix of-type (integer -3 3) from -1 to 1
	   for ,xvar  of-type ,float-type = (+ ,x0 
					       (* ,(coerce 0.25d0 float-type) %ix))
	   for %wx  of-type (integer -2 2) = (if (= %ix 0) -1 2);; weight
	   for %val of-type ,float-type =  (* %wx %wy ,expression)
	   do
	     (incf %sum %val))
      finally
	(return (* 
		 ,(coerce 1/9 float-type)
		   %sum)))) ;; 1/3x1/3 2D Milne term


 


;; Boole's rule 
;; value = (b-a)/90  (7 f0 + 32 f1 + 12 f2 + 32 f3 + 7 f1)
;; spacing is 0.25, and efficiency is (25-19)/25=65% of what would
;; be possible if we were to reuse border values - not great
;; but not shabby
(defmacro subpixel-quadrature-outside-5x5
    (x0 y0  xvar yvar expression  &key (float-type 'single-float))
  `(loop 
      with %sum of-type ,float-type = ,(coerce 0 float-type)
      for %iy of-type (integer -3 3) from -2 to 2
      for ,yvar  of-type ,float-type = (+ ,y0 (* ,(coerce 0.25d0 float-type) %iy))
      for %wy of-type (integer 7 32) = 
	(case (abs %iy)
	  (2 7)
	  (1 32)
	  (t 12))
      do 
	(loop
	   for %ix of-type (integer -3 3) from -2 to 2
	   for ,xvar  of-type ,float-type = 
	     (+ ,x0 (* ,(coerce 0.25d0 float-type) %ix))
	   for %wx  of-type (integer 7 32) = 
	     (case (abs %ix)
	       (2 7)
	       (1 32)
	       (t 12))
	   for %val of-type ,float-type =  (* %wx %wy ,expression)
	   do
	     (incf %sum %val))
      finally
	(return (* 
		 ,(coerce #.(/ 1d0 90 90) float-type)
		 %sum)))) ;;1/90^2 weight bode's term
  
 


;; A general routine for subpixel quadrature - subpix can be 1,3,5
;; It will choose an order 1,3, or 5 quadrature routine.
(defmacro subpixel-quadrature (x0 y0 xvar yvar subpix expression
			       &key (float-type 'single-float))
  (declare (type symbol xvar yvar))
  `(let ((%subpix ,subpix))
     (declare (type (member 1 3 5) %subpix))
     (cond ((= %subpix 1);; just the central pix
	    (let* ((,xvar ,x0)
		   (,yvar ,y0)
		   (%sum ,expression))
	      (declare (type ,float-type ,xvar ,yvar %sum))
	      %sum))
	   ((= %subpix 3)
	    (subpixel-quadrature-inside-3x3 ,x0 ,y0 ,xvar ,yvar ,expression
					    :float-type ,float-type))
	   (t
	    (subpixel-quadrature-outside-5x5 ,x0 ,y0 ,xvar ,yvar ,expression
					     :float-type ,float-type)))))
  
 
  
