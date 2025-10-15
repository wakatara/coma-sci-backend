
;; generate some test images 

(in-package imutils)


(defun make-2d-quadratic-test-array
    (&key (nx 100) (ny 100) (x0 33.333) (y0 44.444)
       (a 0.5) (b 0.0) (c 0.5) (d 0.0) (e 0.0) (f 0.0))
  "Make an NX x NY image of form
     value = a(x-x0)^2 + b(x-x0)(y-y0) 
           + c(y-y0)^2 + d(x-x0) + e(y-y0) + c"
  (let ((arr (make-array (list ny nx) :element-type 'single-float)))
    (loop for iy below ny
	  for y = (- iy y0)
	  do
	  (loop 
	   for ix below nx
	   for x = (- ix x0)
	   for z = (+ (* a x x) (* b x y) (* c y y)
		      (* d x) (* e y) f)
	   do
	   (setf (aref arr iy ix) z)))
    arr))



(defun make-dot-image
    (&key (nx 100) (ny 100) (x0 10.0) (y0 20.0) (r 5.0)
       (val 10.0) (backd-val 1.0))
  "Make an image with a dot of radius R at X0 Y0"
  (let ((arr (make-array (list ny nx) :element-type 'single-float
			 :initial-element backd-val)))
    (loop for iy below ny
       for y = (- iy y0)
       do
	 (loop 
	    for ix below nx
	    for x = (- ix x0)
	    for z = (sqrt (+ (expt x 2)
			     (expt y 2)))
	    when  (<= z r)
	    do
	      (setf (aref arr iy ix) val)))
    arr))

;; the position angle in the following is verified - with a>b, small angles point
;; slightly right of vertical, ie from +x
(defun make-gaussian-image
    (&key 
       (nx 100) (ny 100) (x0 50.0) (y0 50.0) (a 5.0) (b 5.0)
       (theta 0.0)
       (norm 1.0) (backd-val 0.0))
  "Make an image with a gaussian of sigmas a,b with position angle THETA at X0 Y0.
THETA is defined as a position angle in degrees, with 0 in +y,
increasing in +x.

NORM is defined as the total photon flux, if the Gaussian were allowed
to extend to infinity."
  (let ((arr (make-array (list ny nx) :element-type 'single-float
			 :initial-element backd-val)))
    (loop 
       with lead = (float 
		    (/ norm  (* a b)  (* 2 pi))
		    1.0)
       with ct = (float (cos (* (/ pi 180) theta)) 1.0)
       with st = (float (sin (* (/ pi 180) theta)) 1.0)
       for iy below ny
       do
	 (loop 
	   for ix below nx
	   ;; major axis dist
	   for u = (+ (* st (- ix x0)) (* ct (- iy y0)))
	   ;; minor axis dist
	   for v = (+ (* ct (- ix x0)) (* st -1 (- iy y0)))
	   for z = (+  (expt (/ u a) 2) 
		       (expt (/ v b) 2))
	   for val = (* lead  (exp (* -0.5 z)))
	   do
	      (incf (aref arr iy ix) val)))
    arr))



(defun make-test-pattern-image
    (&key (nx 101) (ny 101) (backd-val 0.1))
  "Make an image with some gaussian dots and lines, to have an
  image with an obvious orientation"
  (let ((arr (make-array (list ny nx) :element-type 'single-float
			 :initial-element backd-val))
	(nx/2 (ash nx -1))
	(ny/2 (ash ny -1)))

    (flet ((make-gaussian (c x0 y0 size sigma)
	     (setf x0 (round x0)
		   y0 (round y0)
		   size (round size))
	     (loop
	       for ix from (max 0 (- x0 size))
		 to (min (1- nx) (+ x0 size))
	       do
		  (loop
		    for iy from (max 0 (- y0 size))
		      to (min (1- ny) (+ y0 size))
		    for r = (sqrt (+ (expt (- x0 ix) 2) 
				     (expt (- y0 iy) 2)))
		    when (<= r size)
		      do
			 (incf
			  (aref arr iy ix)
			  (* c
			     (exp (* -0.5
				     (expt  (/ r sigma) 2)))))))))
      
      (loop for pair in '((0.1 0.1) (0.1 0.9) (0.9 0.1)
			  (0.9 0.9) (0.7 0.7) (0.5 0.5)
			  (0.3 0.3) (0.3 0.7) (0.7 0.3))
	   for dx = (first pair) and dy = (second pair)
	   do
	   (make-gaussian 3.0
			  (floor (* dx nx))
			  (floor (* dy ny))
			  20.0 2.0))
      
      (flet ((draw-plus (x y)
	       (loop
		 with dx = (ceiling (* 0.05 nx))
		 with dy = (ceiling (* 0.05 ny))
		 with jx = (round (* x nx))
		 with jy = (round (* y ny))
		 for ix from (round (- (* x nx) dx))
		   to (round (+ (* x nx) dx))
		 for iy from (round (- (* y ny) dy))
		   to (round (+ (* y ny) dy))
		 do (setf (aref arr iy jx) 2.0)
		    (setf (aref arr jy ix) 2.0)
		    (setf (aref arr jx iy) 2.0)
		    (setf (aref arr ix jy) 2.0))))
	(draw-plus 0.5 0.2)
	(draw-plus 0.2 0.5))

      
	   
      (loop 
	 for ix from (round (* 1.3 nx/2)) below nx by 1
	 do 
	   (loop
	      for iy from (- ny/2 2) to (+ ny/2 2)
	      do
		(setf (aref arr  iy ix) 1.0)))

        (loop 
	  for iy from (round (* 1.3 ny/2))
	    below (round (* 0.8 ny)) by 1
	 do 
	   (loop
	      for ix from (- nx/2 2) to (+ nx/2 2)
	      do
		(setf (aref arr  iy ix) 1.0)))


      arr)))
	   

(defun make-bullseye-image (&key (nx 101) (ny 101) 
			    (ring-val 1.0)
			    (step-ring-val nil)
			    (backd-val 0.0) (ring-width 5)
			    x0 y0)
  "Make a bullesye image of size NX,NY with rings of width RING-WIDTH"
  (let* ((arr (make-array (list ny nx) :element-type 'single-float
			  :initial-element backd-val))
	 (nx/2 (ash nx -1))
	 (ny/2 (ash ny -1))
	 (x0 (or x0 (float nx/2)))
	 (y0 (or y0 (float ny/2))))
    (loop 
       for ix below nx
       do
	 (loop for iy below ny
	    for x = (- ix x0)
	    for y = (- iy y0)
	    for r = (sqrt (+ (* x x) (* y y)))
	    for nring = (floor r ring-width)
	    for is-ring = (evenp nring)
	    for val = (* ring-val
			 (if step-ring-val 
			     (+ 1.0 (* 0.5 nring))
			     1.0))
			     
	    when is-ring
	    do
	      (setf (aref arr iy ix) val)))
    arr))
	      

	       
	      
	 

