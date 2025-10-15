#|

Some quaternion math for doing 3d rotations - see wikipedia article

Basic rules:

0. a quaternion is q = [c1, i c2, j c3, k c4]

1. to rotate around x,y,z unit vector by theta, 
   q = [ cos(theta/2),  x sin(theta/2), y sin(theta/2), z sin(theta/2) ]

2. rotations are done by representing X,Y,Z as p=[0, iX, jY, kZ]
   and applying p_rotated = q p q^-1

3. quaternions can be composited so rotation quaternion  q1 followed by q2 is the
   product q2*q1

4. there are functions to transform conventional representations of rotations
   like euler angles, vector -> vector, and lat-and-lon -> lat-and-lon

5. There is a STRUCT version of these routines that uses a quaternion structure
   and double precision 3 vectors



we don't put in optimizations, but declare them all inline, so that
code using these functions will be optimized.  Hope this works.

|#


(in-package quaternion)

(declaim (inline quaternion-multiply invert-quaternion
		 make-rotation-quaternion quaternion-rotate
		 rotate-around-vector convert-quaternion-to-euler-axis-and-angle))

(defun quaternion-multiply (a b c d   e f g h)
  "Multiply two quaternions (A B C D) and (E F G H)"
  (values
   (+ (* +1 a e) (* -1 b f) (* -1 c g) (* -1 d h))
   (+ (* +1 a f) (* +1 b e) (* +1 c h) (* -1 d g))
   (+ (* +1 a g) (* +1 c e) (* +1 d f) (* -1 b h))
   (+ (* +1 a h) (* +1 d e) (* +1 b g) (* -1 c f))))

 
(defun invert-quaternion (a b c d)
  "Make inverse of quaternion (a b c d); left and right inverses are
 identical"
  (let ((norm (+ (* a a) (* b b) (* c c) (* d d))))
    (values 
     (/ a norm)
     (- (/ b norm))
     (- (/ c norm))
     (- (/ d norm)))))

(defun make-rotation-quaternion (x y z theta)
  "Make a rotation quaternion 
  [cos(theta/2),  x sin(theta/2), y sin(theta/2), z sin(theta/2) ]
where theta is in radians and x,y,z must already be unit-normalized."
  (let ((ct (cos (/ theta 2)))
	(st (sin (/ theta 2))))
    (values
     ct 
     (* x st) (* y st) (* z st))))


(defun convert-quaternion-to-euler-axis-and-angle (a b c d)
"Convert quaternion to euler axis and angle; the first A element of the quaternion
is the angle, and the B,C,D elements are the vector component - returns 
 (VALUES X Y Z THETA)"
  (let ((norm (sqrt (abs (+ (* b b) (* c c) (* d d))))))
    (values (/ b norm)
	    (/ c norm)
	    (/ d norm)
	    (* 2 (acos a)))))
    


(defun rotate-around-vector (x y z xr yr zr theta)
  "Rotate vector x,y,z by vector xr,yr,zr and angle theta; theta is
clockwise viewed from the origin down xr,yr,zr."
  (multiple-value-bind (a b c d)
      (make-rotation-quaternion  xr yr zr theta)
    (let*  ((t2 (* a b))
	    (t3 (* a c))
	    (t4 (* a d))
	    (t5 (* -1 b b))
	    (t6 (* b c))
	    (t7 (* b d))
	    (t8 (* -1 c c))
	    (t9 (* c d))
	    (t10 (* -1 d d))
	    ;;
	    (xn (+ (* 2 (+ (* (+ t8 t10) x) 
			   (* (- t6 t4) y) (* (+ t3 t7) z)))
		   x))
	    (yn (+ (* 2 (+ (* (+ t4 t6) x) 
			   (* (+ t5 t10) y) (* (- t9 t2) z)))
		   y))
	    (zn (+ (* 2 (+ (* (- t7 t3) x) 
			   (* (+ t2 t9) y) (* (+ t5 t8) z)))
		   z)))
      (values xn yn zn))))
	    
(defun quaternion-rotate (x y z a b c d)
  "given a vector x,y,z and a rotation quaternion q = a,b,c,d, rotate x,y,z
according to XYZnew=Q XYZ Q^-1"
  (let*  ((t2 (* a b))
	  (t3 (* a c))
	  (t4 (* a d))
	  (t5 (* -1 b b))
	  (t6 (* b c))
	  (t7 (* b d))
	  (t8 (* -1 c c))
	  (t9 (* c d))
	  (t10 (* -1 d d))
	  ;;
	  (xn (+ (* 2 (+ (* (+ t8 t10) x) 
			 (* (- t6 t4) y) (* (+ t3 t7) z)))
		 x))
	  (yn (+ (* 2 (+ (* (+ t4 t6) x) 
			 (* (+ t5 t10) y) (* (- t9 t2) z)))
		 y))
	  (zn (+ (* 2 (+ (* (- t7 t3) x) 
			 (* (+ t2 t9) y) (* (+ t5 t8) z)))
		 z)))
    (values xn yn zn)))

  
(declaim (inline %cross-product %dot-product))
;;
(defun %cross-product (a b c  d e f) ;; cross product of (a b c) and (d e f)
  (values
   (- (* b f) (* c e))
   (- (* c d) (* a f))
   (- (* a e) (* b d ))))

(defun %dot-product (a b c  d e f)
  (float (+ (* a d) (* b e) (* c f)) 1d0))



;; force (x1 y1 z1) to be orthogonal to (x2 y2 z2) by subtracting
;; compononent of first vector from second and normalizing both
(defun %force-orthogonalize (x1 y1 z1 x2 y2 z2)
  (declare (type double-float x1 y1 z1 x2 y2 z2))
  (let ((r1 (sqrt (+ (expt x1 2) (expt y1 2) (expt z1 2))))
	(r2 (sqrt (+ (expt x2 2) (expt y2 2) (expt z2 2)))))
    (setf x1 (/ x1 r1)
	  y1 (/ y1 r1)
	  z1 (/ z1 r1))
    (setf x2 (/ x2 r2)
	  y2 (/ y2 r2)
	  z2 (/ z2 r2))
    ;; now they're normalized
    (let ((dotprod (+ (* x1 x2) (* y1 y2) (* z1 z2))))
      ;; and subtract the dot product of the two from vector 2
      (decf x2 (* dotprod x1))
      (decf y2 (* dotprod y1))
      (decf z2 (* dotprod z1))
      ;; and re-normalize
      (let ((r2 (sqrt (+ (expt x2 2) (expt y2 2) (expt z2 2)))))
	(setf x2 (/ x2 r2)
	      y2 (/ y2 r2)
	      z2 (/ z2 r2))
	(values x1 y1 z1 x2 y2 z2)))))
      
	
		      

;; IT SEEMS LIKE THERE MUST BE A NICER WAY TO DO THE FOLLOWING.
(defun compute-quaternion-for-transform (x1f y1f z1f  x2f y2f z2f  ;; note pattern A A
					 x1t y1t z1t  x2t y2t z2t) ;;              B B
  "Compute the rotation quaternion that both transforms (where f=From, and t=To)
  (X1f,Y1f,Z1f)->(X1t,Y1t,Z1t)  and   (X2f,Y2f,Z2f)->(X2t,Y2t,Z2t).
The vectors are forced to be orthogonal.  This works by constructing the third
vector from the cross product, using it to build a transformation matrix,
and extracting the quaternion from the transformation matrix."

  ;; force x1f,b to be orthogonal
  (multiple-value-setq (x1f y1f z1f  x2f y2f z2f)
    (%force-orthogonalize x1f y1f z1f   x2f y2f z2f))
  ;;
  (multiple-value-setq (x1t y1t z1t  x2t y2t z2t)
    (%force-orthogonalize x1t y1t z1t   x2t y2t z2t))
  
  (multiple-value-bind (x3f y3f z3f)
      (%cross-product x1f y1f z1f  x2f y2f z2f)
    (multiple-value-bind (x3t y3t z3t)
	(%cross-product x1t y1t z1t  x2t y2t z2t)
      ;; build the transform matrix - it is
      ;; M=AB where A has the output '2' vectors as columns, and B has the '1'
      ;; input vectors as rows
      (let* ((m (make-array '(3 3) :element-type 'double-float)))
	;;(declare (dynamic-extent m))

	(setf (aref m 0 0) (%dot-product x1t x2t x3t  x1f x2f x3f)
	      (aref m 1 0) (%dot-product y1t y2t y3t  x1f x2f x3f)
	      (aref m 2 0) (%dot-product z1t z2t z3t  x1f x2f x3f)
	      ;;
	      (aref m 0 1) (%dot-product x1t x2t x3t  y1f y2f y3f)
	      (aref m 1 1) (%dot-product y1t y2t y3t  y1f y2f y3f)
	      (aref m 2 1) (%dot-product z1t z2t z3t  y1f y2f y3f)
	     ;;
	      (aref m 0 2) (%dot-product x1t x2t x3t  z1f z2f z3f)
	      (aref m 1 2) (%dot-product y1t y2t y3t  z1f z2f z3f)
	      (aref m 2 2) (%dot-product z1t z2t z3t  z1f z2f z3f))
	#+nil
	(progn
	  (format t "dotprod 1f 1f = ~A~%" (%dot-product x1f y1f z1f x1f y1f z1f))
	  (format t "dotprod 2f 2f = ~A~%" (%dot-product x2f y2f z2f x2f y2f z2f))
	  (format t "dotprod 1f 2f = ~A~%" (%dot-product x1f y1f z1f x2f y2f z2f))
	  (format t "dotprod 1f 3f = ~A~%" (%dot-product x1f y1f z1f x3f y3f z3f))
	  (format t "dotprod 2f 3f = ~A~%" (%dot-product x2f y2f z2f x3f y3f z3f))
	  (format t "dotprod 1t 2t = ~A~%" (%dot-product x1t y1t z1t x2t y2t z2t))
	  (format t "dotprod 1t 3t = ~A~%" (%dot-product x1t y1t z1t x3t y3t z3t))
	  (format t "dotprod 2t 3t = ~A~%" (%dot-product x2t y2t z2t x3t y3t z3t)))
	
;; 	(print m)
;; 	(print (list (list x1f y1f z1f)
;; 		     (list x2f y2f z2f)
;; 		     (list x3f y3f z3f)))
;; 	(print (list (list x1t y1t z1t)
;; 		     (list x2t y2t z2t)
;; 		     (list x3b y3b z3b)))

	(let* ((u (cond ((and (>= (aref m 0 0) (aref m 1 1)) (>= (aref m 0 0) (aref m 2 2)))
			 0)
			((>= (aref m 1 1) (aref m 2 2))  
			 1)
			(t 2)))
	       ;; uvw are an even permutation like 123 or 231
	       (v (cond ((= u 0) 1)
			((= u 1) 2)
			((= u 2) 0)))
	       (w (cond ((= v 0) 1)
			((= v 1) 2)
			((= v 2) 0)))
	       ;;
	       (quu (aref m u u))
	       (qvv (aref m v v))
	       (qww (aref m w w))
	       (qwv (aref m w v))
	       (qvw (aref m v w))
	       (quv (aref m u v))
	       (qvu (aref m v u))
	       (qwu (aref m w u))
	       (quw (aref m u w))
	       ;;
	       (r (sqrt (the (double-float 0d0)
			  (+ 1d0 quu (- qvv) (- qww)))))
	       ;; now the output quaternion p
	       (p0 (/ (- qwv qvw) (* 2 r)))
	       (pu (/ r 2))
	       (pv (/ (+ quv qvu) (* 2 r)))
	       (pw (/ (+ qwu quw) (* 2 r))))
	  ;;
	  (values p0 
		  (cond ((= u 0) pu)
			((= v 0) pv)
			((= w 0) pw))
		  (cond ((= u 1) pu)
			((= v 1) pv)
			((= w 1) pw))
		  (cond ((= u 2) pu)
			((= v 2) pv)
			((= w 2) pw))))))))
		  
		
(defun compute-quaternion-for-angular-transform
    (lon1f lat1f  lon1t lat1t  ;; note pattern F -> T
     lon2f lat2f  lon2t lat2t) ;;              F -> T
  "Given spherical coordinates defined by LONgitude and LATitude  
in RADIANS, create a quaternion that transforms radian quanities
   (LAT1F, LON1F) -> (LAT1T, LAT2T)
   (LAT2F, LON2F) -> (LAT2T, LAT2T)"
  (declare (type (double-float -1d6 1d6)
		 lat1f lon1f  lat1t lon1t lat2f lon2f  lat2t lon2t))
  (flet ((xyz-for-lat-lon (lat lon)
	   (declare (type double-float lat lon))
	   (let ((cos-lat (cos lat))
		 (sin-lat (sin lat))
		 (cos-lon (cos lon))
		 (sin-lon (sin lon)))
	     (values (* cos-lat cos-lon)  ;; x
		     (* cos-lat sin-lon)  ;; y
		     sin-lat))))          ;; z
    ;; first pair of points
    (multiple-value-bind (x1f y1f z1f)
	(xyz-for-lat-lon lat1f lon1f)
       (multiple-value-bind (x1t y1t z1t)
	   (xyz-for-lat-lon lat1t lon1t)
	 ;; second pair of points
	 (multiple-value-bind (x2f y2f z2f)
	     (xyz-for-lat-lon lat2f lon2f)
	   (multiple-value-bind (x2t y2t z2t)
	       (xyz-for-lat-lon lat2t lon2t)
	     ;; note FF TT (from/to)  pattern for the next function
	     (compute-quaternion-for-transform x1f y1f z1f  x2f y2f z2f
					       x1t y1t z1t  x2t y2t z2t)))))))
	 
  

		     
(defun compute-euler-axis-and-angle-for-transform (x1f y1f z1f  x2f y2f z2f
						   x1t y1t z1t  x2t y2t z2t)
  "Compute the axis and angle that both transforms 
  (X1f,Y1f,Z1f)->(X1t,Y1t,Z1t)  and   (X2f,Y2f,Z2f)->(X2t,Y2t,Z2t).
The vectors must be orthonormal.  This works by constructing the third
vector from the cross product, using it to build a transformation matrix,
and extracting the quaternion from the transformation matrix, and getting the
euler axis and angle from the quaternion."		     
  (multiple-value-bind (a b c d) ;; the quaternion
      (compute-quaternion-for-transform x1f y1f z1f  x2f y2f z2f
					x1t y1t z1t  x2t y2t z2t)
    (convert-quaternion-to-euler-axis-and-angle a b c d)))
      
