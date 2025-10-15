#|
  Some routines for manipulating 3-vectors.

  To do: rotations, matrices, euler angles, etc.

  Also see the QUATERNION package for useful rotations, particularly
  rotations using euler vector and euler angle.


|#


(defpackage three-vector
  (:use #:cl)
  (:nicknames #:3vec)
  (:export #:make-3vec
	   #:3vec-norm
	   #:cross-product-into-dest
	   #:cross-product
           #:dot-product
	   #:normalize
	   #:normalize!
	   #:create-two-orthog-vectors
	   #:add-3-3vectors
	   #:add-3vector-to-vdest
	   #:rotate-two-vectors
	   #:subtract-3vector-component-along
	   
	   #:3vector-from-spherical-coordinates
	   #:spherical-coordinates-from-3vector
	   #:spherical-central-angle
	   ;;
	   #:make-3matrix
	   #:3matrix-times-3vector
	   ))
(in-package three-vector)

(deftype 3vec ()
  `(simple-array double-float (3)))

(deftype 3matrix ()
  `(simple-array double-float (3 3)))

(declaim (inline make-3vec))

(defun make-3vec (&optional x0 x1 x2)
  (let ((v (make-array 3 :element-type 'double-float :initial-element 0d0)))
    (when x0 (setf (aref v 0) (float x0 1d0)))
    (when x1 (setf (aref v 1) (float x1 1d0)))
    (when x2 (setf (aref v 2) (float x2 1d0)))
    v))

(defun make-3matrix ()
  (make-array '(3 3) :element-type 'double-float))


(declaim (inline 3vec-norm))

(defun 3vec-norm (v)
  (declare (type 3vec v)
	   (optimize speed))
  (the (double-float 0d0)
       (sqrt (the (double-float 0d0)
		  (+ (expt (aref v 0) 2)
		     (expt (aref v 1) 2)
		     (expt (aref v 2) 2))))))


(defun cross-product-into-dest (v1 v2 v-dest)
  (declare (type 3VEC v1 v2 v-dest)
	   (optimize speed))
  (setf (aref v-dest 0)
	(- (* (aref v1 1) (aref v2 2))  (* (aref v1 2) (aref v2 1))))
  (setf (aref v-dest 1)
	(- (* (aref v1 2) (aref v2 0))  (* (aref v1 0) (aref v2 2))))
  (setf (aref v-dest 2)
	(- (* (aref v1 0) (aref v2 1))  (* (aref v1 1) (aref v2 0))))
  v-dest)

(defun cross-product (v1 v2 &key (vdest nil))
  (declare (type 3vec v1 v2)
	   (type (or null 3vec) vdest)
	   (optimize  speed))
  (cross-product-into-dest v1 v2 
			   (or vdest (make-array 3 :element-type 'double-float))))
  
  

(declaim (inline dot-product))

(defun dot-product (v1 v2)
  (declare (type 3vec v1 v2)
	   (optimize  speed))
  (+ (* (aref v1 0) (aref v2 0) )
     (* (aref v1 1) (aref v2 1) )
     (* (aref v1 2) (aref v2 2) )))


(defun normalize (v &key (vdest nil))
  "Normalize vector V, placing result into VDEST, which is a new vector
by default, but may be V; return normalized output vector."
  (declare (type 3vec v)
	   (type (or null 3vec) vdest)
	   (optimize  speed))
  (loop with vout = (or vdest (make-array 3 :element-type 'double-float))
	with norm of-type double-float = (sqrt (+ (expt (aref v 0) 2)
						  (expt (aref v 1) 2)
						  (expt (aref v 2) 2)))
	for i below 3
	do (setf (aref vout i) (/ (aref v i) norm))
	finally (return vout)))

(defun normalize! (v)
  "Normalize a vector V in place."
  (normalize v :vdest v))
  
	
 
(defun create-two-orthog-vectors (v &key (v1 nil) (v2 nil))
  "Given a 3 vector V, return two orthonormal vectors V1 V2 to form an
orthogonal triad, satisfying V2=VxV1 - V1 and V2 are created unless
they are given as keywords.  V1 and V2 are normalized, but V is left
as-is."
  (declare (type 3vec v)
	   (type (or null 3vec) v1 v2)
	   (optimize  speed))
  (let ((v1 (or v1  (make-array 3 :element-type 'double-float)))
	(v2 (or v2  (make-array 3 :element-type 'double-float))))
    ;;
    ;; for v1, rotate largest and smallest elements of v, negate one of them,
    ;; and set middle element to 0
    (loop 
     with xmax of-type double-float = 0d0 ;; biggest abs element and its index
     with imax = 0
     with xmin of-type double-float = most-positive-double-float ;; smallest ..
     with imin = 0
     for i below 3
     for x of-type double-float across v
     do
     (when (> (abs x) xmax) 
       (setf xmax (abs x))
       (setf imax i))
     (when (< (abs x) xmin) 
       (setf xmin (abs x))
       (setf imin i))
     ;;
     finally 
     (fill v1 0d0)
     ;; swap biggest and smallest, and let the middle 1 be zero
     (setf (aref v1 imax) (aref v imin))
     (setf (aref v1 imin) (- (aref v imax)))
     (normalize v1 :vdest v1))
    ;;
    ;; v2 is just cross product then
    (cross-product-into-dest v v1 v2)
    (normalize v2 :vdest v2)
    ;;
    (values v1 v2)))


(defun rotate-two-vectors (v1 v2 theta/rad &key (v1-out nil) (v2-out nil))
  (declare (type 3vec v1 v2)
	   (type (or null 3vec) v1-out v2-out)
	   (type double-float theta/rad))
  "Rotate v1 and v2 by THETA/RAD (in radians), so that theta=0 preserves v1 and v2,
and theta=pi/2 transforms v1->v2, v2->-v1"
  (loop 
   with v1-out = (or v1-out (make-array 3 :element-type 'double-float))
   with v2-out = (or v2-out (make-array 3 :element-type 'double-float))
   with costheta = (cos theta/rad)
   with sintheta = (sin theta/rad)
   for i below 3
   for x1 of-type double-float = (aref v1 i)
   for x2 of-type double-float = (aref v2 i)
   do
   (setf (aref v1-out i) (+ (* x1 costheta) (* x2 sintheta)))
   (setf (aref v2-out i) (+ (* x2 costheta) (* -1 x1 sintheta)))
   finally
   (return (values v1-out v2-out))))




;; this is useful for turning a basis of v1,v2,v3 into a new direction
(defun add-3-3vectors (c1 c2 c3  v1 v2 v3 &key (vdest nil))
  "Add c1*V1+c2*V2+c3*V3 where ci are coefficients and Vi are 3-vectors;
VDEST is NIL or a destination"
  (declare (type 3vec v1 v2 v3)
	   (type double-float c1 c2 c3)
	   (type (or null 3vec) vdest)
	   (optimize  speed))
  (loop
   with vout = (or vdest (make-array 3 :element-type 'double-float))
   for i below 3
   do
   (setf (aref vout i) (+ (* c1 (aref v1 i))
			  (* c2 (aref v2 i))
			  (* c3 (aref v3 i))))
   finally (return vout))) 

(defun add-3vector-to-vdest (c v vdest)
  "Add c*V to VDEST"
  (declare (type 3vec v vdest)
	   (type double-float c)
	   (optimize  speed))
  (loop for i below 3
	do (incf (aref vdest i) (* c (aref v i)))))
   
 

(defun subtract-3vector-component-along (v1 v2 &key (vdest nil) (normalize nil))
  "Remove the component of v1 in direction of v2; returning
  V2-(V1.V2)/|V1| in VDEST.  If NORMALIZE is set, then normalize result."
  (declare (type 3vec v1 v2)
	   (type (or null 3vec) vdest))
  (when (eq v1 vdest) (error "V1 cannot be VDEST"))
  (let* ((vdest (or vdest (make-3vec)))
	 (norm (3vec-norm v1))
	 (dot (dot-product v1 v2))
	 (f (- (/ dot norm))))
    (loop for i below 3 
       do (setf (aref vdest i) (aref v2 i)))
    (add-3vector-to-vdest f v1 vdest)
    (when normalize (normalize vdest :vdest vdest))
    vdest))



(declaim (inline 3vector-from-spherical-coordinates)) ;; because it takes dbls

(defun 3vector-from-spherical-coordinates (theta phi r &key (vdest nil))
  "Given theta (latitude) from -pi/2 to +pi/2 and phi (longitude)
from 0 to 2pi, return a 3vector pointing in this direction.  
Theta=0, phi=0 is along the +X direction, and the coordinates are right-handed, so that
Theta=0,pi/2 in +Y direction, and Theta=pi/2 is in +Z."
  (declare (type (or null 3vec) vdest)
	   (type real theta phi r))
  (let* ((3vec (or vdest (make-3vec)))
	 (r (float r 1d0))
	 (theta (float theta 1d0))
	 (phi (float phi 1d0))
	 (ct (cos theta))
	 (st (sin theta))
	 (cp (cos phi))
	 (sp (sin phi)))
    (setf (aref 3vec 0) (* r ct cp)
	  (aref 3vec 1) (* r ct sp)
	  (aref 3vec 2) (* r st))
    3vec))


(declaim (inline spherical-coordinates-from-3vector)) ;; because it returns dbls

(defun spherical-coordinates-from-3vector (v)
  "Given a 3vector V, return 
  (VALUES THETA PHI R), with THETA=latitude and PHI=longitude, 
so that THETA=0, PHI=0 points at +x, and PHI=PI/2 points +Y, and
THETA=PI/2 points at +Z.  Return (VALUES THETA PHI R)."
  (declare (type 3vec v))
  (let* ((x (aref v 0))
	 (y (aref v 1))
	 (z (aref v 2))
	 (r (the (double-float 0d0)
		 (sqrt
		  (the (double-float 0d0)
		       (+ (expt x 2) (expt y 2) (expt z 2))))))
	 (r% (if (zerop r) 1d0 r))
	 (theta (the double-float
		     (asin
		      (the (double-float -1d0 1d0) (/ z r%)))))
	 (phi (the double-float (atan y x))))
    (values theta phi r)))

 
(declaim (inline spherical-central-angle)) ;; because it takes/returns dbls

;; see https://en.wikipedia.org/wiki/Great-circle_distance
;; but note that our notation is different, and PHI is latitude,
;; not longitude.
(defun spherical-central-angle (theta1 phi1 theta2 phi2)
  "Compute the spherical angle between angle pairs THETA1,PHI1 and
THETA2,PHI2 on a sphere, using the precise Vincenty formula.  All
angles are radians, and THETA is a latitude, and PHI is a longitude"
  (let ((lambda1 (float phi1 1d0))   ;; rename to match wikipedia notation
	(phi1    (float theta1 1d0)) ;; note that we renamed PHI->THETA!
	(lambda2 (float phi2 1d0))
	(phi2    (float theta2 1d0)))

    (let* ((dlambda (- lambda1 lambda2))
	   (cdl (cos dlambda))
	   (sdl (sin dlambda))
	   ;;
	   (sp1 (sin phi1))
	   (cp1 (cos phi1))
	   (sp2 (sin phi2))
	   (cp2 (cos phi2))
	   ;;
	   (a (expt (* cp2 sdl) 2))
	   (b (expt
	       (- (* cp1 sp2) (* sp1 cp2 cdl))
	       2))
	   (c (+ (* sp1 sp2) (* cp1 cp2 cdl))))

      (atan (sqrt (the (double-float 0d0) (+ a b)))
	    c))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some matrix ops

(defun 3matrix-times-3vector (matrix v &key vdest)
  (declare (type 3matrix matrix)
	   (type 3vec v)
	   (type (or null 3vec) vdest)
	   (optimize speed))
  (let ((vout (or vdest (make-3vec))))
    (loop for iy below 3
	  do
	     (setf (aref vout iy) 0d0)
	     (loop for ix below 3
		   do (incf (aref vout iy)
			    (* (aref v ix)
			       (aref matrix iy ix)))))
    vout))
		   
