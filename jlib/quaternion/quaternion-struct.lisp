#|
 routines to wrap quaternion in a structure, and treat xyz as double
 float vectors


|#

(in-package quaternion)

(deftype dblvec3 () '(simple-array double-float (3)))

(defun make-dblvec3 (&optional x y z)
  (declare (type (or null double-float) x y z))
  (let ((v (make-array 3 :element-type 'double-float)))
    (when x (setf (aref v 0) x))
    (when y (setf (aref v 1) y))
    (when z (setf (aref v 2) z))
    v))

(defstruct quaternion
  (r 0d0 :type double-float)
  (i 0d0 :type double-float)
  (j 0d0 :type double-float)
  (k 0d0 :type double-float))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros to pack and unpack vecs and structs easily
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; build a quaternion struct with the 4 values provided by body
(defmacro %build-qs-from-values (&body body)
  `(multiple-value-bind (%r %i %j %k)
       (progn ,@body)
     (make-quaternion :r %r :i %i :j %j :k %k)))
					;
(defmacro %with-quaternion-values ((q rvar ivar jvar kvar) &body body)
  `(let ((,rvar (quaternion-r ,q))
	 (,ivar (quaternion-i ,q))
	 (,jvar (quaternion-j ,q))
	 (,kvar (quaternion-k ,q)))
     (progn ,@body)))

(defmacro %with-dblvec3-values ((vec xvar yvar zvar) &body body)
  `(let ((,xvar (aref ,vec 0))
	 (,yvar (aref ,vec 1)) 
	 (,zvar (aref ,vec 2)))
     (progn ,@body)))

(defmacro  %build-dblvec3-from-values (&body body)
  `(multiple-value-bind (%x %y %z)
       (progn ,@body)
     (make-dblvec3 %x %y %z)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-rotation-quaternion-struct  (vec theta)
  "Create a quaternion struct that rotates around NORMALIZED vector VEC by THETA"
  (declare (type dblvec3 vec))
  (%with-dblvec3-values (vec x y z)
    (%build-qs-from-values (make-rotation-quaternion x y z theta))))

(defun quaternion-struct-multiply (q1 q2)
  "Compute the product of quaternion structs Q1,Q2"
  (declare (type quaternion q1 q2))
  (%build-qs-from-values
   (%with-quaternion-values (q1  a1 b1 c1 d1)
     (%with-quaternion-values (q2  a2 b2 c2 d2)
       (quaternion-multiply a1 b1 c1 d1
			    a2 b2 c2 d2)))))


(defun invert-quaternion-struct (q)
  "Invert the quaternion struct Q"
  (declare (type quaternion q))
  (%build-qs-from-values
   (%with-quaternion-values (q a b c d)
     (invert-quaternion a b c d))))



(defun convert-quaternion-struct-to-euler-axis-and-angle (q)
  "Given an quaternion struct Q, return an Euler vector"
  (declare (type quaternion q))
  (multiple-value-bind (x y z angle)
      (%with-quaternion-values (q a b c d)
	(convert-quaternion-to-euler-axis-and-angle a b c d))
    (values (make-dblvec3 x y z) angle)))

(defun quaternion-struct-rotate (vec q)
  "Rotate vector VEC by quaternion struct Q, creating a new vector"
  (declare (type dblvec3 vec)
	   (type quaternion q))
  (%build-dblvec3-from-values
    (%with-quaternion-values (q a b c d)
      (%with-dblvec3-values (vec x y z)
	(quaternion-rotate x y z a b c d)))))


(defun compute-quaternion-struct-for-transform (vec1f vec2f vec1t vec2t)
  "Compute the rotation quaternion that both transforms [F]rom .. [T]o 
  VEC1F -> VEC1T  and  VEC2F -> VEC2T
The vectors must be orthonormal.  This works by constructing the third
vector from the cross product, using it to build a transformation matrix,
and extracting the quaternion from the transformation matrix."
  (declare (type dblvec3 vec1f vec2f vec1t vec2t))
  (%build-qs-from-values
    (compute-quaternion-for-transform
     (aref vec1f 0) (aref vec1f 1) (aref vec1f 2)
     (aref vec2f 0) (aref vec2f 1) (aref vec2f 2)
     (aref vec1t 0) (aref vec1t 1) (aref vec1t 2)
     (aref vec2t 0) (aref vec2t 1) (aref vec2t 2))))

(defun compute-quaternion-struct-for-angular-transform
    (lon1f lat1f  lon1t lat1t
     lon2f lat2f  lon2t lat2t)
   "Given spherical coordinates defined by LATitude and LONgitude in
RADIANS, create a quaternion struct that transforms radian quanities
[F]rom [T]o

   (LAT1F, LON1F) -> (LAT1T, LAT2T)
   (LAT2F, LON2F) -> (LAT2T, LAT2T)"
  (declare (type (double-float -1d6 1d6)
		 lat1f lon1f  lat1t lon1t lat2f lon2f  lat2t lon2t))
  (%build-qs-from-values
    (compute-quaternion-for-angular-transform
     lon1f lat1f  lon1t lat1t
     lon2f lat2f  lon2t lat2t)))





  
  
   
   




