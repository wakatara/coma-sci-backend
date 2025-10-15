

#|

utility functions to find an enclosing circle for points on the sky.

|#

(in-package astro-obj)

;; compute the mean ra,dec of a set of points, and the radius
;; return (ra0 dec0 radius) to hold all the points.
(defun find-bounding-radec-circle-for-ra-dec-vecs (ra-vec dec-vec)
  "Return a bounding circle (VALUES RA DEC RADIUS-IN-DEG) on the sky
holding all of the objects in RA-VEC,DEC-VEC."

  (declare (type vector ra-vec dec-vec))
  (when (not (= (length ra-vec) (length dec-vec)))
    (error "RA-VEC and DEC-VEC have unqual lengths."))
  ;;
  (let ((3v0 (make-array 3 :element-type 'double-float :initial-element 0d0))
	(ra0 0d0) (dec0 0d0)
	(radius 0d0))
    ;; compute mean x,y,z
    (loop 
      with 1/n = (/ 1d0 (length ra-vec))
      with 3vec = (three-vector:make-3vec)
      for ra across ra-vec and dec across dec-vec
      for i from 0
      do
	 (three-vector:3vector-from-spherical-coordinates 
	  (* #.(/ pi 180) dec) ;; theta=-pi/2.. pi/2 is DEC
	  (* #.(/ pi 180) ra)  ;; phi=0..2pi is RA 
	  1d0
	  :vdest 3vec)
	 ;;
	 (incf (aref 3v0 0) (* 1/n (aref 3vec 0)))
	 (incf (aref 3v0 1) (* 1/n (aref 3vec 1)))
	 (incf (aref 3v0 2) (* 1/n (aref 3vec 2))))
    ;;
    (multiple-value-bind (theta phi r0) 
	(3vec:spherical-coordinates-from-3vector 3v0)
      (when (< r0 1d-2)
	(error "Object in list seem to have average position in center of sphere - should not happen."))
      (setf dec0 (* #.(/ 180 pi)  theta))
      (setf ra0  (* #.(/ 180 pi)  phi)))
    ;;
    ;; find the maximum radius from the average position
    (loop for ra across ra-vec and dec across dec-vec
	  do (setf radius
		   (max radius
			(astro-coords:sky-angle 
			 ra0 dec0 
			 ra dec
			 :units :degrees))))
    ;; return center position and radius
    (values (mod ra0 360d0)
	    dec0 radius)))
			

(defun find-bounding-radec-circle-for-aobj-list (aobj-list)
  "Return a bounding circle (VALUES RA DEC RADIUS-IN-DEG) on the sky holding all of the
objects in AOBJ-LIST"
  (find-bounding-radec-circle-for-ra-dec-vecs 
   (map '(simple-array double-float (*)) 'obj-alpha aobj-list)
   (map '(simple-array double-float (*)) 'obj-delta aobj-list)))
