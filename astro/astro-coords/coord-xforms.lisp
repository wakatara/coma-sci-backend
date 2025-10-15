


(in-package astro-coords)

;; IN PROGRESS - these are the more up date methods for coord transforms
;; using quaternions.




;; rotational quaternion using degrees
(eval-when (:compile-toplevel :load-toplevel)
  (defun %build-rotation-quaternion (lon1a lat1a  lon1b lat1b
				     lon2a lat2a  lon2b lat2b)
    (quaternion:compute-quaternion-struct-for-angular-transform
     (deg2rad lon1a) (deg2rad lat1a)
     (deg2rad lon1b) (deg2rad lat1b)
     (deg2rad lon2a) (deg2rad lat2a)
     (deg2rad lon2b) (deg2rad lat2b))))


;; defconstant that sets only once
(defmacro defquaternion (varname quaternion)
  `(alexandria:define-constant ,varname ,quaternion :test 'equalp))

 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define quaternions for important coordinate systems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
;; Galactic II
;;;;;;;;;;;;;;;;
(defquaternion +quaternion-J2000-to-galactic-ii+
    (%build-rotation-quaternion
     ;; north galactic pole to 0,90 - the definition doesn't seem exact
     (ra-dec:hms->deg 12 51 24)   +27.13d0     00d0 90d0
     ;; galactic center to 0,0
     (ra-dec:hms->deg 17 45 36)   -28.92d0     00d0 0d0))

(defquaternion +quaternion-galactic-ii-to-J2000+
    (quaternion:invert-quaternion-struct
     +quaternion-J2000-to-galactic-ii+))

;;;;;;;;;;;;;;;;
;; Ecliptic
;;;;;;;;;;;;;;;;
(defquaternion +quaternion-equatorial-J2000-to-ecliptic-J2000+
     (%build-rotation-quaternion
      0d0 0d0       0d0 0d0 	  ;; 0,0 maps to 0,0
      90d0 0d0      90d0 -23.43929111d0))


(defquaternion +quaternion-ecliptic-J2000-to-equatorial-J2000+
    (quaternion:invert-quaternion-struct
     +quaternion-equatorial-J2000-to-ecliptic-J2000+))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun xform-lat-lon-to-lat-lon (quaternion longitude latitude
				 &key (lon-type :one-sided))
  "Transform a LAT,LON pair to another LAT,LON pair."
  (declare (type quaternion:quaternion quaternion))
  (quaternion:rotate-lon-lat-using-quaternion
   quaternion longitude latitude))


(defun xform-xyz-to-xyz (quaternion x y z)
  "Transform a X,Y,Z coordinate set to another X,Y,Z coordinate set"
  (declare (type quaternion:quaternion quaternion)
	   (type double-float x y z)) 
  (quaternion:quaternion-rotate 
   x y z  
   (quaternion:quaternion-r quaternion)
   (quaternion:quaternion-i quaternion)
   (quaternion:quaternion-j quaternion)
   (quaternion:quaternion-k quaternion)))

(defun xform-vector-to-vector (quaternion 3vec)
  "Transform a #(X Y Z) vector to another #(X Y Z) vector."
  (declare (type quaternion:quaternion quaternion)
	   (type (simple-array double-float (3))))
  (quaternion:quaternion-struct-rotate quaternion 3vec))



(defun xform-lat-lon-to-xyz (quaternion longitude latitude
			     &key (r 1d0) (output :values))
  "Transform a LAT,LON pair and optionally a radius R to x,y,z.  
If :OUTPUT is :VECTOR, output a double float vector #(x y z), else if 
   :OUTPUT is :VALUES, output (VALUES X Y Z)."
  (declare (type quaternion:quaternion quaternion)
	   (type double-float longitude latitude r)
	   (type (member :vector :values) output))
  (multiple-value-bind (x y z)
      (quaternion:lon-lat->xyz  longitude latitude r)
    (multiple-value-bind (xx yy zz)
	(quaternion:quaternion-rotate 
	 x y z  
	 (quaternion:quaternion-r quaternion)
	 (quaternion:quaternion-i quaternion)
	 (quaternion:quaternion-j quaternion)
	 (quaternion:quaternion-k quaternion))
      (if (eq output :values)
	  (values xx yy zz)
	  (three-vector:make-3vec xx yy zz)))))






;; define some quaternions using angle-from to angle-to
;; this site is useful for finding 2 points to make the transform
#+nil
(let* (;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; equatorial to galactic
       (q-equatorial-J2000-to-galactic-ii
	 (%build-rotation-quaternion
	  ;; north galactic pole to 0,90 - the definition doesn't seem exact
	  (ra-dec:hms->deg 12 51 24)   +27.13d0     00d0 90d0
	  ;; galactic center to 0,0
	  (ra-dec:hms->deg 17 45 36)   -28.92d0     00d0 0d0))
       ;; inverse transform
       (q-galactic-ii-to-equatorial-J2000
	 (quaternion:invert-quaternion-struct q-equatorial-J2000-to-galactic-ii))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; ecliptic coordinates - https://ned.ipac.caltech.edu/forms/calculator.html
       (q-equatorial-J2000-to-ecliptic-J2000 
	 (%build-rotation-quaternion
	  0d0 0d0       0d0 0d0 	  ;; 0,0 maps to 0,0
	  90d0 0d0      90d0 -23.43929111d0))
       (q-equatorial-ecliptic-J2000-to-J2000 
	 (quaternion:invert-quaternion-struct q-equatorial-J2000-to-ecliptic-J2000 ))
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       )

  ;; these match the output of old routines but are probably a bit better
  (defun xform-equatorial-J2000->galactic-ii    (ra dec)
    (quaternion:rotate-lon-lat-using-quaternion
     q-equatorial-J2000-to-galactic-ii ra dec))
  (defun xform-galactic-ii->equatorial-J2000  (l-ii b-ii)
    (quaternion:rotate-lon-lat-using-quaternion
     q-galactic-ii-to-equatorial-J2000 l-ii b-ii))
  ;;
  (defun xform-equatorial-J2000->ecliptic-j2000 (ra dec)
    (quaternion:rotate-lon-lat-using-quaternion
     q-equatorial-J2000-to-ecliptic-J2000 ra dec))
  (defun xform-ecliptic-J2000->equatorial-j2000 (lat-ecl lon-ecl)
    (quaternion:rotate-lon-lat-using-quaternion
     q-equatorial-ecliptic-J2000-to-J2000 lat-ecl lon-ecl))
    
  

  )
  
    
