

;; steps a kepler orbit forward in time using 
;; semi-anyltical techniques, Adapted from
;; http://staff.science.uva.nl/~ato/kepler/sol_kep/sol_kep.html
;; (GPL licensed)

;; users c library sol_kep_simple which is a lightly modified
;; and stripped version of sol_kep

;; from diagnostic function ORBIT-ENERGY, the energy is very well
;; conserved even for repulsive force MU<0

(defpackage kepler-orbit
  (:use #:cl)
  (:export
   #:advance-orbit
   #:orbit-energy ;; diagnostic - energy is well conserved
   ))

(in-package kepler-orbit)


(eval-when (:load-toplevel)
  (cffi:define-foreign-library lib-sol-kep
    (:darwin (:or "libsolkep.dylib"))
    (:unix (:or "libsolkep.so")))

  (cffi:use-foreign-library lib-sol-kep)
  )

(declaim (inline sol-kep-advance))
 
;; this conses a bit but using sbcl native alien doesn't help
;; 13% of run time is GC at the level of consing seen.  This is probably OK.
(waaf-cffi:sbdefine-alien-routine 
 ("sol_kep_advance" sol-kep-advance) :void
 (mu :double) ;; mass (or GM actually)
 (dt :double) ;; time interval
 (x :double :in-out)  ;; positions and velocities are modified in place
 (y :double :in-out)
 (z :double :in-out)
 (vx :double :in-out)
 (vy :double :in-out)
 (vz :double :in-out))
				   

(defun advance-orbit (mu pv dt)
"Given a zero-mass point orbiting a central mass mu (with G=1)
at coordinates PV=[x,y,z,vx,vy,vz], update PV with its new phase
space coordinates time DT later."
  (declare (type double-float mu dt)
	   (type (simple-array double-float (6)) pv)
	   (optimize speed))
  (multiple-value-bind (dummy x y z vx vy vz)
      (sol-kep-advance 
       mu dt
       (aref pv 0) (aref pv 1) (aref pv 2)
       (aref pv 3) (aref pv 4) (aref pv 5))
    (declare (ignore dummy)
	     (type double-float x y z vx vy vz))
    (setf (aref pv 0) x)
    (setf (aref pv 1) y)
    (setf (aref pv 2) z)
    (setf (aref pv 3) vx)
    (setf (aref pv 4) vy)
    (setf (aref pv 5) vz)
    t))


(defun orbit-energy (mu pv)
  "A diagnostic routine to measure energy conservation"
  (let ((r 0d0) (v2 0d0))
    (loop 
       for i from 0 to 2
       for j from 3 to 5
       do
	 (incf r (expt (aref pv i) 2))
	 (incf v2 (expt (aref pv j) 2))
       finally
	 (setf r (sqrt r)))
    ;;
    (- (* 0.5 v2)
       (/ mu r))))
 