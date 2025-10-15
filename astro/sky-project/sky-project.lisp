(defpackage "SKY-PROJECT"
  (:use "COMMON-LISP")
  (:export
   #:sin-project
   #:tan-project
   #:sin-de-project
   #:tan-de-project
   #:sin-proj-find-center
   #:tan-proj-find-center
   #:rect-to-sph
   #:sph-to-rect))

(in-package sky-project)

#+(or cmu sbcl)
(declaim (#+cmu ext:maybe-inline #+sbcl sb-ext:maybe-inline 
	   sin-project tan-project 
	   sin-de-project tan-de-project
	   sin-proj-find-center tan-proj-find-center
	   rect-to-sph sph-to-rect))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; routines to handle sin/tan (de)projections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro cos-deg (deg)   `(the double-float
				(cos (* (the (double-float -1d10 1d10) ,deg )
					0.0174532925199433d0))))
(defmacro sin-deg (deg)   `(the double-float
				(sin (* (the (double-float -1d10 1d10) ,deg )
					0.0174532925199433d0))))
(declaim (inline f-modulo))
(defun f-modulo (x q)
  (declare (type (real -1d10 1d10) x)
	   (type (real 1d0 10000d0) q)
	   (inline floor))
  (- x (* (floor (/ x q)) q)))

 
(defmacro get-unit-conversion-factor (units)
  `(the double-float
	(cond ((eq ,units :natural) 1d0)
	      ((eq ,units :degrees) #.(/ 180 pi))
	      ((eq ,units :arcmin) #.(/ (* 60 180) pi))
	      ((eq ,units :arcsec) #.(/ (* 3600 180) pi))
	      (t (error "unknown units ~A" units)))))
  

;; projects a (ra type angle) and d (dec type angle) around a-center and
;; d-center, in a sin-projection, in natural (unit circle) units; all input
;; angles are in decimal degrees. Returns a vector of the form
;; #(x y)  where x is in the direction of increasing a, and y in the direction
;; of increasing d
(defun sin-project (a d a-center d-center &key (units :natural))
  "projects a (ra type angle) and d (dec type angle) around a-center and
d-center, in a sin-projection; all input angles are in decimal degrees.
Returns (values x y) where x is in the direction of increasing a, and y in
the direction of increasing d. UNITS can be :natural (unit circle),
:degrees, :arcmin, or :arcsec"
  (let ((sa (sin-deg a))         (ca (cos-deg a))
        (sd (sin-deg d))         (cd (cos-deg d))        
        (sac (sin-deg a-center)) (cac (cos-deg a-center))
        (sdc (sin-deg d-center)) (cdc (cos-deg d-center))
	(fac (get-unit-conversion-factor units)))
    (values
     (* fac (+ (* -1.0 sac cd ca) (* cac cd sa)))
     (* fac (+ (* -1.0 sdc cac cd ca) (* -1.0 sdc sac cd sa) (* cdc sd))))))
 
;; same as above, but using tan-project
(defun tan-project (a d a-center d-center &key (units :natural))
    "projects a (ra type angle) and d (dec type angle) around a-center and
d-center, in a tan-projection; all input angles are in decimal degrees.
Returns (values x y) where x is in the direction of increasing a, and y in
the direction of increasing d. UNITS can be :natural (unit circle),
:degrees, :arcmin, or :arcsec"
    (declare (inline sin-project))
    (multiple-value-bind 
     (x y)
     (sin-project a d a-center d-center) ;; use natural units (default)
     (let* ((adj (sqrt (the (double-float 0d0) (- 1.0 (* x x) (* y y)))))
	    (fac (get-unit-conversion-factor units)))
       (values (* fac (/ x adj))
	       (* fac (/ y adj))))))

 
 
;; convert x y z coordinates to ra/dec type angular; return (values a d)
;; x,y,z need not be normalized
(defun rect-to-sph (x y z)
  (declare (type double-float x y z))
  (let* ((r (max 1d0 (sqrt (+ (* x x) (* y y) (* z z))))))
    (values
     (the (double-float 0d0 360d0)
       (f-modulo (* 57.2957795130823d0 (atan y x)) 360.0d0))
     (the (double-float -90d0 90d0)
       (if (zerop r) ;; undefined
	   0d0
	 (* 57.2957795130823d0 
	    (asin (the (double-float -1d0 1d0) (/ z r)))))))))



(defun sph-to-rect (a d)
  (declare (type double-float a d))
  (let ((sa (sin-deg a)) (ca (cos-deg a))
        (sd (sin-deg d)) (cd (cos-deg d)))
    (values (* ca cd)  (* sa cd) sd)))
    
 
;; de-project a tan projection x,y about a-center,d-center;
;; returns (values a d)
(defun tan-de-project (x y a-center d-center &key (units :natural))
  (declare (inline rect-to-sph)
	   (type real x y a-center d-center))
  (let* ((fac (get-unit-conversion-factor units))
	 (x (float (/ x fac) 1d0))
	 (y (float (/ y fac) 1d0))
	 (sac (sin-deg a-center)) (cac (cos-deg a-center))
	 (sdc (sin-deg d-center)) (cdc (cos-deg d-center)))
    (declare (type double-float fac x y sac sdc))
    (rect-to-sph
     (+ (* cdc cac) (* x -1.0d0 sac) (* y -1.0d0 sdc cac))
     (+ (* cdc sac) (* x cac)      (* y -1.0d0 sdc sac))
     (+ sdc (* y cdc)))))
 
;; ditto for a tan projection 
(defun sin-de-project (x y a-center d-center &key (units :natural))
  (declare (inline rect-to-sph))
  (let* ((fac (get-unit-conversion-factor units))
	 (x (float (/ x fac) 1d0))
	 (y (float  (/ y fac) 1d0))
	 (sac (sin-deg a-center)) (cac (cos-deg a-center))
	 (sdc (sin-deg d-center)) (cdc (cos-deg d-center))
	 (z (sqrt (- 1.0d0 (+ (* x x) (* y y))))) )
    (declare (type double-float fac x y sac sdc z))
    (rect-to-sph
     (+ (* z cdc cac) (* x -1.0d0 sac) (* y -1.0d0 sdc cac))
     (+ (* z cdc sac) (* x cac)      (* y -1.0d0 sdc sac))
     (+ (* z sdc) (* y cdc))))) 
 

;; routine for finding the center ra and dec of a sin projection,
;; given that x,y is at axy, dxy
;;  returns (values a-center d-center)
;;  there are really 2 solutions, on opposite sides of the sphere; we compute
;; both and return the right one


(defun sin-proj-find-center (x y axy dxy)
  (declare (inline sph-to-rect))
  (multiple-value-bind (rx ry rz)
      (sph-to-rect axy dxy)
    (let* ((z (sqrt (- 1 (* x x) (* y y)))) ;; assume z>0; then only proj from front is valid
	   (a 0.0d0) 
	   (d 0.0d0) 
	   (psi 0.0d0) ;; angle between (ry,-rx) and (cos(-a+pi/2),sin(-a+pi/2))
	   (tmp 0.0d0))
      ;; test for invalid conditions
      ;;    if rx=ry=0, then x must be zero (can't be in ra direction on pole)
      (if (and (= rx 0.0d0)  (= ry 0.0d0) (not (= x 0.0d0)))
	  (error "sin-proj-find-center: point on pole, but x not 0"))
      (if (and (= rx 0.0d0) (= ry 0.0d0)) ;; if at poles, "a" angle indeterminate
	  (progn (setf a 0.0d0))
        (progn ;; else compute the two possible values of a
          ;;acos [x/length(rx,ry)]
	  (setf tmp (acos (/ x (sqrt (+ (* rx rx) (* ry ry))))))
	  (setf psi (atan  (- rx) ry))
         (setf a  (+ psi tmp))))
      ;; internal function to compute d for a particular a
      (flet ((compute-d (a) 
                 (let* ((p  rz)                                ;; |x|   |-q  p| |sin(d)|
			(q (+ (* rx (cos a))  (* ry (sin a)))) ;; |y| = |p   q| |cos(d)|
			(det (- (* -1.0d0 q q) (* p p)))
			(sin-d (/ (+ (* q y) (* -1.0d0 p z))  det))
			(cos-d (/ (+ (* -1.0d0 p y) (* -1.0d0 q z))  det)))
		   (atan sin-d cos-d))))
      ;;
	(setf d (compute-d a)))
      ;; now fix a,d if d is outside [90,-90]
    (cond ((< d -3.14159265358979d0)
           (setf d (- -3.14159265358979d0 d))
           (setf a (+ a 3.14159265358979d0)))
         ((> d 3.14159265358979)
           (setf d (- 3.14159265358979d0 d))
           (setf a (+ a 3.14159265358979d0)))) 

    (values (f-modulo (* 57.295779513082d0 a) 360.0d0)
            (* 57.295779513082d0 d)))))

;; same for tan projection, simply converting the tan proj to a sin proj
(defun tan-proj-find-center (x y axy dxy)
  (let ((adj (sqrt (+ 1.0d0  (* x x) (* y y)))))
    (sin-proj-find-center (/ x adj) (/ y adj) axy dxy)))
         


(provide 'sky-project)
