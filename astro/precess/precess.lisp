

(defpackage :precess
  (:use #:common-lisp)
  (:export
   #:precess-decimal-deg-radec
   #:B1950->J2000
   #:J2000->B1950))

(in-package precess)

;; generate cos,sin that comile without range checks
(defmacro cos-nice (theta)  `(cos (the (double-float -1d19 1d19) ,theta)))
(defmacro sin-nice (theta)  `(sin (the (double-float -1d19 1d19) ,theta)))
(defmacro asin-nice (x)
  `(asin (the (double-float -1d0 1d0) (max -1d0 (min 1d0 ,x)))))
(defmacro acos-nice (x)
  `(acos (the (double-float -1d0 1d0) (max -1d0 (min 1d0 ,x)))))
  


(defun precess-decimal-deg-radec (ra0 dec0 jd0 jd1)
  (declare (type double-float ra0 dec0 jd0 jd1)
	   (optimize (speed 3) (safety 1)))
  (setf ra0  (* ra0  0.0174532925199433d0)) ;; deg->radians
  (setf dec0 (* dec0 0.0174532925199433d0))
  (let* ((s (/ (- jd0 2451545.0d0) 36525.0d0))
	 (x (/ (- jd1 jd0) 36525.0d0))
	 (ss  (* s s))
	 (xx  (* x x))
	 (xxx (* x xx))
	 (zeta (* 4.84813681109536d-6  ;; arcsec to rad conversion
		  (+ 
		   (* x  (+ 2306.2181d0 (* s 1.39656d0)   (* ss -0.000139d0)))
		   (* xx (+ 0.30188d0   (* s -0.000344d0)))
		   (* xxx 0.017998d0))))
	 (z     (* 4.84813681109536d-6
		   (+ 
		    (* x   (+ 2306.2181d0 (* s 1.39656d0)   (* ss -0.000139d0)))
		    (* xx  (+ 1.09468d0   (* s 0.000066d0)))
		    (* xxx 0.018203d0))))
	 (theta (* 4.84813681109536d-6
		   (+ 
		    (* x   (+ 2004.3109d0 (* s -0.85330d0)   (* ss -0.000217d0)))
		    (* xx  -1.0d0 (+ 0.42665d0   (* s 0.000217d0)))
		    (* xxx -0.041833d0))))

	 (dec1 0.0d0) (ra1 0.0d0) 
	 (c-theta (cos-nice theta)) (s-theta (sin-nice theta)) 
	 (c-raz (cos-nice (+ ra0 zeta)))
	 (s-raz (sin-nice (+ ra0 zeta)))
	 (c-dec (cos-nice dec0)) (s-dec (sin-nice dec0))
	 (a (* c-dec s-raz))
	 (b (- (* c-theta c-dec c-raz) (* s-theta s-dec)))
	 (c (+ (* s-theta c-dec c-raz) (* c-theta s-dec))) )
    (declare (type double-float x x ss xx xxx zeta z theta dec1 ra1 c-theta
		   c-raz s-raz c-dec a b c))	
	 (setf ra1 (* 57.2957795130823d0 (+ z (atan A B))))
	 (if (< ra1 0) (setf ra1 (+ ra1 360.0d0)))
	 (if (or (< dec0 -85d0) (> dec0 85d0))
	     (setf dec1 (* 57.2957795130823d0
			   (the double-float
			     (acos-nice
			      (sqrt (the (double-float 0d0)
				      (+ (* a a) (* b b))))))))
	     (setf dec1 (* 57.2957795130823d0
			   (the double-float (asin-nice c)))))
	 (values ra1 dec1)))

;;; precess ra and dec, returning the types (decimal deg, split, or string)
;;; of the original arguments
;(define (precess:JDa->JDb ra dec JDa JDb)
;  (let* ((v (radec-utils:get-types ra dec))
;	 (vv (precess-radec (vector-ref v 0) (vector-ref v 1) JDa JDb)))
;    (vector-set! v 0 (vector-ref vv 0))
;    (vector-set! v 1 (vector-ref vv 1))
;    (radec-utils:restore-types v)))


(defun B1950->J2000 (ra dec)
  (precess-decimal-deg-radec (coerce ra 'double-float)
			     (coerce dec 'double-float)
			     2433282.4235d0  2451545.0000d0))

(defun J2000->B1950 (ra dec)
  (precess-decimal-deg-radec (coerce ra 'double-float)
			     (coerce dec 'double-float)
			     2451545.0000d0 2433282.4235d0))


;(define (precess:year1->year2 ra dec year1 year2)
;  (define (year->jd year)
;    (+ 2433282.4235 (* (- year 1950) 365.25153)))
;  (precess:JDa->JDb ra dec (year->jd year1) (year->jd year2)))
