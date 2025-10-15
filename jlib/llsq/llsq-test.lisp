
(defpackage llsq-test
  (:use #:cl #:llsq)
  (:export
   #:test-lfit-funcs
   #:test-lfit-vecs
   #:simple-regression-test))

(in-package llsq-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test-lfit-funcs ()
  "fit 1+2x+3x^2+4x^3+5x^4 and recover coefficients  1,2,3,4,5"
  (let* ((n 100) ;; 100 data points
	 (xvec (make-array n :element-type 'double-float ))
	 (yvec (make-array n :element-type 'double-float ))
	 (sigvec (make-array n :element-type 'double-float :initial-element 1d0))
	 (funcs
	  (list
	   (lambda (x) (declare (ignore x)) 1d0)
	   (lambda (x) x)
	   (lambda (x) (* x x))
	   (lambda (x) (* x x x))
	   (lambda (x) (* x x x x)))))
    (loop for i below 100
	  for x = (float i 1d0)
	  do
	  (setf (aref xvec i) x)
	  (setf (aref yvec i)
		(+ 1d0 (* 2d0 x) (* 3d0 x x) (* 4d0 x x x) (* 5d0 x x x x))))
    (lfit-funcs xvec yvec sigvec funcs)))


(defun test-lfit-vecs ()
  "fit 1+2x+3x^2+4x^3+5x^4 and recover coefficients 1,2,3,4,5"
  (let* ((n 100)  ;; 100 data points
	 (xb (make-array (list 5 n) :element-type 'double-float ))
	 (yvec (make-array n :element-type 'double-float ))
	 (sigvec (make-array n :element-type 'double-float :initial-element 1d0)))

    (loop for i below 100
	  for x = (float i 1d0)
	  do
	  (setf (aref yvec i)
		(+ 1d0 (* 2d0 x) (* 3d0 x x) (* 4d0 x x x) (* 5d0 x x x x)))
	  (setf (aref xb 0 i) 1d0
		(aref xb 1 i) x
		(aref xb 2 i) (* x x)
		(aref xb 3 i) (* x x x)
		(aref xb 4 i) (* x x x x)))
    ;;
    (lfit-values  yvec sigvec xb)))

(defun simple-regression-test (&key (type :linear) (n 100) (errors 0d0)
				 (c2 1d0) (c1 2d0) (c0 3d0) (x0 -10d0) (dx 1.0))
  "A test routine for the linear and quadratic simple regressions."
  (let ((xv (make-array n :element-type 'double-float))
	(yv (make-array n :element-type 'double-float))
	(sv (make-array n :element-type 'double-float)))
    (loop with c2 = (if (eq type :linear) 0d0 c2)
	  for i below n
	  for x = x0 then (+ x dx)
	  for y = (+ c0 (* c1 x) (* c2 x x))
	  for s = (* errors (random:gaussian))
	  do (setf (aref xv i) x
		   (aref yv i) (+ y s)
		   (aref sv i) s))
    (let ((s1 0d0)  (sy 0d0) (sx 0d0) (sx2 0d0) (sx3 0d0) (sx4 0d0) (sxy 0d0) (sx2y 0d0))
      ;; compute coefficients
      (loop for x across xv
	    for y across yv
	    for s across sv
	    for s2 = (if (zerop errors) 1d0  (* errors errors))
	    do
	       (incf s1  (/ 1d0 s2))
	       (incf sx  (/ x s2))
	       (incf sy  (/ y s2))
	       (incf sx2 (/ (* x x) s2))
	       (incf sx3 (/ (* x x x) s2))
	       (incf sx4 (/ (* x x x x) s2))
	       (incf sxy (/ (* x y) s2))
	       (incf sx2y (/ (* x x y) s2)))
      (if (eq type :linear)
	  (linear-regression s1 sx sy sx2 sxy)
	  (quadratic-regression s1 sx sy sx2 sx3 sx4 sxy sx2y)))))

