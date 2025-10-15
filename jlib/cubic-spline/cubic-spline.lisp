
;; spline a table

(defpackage #:cubic-spline
  (:use #:common-lisp)
  (:export 
   #:spline
   #:make-spline
   #:splint-in-place #:splint  #:splint-deriv-in-place #:splint-deriv
   #:make-splint-func    #:make-splint-func-deriv
   #:make-splint-func-from-xy    #:make-splint-func-deriv-from-xy 
   
   ;;
   ;; functions associated with spline struct
   #:make-spline-struct
   #:spline-struct-x-vec    #:spline-struct-y-vec
   #:spline-struct-x        #:spline-struct-y
   #:spline-struct-ddy
   #:spline-struct-d-start  #:spline-struct-d-end
   #:spline-struct-x-min    #:spline-struct-x-max
   ))

(in-package cubic-spline)
	   

;; structure holding a spline
(defstruct spline-struct
  (x-vec nil :type (or null (simple-array double-float (*))))
  (y-vec nil :type (or null (simple-array double-float (*))))
  ;; scratch variables used for passing non-boxed arguments
  (x 0d0 :type double-float)
  (y 0d0 :type double-float)
  ;;
  (ddy nil)
  (d-start nil)
  (d-end nil)
  (x-min nil)
  (x-max nil))

;; reverses vectors x,y if necessary so elements in x are in
;; increasing order
(defun force-increasing-order (x y)
  (if (< (aref x 1) (aref x 0))
    (progn
      (let ((n (length x)))
	(dotimes (i (floor n 2))
	  (rotatef (aref x i) (aref x (- n i 1)))
	  (rotatef (aref y i) (aref y (- n i 1))))))))


;; return a double float array if not already one; otherwise return
;; copy of this array
(defun make-dbl-array (x &optional (force-copy-anyway nil))
  (let ((type (type-of x)))
    (if (and (eq (first type) 'simple-array)
	     (eq (second type) 'double-float)
	     (not force-copy-anyway))
	x
      (let ((y (make-array (length x) :element-type 'double-float)))
	(dotimes (i (length x))
	  (setf (aref y i) (coerce (aref x i) 'double-float)))
	y))))

(defun fix-input-spline-arrays (x y)
  (if (or (not (arrayp x)) (not (arrayp y))
	  (not (= (length (array-dimensions x)) 1))
	  (not (= (length (array-dimensions y)) 1))
	  (not (= (length x) (length y))))
      (error "Bad vectors x,y given to make-spline"))
  ;; make copy of arrray if necessary (either non-double or not in inc order)
  (let ((need-reverse (< (aref x 1) (aref x 0)))) 
    (setf x (make-dbl-array x need-reverse)) ;;force copy if we need to reverse
    (setf y (make-dbl-array y need-reverse)) ;;force copy if we need to reverse
    (if need-reverse (force-increasing-order x y)))
  ;; now check if x array really sorted
  (dotimes (i (+ -1 (length x)))
    (if (<= (aref x (+ i 1)) (aref x i))
	(error "Badly ordered x array given to make-spline")))
  (values x y))



(defun make-spline (x-vec y-vec &optional (d-start nil) (d-end nil))
  "make a spline structure from x-vec and y-vec; d-start and d-end are 
derivatives at endpoints, or 'natural' spline if NIL"
  (multiple-value-setq (x-vec y-vec) (fix-input-spline-arrays x-vec y-vec))
  ;;
  (let* ((n (length x-vec))
	 (nm1 (- n 1))
	 (spline 
	  (make-spline-struct
	   :x-vec x-vec :y-vec y-vec
	   :ddy (make-array n :element-type 'double-float)
	   :d-start d-start 
	   :d-end d-end))
	 (u (make-array (+ -1 n) :element-type 'double-float))
	 (y2 (spline-struct-ddy spline))
	 (p 0.0d0) (qn 0.0d0) (sig 0.0d0) (un 0.0d0) )
    (declare (double-float p qn sig un))
    (setf (spline-struct-x-min spline) (aref x-vec 0))
    (setf (spline-struct-x-max spline) (aref x-vec nm1))
    ;;
    (if (not d-start)
	(progn
	  (setf (aref y2 0) 0.0d0)
	  (setf (aref u 0) 0.0d0))
      (progn
	(setf (aref y2 0) -0.5d0)
	(setf (aref u 0)
	      (* (/ 3.0 (- (aref x-vec 1) (aref x-vec 0)))
		 (- (/ (- (aref y-vec 1) (aref y-vec 0))
		       (- (aref x-vec 1) (aref x-vec 0))) d-start)))))
    
    ;;
    (loop for i from 1 to (+ -2 n) do
	  (setf sig (/ (- (aref x-vec i) (aref x-vec (- i 1)))
		       (- (aref x-vec (+ i 1)) (aref x-vec (- i 1)))))
	  (setf p (+ (* sig (aref y2 (- i 1))) 2.0))
	  (setf (aref y2 i) (/ (- sig 1.0d0) p))
	  (setf (aref u i) (- (/ (- (aref y-vec (+ i 1)) (aref y-vec i))
				 (- (aref x-vec (+ i 1)) (aref x-vec i)))
			      (/ (- (aref y-vec i) (aref y-vec (- i 1)))
				 (- (aref x-vec i) (aref x-vec (- i 1))))))
	  (setf (aref u i) (/
			    (- (/ (* 6.0 (aref u i))
				  (- (aref x-vec (+ i 1))
				     (aref x-vec (- i 1))))
			       (* sig (aref u (- i 1))))
			    p)))
    ;;
    (if (not d-end)
	(progn
	  (setf qn 0.0d0)
	  (setf un 0.0d0))
      (progn
	(setf qn 0.5d0)
	(setf un (* (/ 3.0 (- (aref x-vec nm1) (aref x-vec (- nm1 1))))
		    (- d-end (/ (- (aref y-vec nm1) (aref y-vec (- nm1 1)))
			      (- (aref x-vec nm1) (aref x-vec (- nm1 1)))))))))
    ;;
    (setf (aref y2 (+ -1 n))
	  (/ (- un (* qn (aref u (- n 2))))
	     (+ (* qn (aref y2 (- n 2))) 1.0)))
    (loop for k from (- n 2) downto 0 do
	  (setf (aref y2 k) (+ (* (aref y2 k) (aref y2 (+ k 1))) (aref u k))))
    spline))
			     

(defun splint-in-place (s)
  (declare (type spline-struct s)
	   (optimize (speed 3) (safety 1)))
  (let ((klo 0)
	(khi 0)
	(k 0)
	(h 0.0d0)
	(b 0.0d0)
	(a 1.0d0)
	(n 0)
	(x  (spline-struct-x s)) 
	(xa (spline-struct-x-vec s))
	(ya (spline-struct-y-vec s))
	(xmin (spline-struct-x-min s))
	(xmax (spline-struct-x-max s))
	(y2a (spline-struct-ddy s)))
    (declare (fixnum klo khi k n)
	     (double-float x h b a xmin xmax)
	     (type (simple-array double-float) xa ya y2a))
    (if (or (> x xmax) (< x xmin))
	(error "Out of range x in splint"))
    (setf n (length xa))
    (setf klo 0)
    (setf khi (- n 1))
    (do ()
	((<= (- khi klo) 1) T)
      (setf k (truncate (+ klo khi) 2))
      (if (> (aref xa k) x) (setf khi k) (setf klo k)))
    (setf h (- (aref xa khi) (aref xa klo)))
    (if (= h 0.0d0) (error "Bad x vector in splint"))
    (setf a (/ (- (aref xa khi) x) h))
    (setf b (/ (- x (aref xa klo)) h))
    ;;
    (setf (spline-struct-y s)
	  (+ (* a (aref ya klo))
	     (* b (aref ya khi))
	     (/
	      (*
	       (+ (* (- (* a a a) a) (aref y2a klo))
		  (* (- (* b b b) b) (aref y2a khi)))
	       (* h h))
	      6.0d0)))
    s))


(declaim (inline splint))

(defun splint (x s)
  (declare (type spline-struct s)
	   (type double-float x))
  (setf (spline-struct-x s) x)
  (splint-in-place s)
  (spline-struct-y s))


;; like splint, but gets the derivative of the spline
(defun splint-deriv-in-place (s)
  (declare (type spline-struct s)
	   (optimize (speed 3) (safety 1)))
  (let ((x (spline-struct-x s))
	(klo 0)
	(khi 0)
	(k 0)
	(h 0.0d0)
	(b 0.0d0)
	(a 1.0d0)
	(n 0)
	(xa (spline-struct-x-vec s))
	(ya (spline-struct-y-vec s))
	(xmin (spline-struct-x-min s))
	(xmax (spline-struct-x-max s))
	(y2a (spline-struct-ddy s)))
    (declare (fixnum klo khi k n)
	     (double-float h b a x xmin xmax)
	     (type (simple-array double-float) xa ya y2a))
    (if (or (> x xmax) (< x xmin))
	(error "Out of range x in splint"))
    (setf n (length xa))
    (setf klo 0)
    (setf khi (- n 1))
    (do ()
	((<= (- khi klo) 1) T)
      (setf k (truncate (+ klo khi) 2))
      (if (> (aref xa k) x) (setf khi k) (setf klo k)))
    (setf h (- (aref xa khi) (aref xa klo)))
    (if (= h 0.0d0) (error "Bad x vector in splint"))
    (setf a (/ (- (aref xa khi) x) h))
    (setf b (/ (- x (aref xa klo)) h))
    ;;
    (setf (spline-struct-y s)
	  (+ (/ (aref ya klo) (- h))
	     (/ (aref ya khi) h)
	     (/
	      (*
	       (+ (* (- 1.0d0 (* 3 a a)) (aref y2a klo))
		  (* (- (* 3 b b) 1.0d0) (aref y2a khi)))
	       h)
	      6.0d0)))
    s))


(defun splint-deriv (x s)
  (declare (type spline-struct s)
	   (type double-float x))
  (setf (spline-struct-x s) x)
  (splint-deriv-in-place s)
  (spline-struct-y s))

;; create a one argument splint function of x from a splint-struct
(defun make-splint-func (s)
  "create a one argument function from a spline-struct"
  (declare (type spline-struct s)
	   (inline splint))
  (flet ((splint-func (x)
		      (splint (float x 1d0) s)))
    #'splint-func))

;; create a one argument splint function of x from x y vectors
(defun make-splint-func-from-xy (x-vec y-vec
				 &optional (d-start nil) (d-end nil))
  "create a one argument spline function of x from x-vec y-vec"
  (make-splint-func (make-spline x-vec y-vec d-start d-end)))


;; create a one argument splint function of the derivative 
;; from a splint-struct
(defun make-splint-func-deriv (s)
  "create a one argument spline function of the derivative
of the function in spline-struct" 
  (declare (type spline-struct s)
	   (inline splint-deriv))
  (flet ((splint-func (x)
		      (setf x (coerce x 'double-float))
		      (splint-deriv x s)))
    #'splint-func))

;; create a one argument splint function of the derivative
;; from x y vectors
(defun make-splint-func-deriv-from-xy (x-vec y-vec
				       &optional (d-start nil) (d-end nil))
  "create a one argument spline function of the derivative of the spline
made from x-vec y-vec"
  (make-splint-func-deriv (make-spline x-vec y-vec d-start d-end)))

 






