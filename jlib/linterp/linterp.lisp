
;; linear interpolation - double precision

(defpackage #:linterp
  (:use #:common-lisp)
  (:export 
   #:make-linterp
   #:linterp-in-place
   #:linterp  
   #:make-linterp-func    
   #:make-linterp-func-from-xy 
   ;;
   ;; functions associated with linterp struct
   #:make-linterp-struct
   #:linterp-struct-x        #:linterp-struct-y
   #:linterp-struct-x-vec    #:linterp-struct-y-vec  
   #:linterp-struct-x-min    #:linterp-struct-x-max
   ))

(in-package linterp)
	   

;; structure holding a linear interp table
(defstruct linterp-struct
  (x-vec nil :type (or null (simple-array double-float)))
  (y-vec nil :type (or null (simple-array double-float)))
  (x-min nil :type (or null double-float))
  (x-max nil :type (or null double-float))
  ;; scratch variables for passing non-boxing parameters
  (x     0d0 :type double-float)
  (y     0d0 :type double-float)
  ;;
  ;; values to return for x above and below x-min and x-max
  ;;   nil values mean throw an error
  (x-min-val nil :type (or null double-float))
  (x-max-val nil :type (or null double-float)))

;; reverses vectors x,y if necessary so elements in x are in
;; increasing order
(defun force-increasing-order (x-vec y-vec)
  (declare (type (simple-array double-float (*)) x-vec y-vec)
	   (optimize (speed 3) (safety 1)))
  (if (< (aref x-vec 1) (aref x-vec 0))
    (progn
      (let ((n (length x-vec)))
	(dotimes (i (floor n 2))
	  (declare (type fixnum i))
	  (rotatef (aref x-vec i) (aref x-vec (- n i 1)))
	  (rotatef (aref y-vec i) (aref y-vec (- n i 1))))))))


;; return a double float array if not already one; otherwise return
;; copy of this array
(defun make-dbl-array (x-vec &optional (force-copy-anyway nil))
  (let ((type (type-of x-vec)))
    (if (and (eq (first type) 'simple-array)
	     (eq (second type) 'double-float)
	     (not force-copy-anyway))
	x-vec
      (let ((y-vec (make-array (length x-vec) :element-type 'double-float)))
	(dotimes (i (length x-vec))
	  (setf (aref y-vec i) (coerce (aref x-vec i) 'double-float)))
	y-vec))))

(defun fix-input-arrays (x-vec y-vec)
  (if (or (not (arrayp x-vec)) (not (arrayp y-vec))
	  (not (= (length (array-dimensions x-vec)) 1))
	  (not (= (length (array-dimensions y-vec)) 1))
	  (not (= (length x-vec) (length y-vec))))
      (error "Bad vectors x-vec,y-vec given to make-linterp"))
  ;; make copy of array if necessary (either non-double or not in inc order)
  (let ((need-reverse (< (aref x-vec 1) (aref x-vec 0))))
    ;;force copy if we need to reverse
    (setf x-vec (make-dbl-array x-vec need-reverse))
    ;;force copy if we need to reverse
    (setf y-vec (make-dbl-array y-vec need-reverse)) 
    (if need-reverse (force-increasing-order x-vec y-vec)))
  ;; now check if x-vec array really sorted
  (dotimes (i (+ -1 (length x-vec)))
    (if (<= (aref x-vec (+ i 1)) (aref x-vec i))
	(error (format nil
    "Badly ordered x-vec array given to make-linterp: x[~D]=~A and x[~D]=~A"
    i (aref x-vec i) (+ 1 i) (aref x-vec (+ 1 i))))))
  (values x-vec y-vec))



(defun make-linterp (x-vec y-vec &key x-min-val x-max-val)
  "make a linterp structure from x and y"
  (multiple-value-setq (x-vec y-vec) (fix-input-arrays x-vec y-vec))
  ;;
  (make-linterp-struct
   :x-vec x-vec :y-vec y-vec
   :x-min (aref x-vec 0)
   :x-max (aref x-vec (1- (length x-vec)))
   :x-min-val (if x-min-val (coerce x-min-val 'double-float))
   :x-max-val (if x-max-val (coerce x-max-val 'double-float))))


(defun linterp-in-place (s)
  "Perform a linear interpolation of x value in linterp structure S,
leaving result in y value of S - performs no float boxing"
  (declare (type linterp-struct s))

  (locally
   (declare (optimize (speed 3) (safety 0)))
   (let ((klo 0)
	 (khi 0)
	 (k 0)
	 (n 0)
	 (xa (linterp-struct-x-vec s))
	 (ya (linterp-struct-y-vec s))
	 (x  (linterp-struct-x s))
	 (h 0.0d0)
	 (xmin (linterp-struct-x-min s))
	 (xmax (linterp-struct-x-max s)))
     (declare (fixnum klo khi k n)
	      (double-float x h xmin xmax)
	      (type (simple-array double-float) xa ya))
     (when (< x xmin)
       (if (linterp-struct-x-min-val s)
	   (progn
	     (setf (linterp-struct-y s) (linterp-struct-x-min-val s))
	     (return-from linterp-in-place s))
	   (locally (declare (optimize (safety 3) (speed 1)))
	     ;; we set h to x, and call the error with x.
	     ;; this is CHEEZY but boxing occurs otherwise  - a compiler glitch
	     (setf h x) 
	     (error  "Out of range in linterp x=~A < ~A" h xmin))))
     (when (> x xmax)
       (if (linterp-struct-x-max-val s)
	   (progn
	     (setf (linterp-struct-y s) (linterp-struct-x-max-val s))
	     (return-from linterp-in-place s))
	   (locally (declare (optimize (safety 3) (speed 1)))
	     (setf h x)
	     (error  "Out of range in linterp x=~A > ~A" h xmax))))
     (setf n (length xa))
     (setf klo 0)
     (setf khi (- n 1))
     (do ()
	 ((<= (- khi klo) 1) T)
       (setf k (truncate (+ klo khi) 2))
       (if (> (aref xa k) x) (setf khi k) (setf klo k)))
     (setf h (- (aref xa khi) (aref xa klo)))
     (if (= h 0.0d0) (error "Bad x vector in linterp"))
     (let ((x0 (aref xa klo))
	   (y0 (aref ya klo))
	   (y1 (aref ya khi)))
       (declare (type double-float x0 y0 y1))
       (setf (linterp-struct-y s)
	     (+ y0 (* (- y1 y0) (/ (- x x0) h)))))
     s)))


#+nil
(declaim (#+sbcl sb-ext:maybe-inline 
  	   #+cmu ext:maybe-inline 
	   #-(or sbcl cmu) inline
	   linterp))

(declaim (inline linterp))


(defun linterp (x s)
  "Linearly interpolate, given a double float value X and a linter struct S - this
is inlined, or maybe-inlined for SBCL and CMUCL"
  (declare (type double-float x)
	   (type linterp-struct s))
  (setf (linterp-struct-x s) x)
  (linterp-in-place s)
  (linterp-struct-y s))



;; create a one argument linterp function of x from a linterp-struct
(defun make-linterp-func (s)
  (declare (type linterp-struct s)
	   (inline linterp))
  (flet ((linterp-func (x)
		       (declare (type real x))
		       (linterp (float x 1d0) s)))
    #'linterp-func))

;; create a one argument linterp function of x from x y vectors
(defun make-linterp-func-from-xy (x-vec y-vec &key x-min-val x-max-val)
  (make-linterp-func (make-linterp x-vec y-vec 
				   :x-min-val x-min-val 
				   :x-max-val x-max-val)))





