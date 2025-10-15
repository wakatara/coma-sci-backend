
(defpackage #:golden-section
  (:use #:common-lisp)
  (:export
   #:findmin
   #:bracketmin))



(in-package golden-section)


(defun findmin (func x0 x1 &key (eps 1.0d-8) (neval-max 1000))
  "Uses golden-section to minimize FUNC between X0 and X1,
to fractional precision EPS, with a maxinum of NEVAL-MAX evaulations.

Returns (VALUES X-OPTIMUM N-EVALUATIONS)"
  (declare (type (function (double-float) double-float)
		 func)
	   (type double-float x0 x1 eps)
	   (type (unsigned-byte 28) neval-max)
	   (optimize speed))
  (if (> x0 x1) ;; reorder x1,x0 so that x1>x0
      (let ((tmp x0)) (setf x0 x1) (setf x1 tmp)))
  (let* ((top x0)
         (bot x1)
         (cent1 (+ bot (* 0.38197d0 (- top bot))))
         (cent2 (+ bot (* 0.61803d0 (- top bot))))
	 (neval 0)
         (f1 (funcall func cent1)) 
         (f2 (funcall func cent2)))
    (declare (type double-float eps top bot cent1 cent1 f1 f2)
	     (type (unsigned-byte 28) neval))
    ;; note that there is no diagnostic we can insert to test if
    ;; the minimum is initially bracketed
    (do ((i 0 (+ i 1)) )
        ((< (abs (- top bot)) (* eps 0.5 (+ eps (abs top) (abs bot)))))
      (declare (fixnum i))
      (if (> i neval-max)
          (error "Too many iterations in findmin"))
      (setf neval i)
      (if (< f1 f2) ;; min is bracketed by [bot cent1 cent2]
          (progn (setf top cent2)
                 (setf cent2 cent1)
                 (setf cent1 (+ (* 0.61803d0 cent2) (* 0.38197d0 bot)))
                 (setf f2 f1)
                 (setf f1 (funcall func cent1)))
          ;; otherwise, bracketed by [cent1 cent2 top]
          (progn (setf bot cent1)
                 (setf cent1 cent2)
                 (setf cent2 (+ (* 0.61803d0 cent1) (* 0.38197d0 top)))
                 (setf f1 f2)
                 (setf f2 (funcall func cent2)))))
    (values
     (if (< f1 f2)
	 cent1
	 cent2)
     neval)))


(defmacro dfloat (x) `(float ,x 1d0))

(defmacro signp (a b)
  `(let ((%aa ,a)
         (%bb ,b))
     (if (>= %bb 0d0)
         (abs %aa)
         (- (abs %aa)))))



(defun bracketmin (ax bx func &key (glimit 100d0) (tiny 1.0d-20))
  "Starting at points ax,bx, find a triplet of points AX,BX,CX
such that FUNC(BX)<FUNC(AX or CX).

Return (VALUES AX BX CX FA FB FC) where FA=FUNC(AX)"
  (declare (type double-float ax bx tiny glimit))

  (prog ((cx 0d0) (fa 0d0) (fb 0d0) (fc 0d0) (fu 0d0) 
	 (gold 0d0) (dum 0d0) (u 0d0) (ulim 0d0) (r 0d0) (q 0d0))
     (declare (type double-float cx fa fb fc gold dum u ulim r q fu))
     
     (setq gold 1.618034d0) 
     
     (setf fa (dfloat (funcall func ax))) 
     (setf fb (dfloat (funcall func bx)))
     (when 
	 (> fb fa) 
       (setf dum ax) 
       (setf ax bx)
       (setf bx dum) 
       (setf dum fb) 
       (setf fb fa) 
       (setf fa dum)) 
     (setf cx (+ bx (* gold (- bx ax)))) 
  (setf fc (dfloat (funcall func cx))) 
  label1 
  (when 
      (>= fb fc)
    (setf r (* (- bx ax) (- fb fc)))
    (setf q (* (- bx cx) (- fb fa)))
    
    (setf u (+ bx
	       (/ (-
		   (+ (* (- bx cx) q) (* (- (- bx ax)) r)))
		  (* 2d0
		     (signp (max (abs (- q r)) tiny)
			    (+ q (- r)))))))
    
    (setf ulim (+ bx (* glimit (- cx bx))))
    (cond 
      ((> (* (- bx u) (- u cx)) 0d0)
       (setf fu (dfloat (funcall func u)))
       (cond 
	 ((< fu fc)  
	  (setf ax bx) 
	  (setf fa fb)
	  (setf bx u) 
	  (setf fb fu) 
	  (return (values ax bx cx fa fb fc)))
	 ((> fu fb) 
	  (setf cx u)
	  (setf fc fu) 
	  (return (values ax bx cx fa fb fc))))
       (setf u (+ cx (* gold (- cx bx)))) 
       (setf fu (dfloat (funcall func u))))
      ((> (* (- cx u) (- u ulim)) 0d0) 
       (setf fu (dfloat (funcall func u)))
       (when 
	   (< fu fc) 
	 (setf bx cx) 
	 (setf cx u)
	 (setf u (+ cx (* gold (- cx bx)))) 
	 (setf fb fc) 
	 (setf fc fu)
	 (setf fu (dfloat (funcall func u)))))
      ((>= (* (- u ulim) (- ulim cx)) 0d0) 
       (setf u ulim) 
       (setf fu (dfloat (funcall func u))))
      (t
       (setf u (+ cx (* gold (- cx bx)))) 
       (setf fu (dfloat (funcall func u)))))
    (setf ax bx) 
    (setf bx cx) 
    (setf cx u) 
    (setf fa fb) 
    (setf fb fc) 
    (setf fc fu)
    (go label1)) 
  
  (return (values ax bx cx fa fb fc))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
