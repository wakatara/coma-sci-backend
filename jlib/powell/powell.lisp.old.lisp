


(defpackage #:powell
  (:use #:common-lisp) 
  (:export #:powell-struct #:powell-struct-p
	   #:powell-struct-name #:powell-struct-n
	   #:powell-struct-x-vec #:powell-struct-dx-vec
	   #:powell-struct-optim-flags #:powell-struct-y
	   #:powell-struct-neval #:powell-struct-params
	   ;;
	   #:build-powell-struct #:init-powell-struct
	   ;;
	   #:run-powell-on-ps #:run-powell 
	   #:*powell-max-steps*))  


(in-package powell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *powell-max-steps*  10000) ;; max number of steps allowed


;; a structure that is passed back to the function being minimized
;; note that it does not contain an actual simplex (the P matrix in
;; NR notation) because the simplex has a different dimensionality
;; from x-vec 
(defstruct powell-struct
  ;; an optional name
  (name "un-named-powell-struct")
  ;; number of dimensions, including the inactive (unoptimized) ones
  (n 0 :type (unsigned-byte 28)) 
  ;; the vector of x values
  (x-vec (make-array 0 :element-type 'double-float)
	 :type (simple-array double-float (*)))
  ;; a vector parallel to x-vec telling if a particular dimension
  ;; is to be optimized - dimensions with NIL are treated as constant
  ;; parameters
  (optim-flags (make-array 0) :type (simple-array t (*)))
  ;; the value y(x) that is being optimized
  (y 0d0 :type double-float)
  ;; the number of function evaluations
  (neval 0 :type (unsigned-byte 28))
  ;; the matrix of initial directions
  (xi-matrix (make-array '(0 0)  :element-type 'double-float)
	    :type (simple-array double-float (* *)))
;  ;; the values of the function at the simplex vectors
;  (y-vec (make-array 0 :element-type 'double-float)
	 :type (simple-array double-float (*)))
  ;; a characteristic scale of the problem, so that we don't expect
  ;; ABSOLUTE (not just ftol fractional) precision greater than this
  ;; value - useful for problems that have an optimum near y(x)=0, where
  ;; the minimizer might try to continue forever 
  (scale 1d-30 :type double-float)
  ;; a slot containing any additional parameters for the function
  (params nil))


(defun build-powell-struct (ndims &key (name "un-named-powell-struct")
			    (params nil) (scale 1d-30))
  "build an powell structure for NDIMS dimensions - it must be then
be initialized with init-powell-struct.  PARAMS is a keyword for an optional
set of user-defined parameters that the function can take, and SCALE is
the characteristic absolute scale (maximum absolute precision) of the problem."
  (make-powell-struct :n ndims
		      :x-vec (make-array ndims :element-type 'double-float)
		      :xi-matrix (make-array (list ndims ndims)
					     :element-type 'double-float)
;;		      :y-vec (make-array (1+ ndims) :element-type 'double-float)
		      :optim-flags (make-array ndims :initial-element t)
		      :name name
		      :scale (float scale 1d0)
		      :params params))





(defun init-powell-struct (ps func x-vec &key optim-flags xi-matrix)
  "initialize an powell-struct at x-vec, with the optional
argument optim-flags being a vector containing T where the X values
are free paramaters, and NIL where they are fixed at the starting value.
XI-MATRIX is an optional NxN matrix of the initial directions"
  (declare (type powell-struct ps)
	   (type function func)
	   (type vector x-vec)
	   (type (or null (simple-array (* *))) xi-matrix) ;; initial directions
	   (type (or null vector) optim-flags))
	   
  (when (not (= (powell-struct-n ps)
		(length x-vec)))
    (error "Bad length of x-vec compared to powell-struct PS"))
  (when (and optim-flags
	     (not (= (powell-struct-n ps) (length optim-flags))))
    (error "Bad length of optim-flags vector compared to powell-struct PS"))
  ;; copy over optim flags and x-vec
  (cond ((not optim-flags)
	 (fill (powell-struct-optim-flags ps) t))
	(t
	 (loop
	    for i of-type (unsigned-byte 28) below (powell-struct-n ps)
	    do
	      (setf (aref (powell-struct-optim-flags ps) i)
		    (not (not (aref optim-flags i)))))))
  ;;
  ;; fill the xi in ps from xi-matrix if xi-matrix is set, else make
  ;; it diagonal
  (loop 
     with n = (powell-struct-n ps)
     with xi-ps = (powell-struct-xi-imatrix ps)
     for i below n
     do (loop for j below n
	     (if xi-matrix
		 (setf (aref xi-ps i j) (float (aref xi-matrix i j) 1d0))
		 (setf (aref xi-ps i j) (if (= i j) 1d0 0d0)))))
  ;; initialize x
  (loop 
     with n = (powell-struct-n ps)
     with x-vec-ps = (powell-struct-x-vec ps)
     for i below n
     do
       (setf (aref x-vec-ps i) (float (aref x-vec i) 1d0)))
  ;; and y
  (funcall func ps)
  ps)


;; internal function to make a reduced x-vec and xi-matrix for an
;; powell struct, leaving out those dimensions that are not being
;; optimized - if FORWARD is T, then map from XI into
;; powell-struct-xi-matrix, otherwise map from powell-struct-xi-matrix
;; into XI
(defun map-reduced-p-and-y-to-powell-struct (xi x ps forward)
  (declare (type (simple-array double-float (* *)) xi)
	   (type (simple-array double-float (*)) x)
	   (type powell-struct ps)
	   (optimize speed))
  (loop ;; over vertices
     with xx = (powell-struct-x-vec ps)
     with flags = (powell-struct-optim-flags ps)
     with n of-type (unsigned-byte 28) = (powell-struct-n ps)
     ;; nred = number of active dimensions 
     for nred of-type (unsigned-byte 28) = (array-dimension xi 0)
     with xips = (powell-struct-xi-matrix ps)
     with ii of-type (unsigned-byte 28) = 0
     for i of-type (unsigned-byte 28) below n
     for iflag = across flags
     when iflag ;; do only live dimensions
     do
       (if forward
	   (setf (aref x ii) (aref xx i))
	   (setf (aref xx i) (aref x ii)))
       (loop ;; over x elements
	  with jj of-type (unsigned-byte 28) = 0
	  for jflag across flags
	  for j of-type (unsigned-byte 28) below n
	  when jflag
	  do
	    (if forward
		(setf (aref xi ii jj) (aref xips i j))
		(setf (aref xips i j) (aref xi ii jj)))
	    (incf jj))
       (incf ii)
       ;;
       finally (return ps)))


(declaim (inline call-func))
;; call the function func (that takes an amoeba struct), filling
;; in only the active slots from x vector
(defun call-func (func ps x)
  (declare (type function func)
	   (type powell-struct ps)
	   (type (simple-array double-float (*)) x)
	   (optimize speed))
  (loop 
     with n of-type (unsigned-byte 28) = (powell-struct-n ps)
     with x-vec of-type (simple-array double-float (*)) = (powell-struct-x-vec ps)
     with j of-type (unsigned-byte 28) = 0
     for i of-type (unsigned-byte 28) below n
     for flag across (powell-struct-optim-flags ps)
     do
       (when flag
	 (setf (aref x-vec i) (aref x j))
	 (incf j)))
  (funcall func ps)
  (incf (powell-struct-neval ps))
  (powell-struct-y ps)) ;; return the y value (called inline, no boxing)




(defun run-powell-on-ps (func ps ftol)
  "Run powell on an powell struct that hps been set up, so that
powell-struct-xi-matrix contains the initial directions, and
powell-struct-x-vec contains the initial point, powell-struct-y
contains the y values for this point.  Return final values in the
powell-struct-x-vec and in powell-struct-y."
  (declare (type (function (powell-struct) t) func)
	   (type powell-struct ps)
	   (type real ftol))
  (let* ((n (powell-struct-n ps))
	 (nred (- n (count nil (powell-struct-optim-flags ps))))
	 (xi (make-array (list nred nred) :element-type 'double-float))
	 (x (make-array nred :element-type 'double-float)))
    (map-reduced-p-and-y-to-powell-struct xi x ps t)
    (powell-internal x y ps (float ftol 1d0) func)
    (map-reduced-p-and-y-to-powell-struct xi x ps nil)
    ;; outputs in powell-struct-x-vec and powell-struct-y should be all set
    ;;
    ps))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun powell-internal (p xi func ps &key (ftol 1.0d-5) (itmax 200))
 (declare (type (simple-array double-float (*)) p)) ;; p is synonymous with x
 (declare (type (simple-array double-float (* *)) xi))
 (declare (type (function (powell-struct) t) func))
 (declare (type powell-struct ps))
 (declare (type ftol))
 (declare (type fixnum itmax))

 (prog* (
  (n (array-dimension p 0))
  (pt (make-array n :element-type 'double-float :initial-element 0d0))
  (ptt (make-array n :element-type 'double-float :initial-element 0d0))
  (xit (make-array n :element-type 'double-float :initial-element 0d0))
  (iter 0) (fret 0d0)  
  (ibig 0) (t0 0d0) (fp 0d0) (del 0d0) (fptt 0d0)) 

  (declare (type (simple-array double-float (*)) pt)) 
  (declare (type (simple-array double-float (*)) ptt)) 
  (declare (type (simple-array double-float (*)) xit)) 
  (declare (type fixnum n itmax nmax iter ibig))
  (declare (type double-float fret t0 fp del fptt))


;;  (setf fret (dfloat (apply func (list-array p))) )
  (setf fret (call-func ps p))


  (do ((j 0 (+ j 1)))
      ((> j (1- n)) t)
      (declare (type fixnum j))
    (setf (aref pt j) (aref p j))) 

  (setf iter 0) 
label1 
  (setf iter (+ iter 1)) 
  (setf fp fret) 
  (setf ibig 0) 
  (setf del 0d0) 

  (do ((i 0 (+ i 1)))
      ((> i (1- n)) t)
      (declare (type fixnum i))
    (do ((j 0 (+ j 1)))
        ((> j (1- n)) t)
        (declare (type fixnum j))
      (setf (aref xit j) (aref xi j i)))

    (setf fptt fret)


    (multiple-value-setq (p xit fret) (linmin p xit func))
    (when 
     (> (abs (- fptt fret)) del)
     (setf del (abs (- fptt fret))) 
     (setf ibig i))) 

  (if (<= (* 2d0 (abs (- fp fret))) (* ftol (+ (abs fp) (abs fret))))
      (return (values p xi iter fret)))
 
  (if (= iter itmax) (error " powell exceeding maximum iterations ")) 

  (do ((j 0 (+ j 1)))
      ((> j (1- n)) t)
      (declare (type fixnum j))
    (setf (aref ptt j) (- (* 2 (aref p j)) (aref pt j)))
    (setf (aref xit j) (- (aref p j) (aref pt j)))
    (setf (aref pt j) (aref p j))) 
;  (setf fptt (dfloat (apply func (list-array ptt)))) 
  (setf fptt (call-func ps ptt))
 
  (if (>= fptt fp) (go label1)) 
  (setf t0 (- (* (* 2d0 (+ (- fp (* 2d0 fret)) fptt))
                (expt (- (- fp fret) del) 2))
             (* del (expt (- fp fptt) 2)))) 
  (if (>= t0 0d0) (go label1)) 
  (multiple-value-setq (p xit fret)  (linmin p xit func)) 
  (do ((j 0 (+ j 1)))
      ((> j (1- n)) t)
      (declare (type fixnum j))
    (setf (aref xi j ibig) (aref xit j))) 

  (go label1)))



(defun linmin (p xi func &key (tol 1.0d-4))
 (declare (type (simple-array double-float (*)) p)) 
 (declare (type (simple-array double-float (*)) xi)) 
 (declare (type double-float tol))

 (prog* (
  (n (array-dimension p 0))
  (xtmp (make-array n :element-type 'double-float :initial-element 0d0))	 
;  (setf ncom n) 
  (pcom (make-array n :element-type 'double-float :initial-element 0d0))
  (xicom (make-array n :element-type 'double-float :initial-element 0d0))
  (fret 0d0) (ncom 0)
  (ax 0d0) (xx 0d0) (bx 0d0) 
  (f1dim 0d0) (xmin 0d0))
  (declare (type fixnum n ncom))
  (declare (type double-float ax xx bx xx fret xmin))
  (declare (type (simple-array double-float (*)) pcom xicom)) 
  (declare (ignore ncom))

;----
    (setq f1dim #'(lambda (x)
      (let 
   ((xt (make-array n :element-type 'double-float :initial-element 0d0)))
      (do ((j 0 (1+ j))) ((> j (1- n)) t)
       (setf (aref xt j) (+ (aref pcom j) (* x (aref xicom j)))))
      (dfloat (apply func (list-array xt))))))


    ;; the one-d function for mnbrak and brent 
    ;; - note that it does not return a value, but leaves its
    (setq f1dim
	  #'(lambda (x ps)
	      (declare (type double-float x))
	      (loop for j of-type fixnum below n
		   do
		   (setq (aref xtmp j)  (+ (aref pcom j) (* x (aref xicom j)))))
	      (call-func func ps xtmp)
	      t))

;; DO THIS DIFFERENTLY - PUT MORE STUFF IN POWELL-STRUCT

;----



  (do ((j 0 (+ j 1)))
      ((> j (1- n)) t)
      (declare (type fixnum j))
    (setf (aref pcom j) (aref p j))
    (setf (aref xicom j) (aref xi j))) 

  (setf ax 0d0) 
  (setf xx 1d0) 
  (multiple-value-setq (ax xx bx) (mnbrak ax xx f1dim)) 
  (multiple-value-setq (fret xmin) (brent ax xx bx f1dim :tol tol)) 
  (do ((j 0 (+ j 1)))
      ((> j (1- n)) t)
      (declare (type fixnum j))
    (setf (aref xi j) (* xmin (aref xi j)))
    (setf (aref p j) (+ (aref p j) (aref xi j)))) 
   
  (return (values p xi fret))))



(defun mnbrak (ax bx func &key (glimit 100d0) (tiny 1.0d-20))
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


(defun brent (ax bx cx f &key (tol 1.0d-4) (itmax 100) (zeps 1.0d-10))
 (declare (type double-float ax bx cx  tol zeps))
 (declare (type fixnum itmax))

 (prog ((a 0d0) (b 0d0) (v 0d0) (w 0d0) (x 0d0) (brent 0d0) 
        (cgold 0d0) (fv 0d0) (fw 0d0) (e 0d0) (fx 0d0) (xm 0d0) 
        (tol1 0d0) (tol2 0d0) (r 0d0) (q 0d0) (p 0d0) (etemp 0d0) 
        (d 0d0) (u 0d0) (fu 0d0) (xmin 0d0))
  (declare (type double-float a b v w x brent
             cgold fv fw e fx xm tol1 tol2 r q p etemp d u fu xmin))

  (setq cgold 0.381966d0) 
  (setf a (min ax cx)) 
  (setf b (max ax cx)) 
  (setf v bx) 
  (setf w v) 
  (setf x v) 
  (setf e 0d0) 
  (setf fx (dfloat (funcall f x))) 
  (setf fv fx) 
  (setf fw fx) 
  (do ((iter 1 (+ iter 1)))
      ((> iter itmax) t)
      (declare (type fixnum iter))
    (setf xm (* 0.5d0 (+ a b)))
    (setf tol1 (+ (* tol (abs x)) zeps))
    (setf tol2 (* 2d0 tol1))
    (if (<= (abs (- x xm)) (- tol2 (* 0.5d0 (- b a)))) (go label3))
    (when 
     (> (abs e) tol1)
     (setf r (* (+ x (- w)) (+ fx (- fv))))
     (setf q (* (+ x (- v)) (+ fx (- fw))))
     (setf p (+ (* (+ x (- v)) q) (* (- (+ x (- w))) r)))
     (setf q (* 2d0 (+ q (- r)))) (if (> q 0d0) (setf p (- p)))
     (setf q (abs q)) (setf etemp e) (setf e d)
     (if (or (>= (abs p) (abs (* 0.5d0 q etemp)))
             (<= p (* q (- a x)))
             (>= p (* q (- b x))))
       (go label1))
     (setf d (/ p q)) (setf u (+ x d))
     (if (or (< (+ u (- a)) tol2) (< (+ b (- u)) tol2))
       (setf d (signp tol1 (+ xm (- x)))))
     (go label2))
label1
    (if 
     (>= x xm) 
     (setf e (+ a (- x)))
     (setf e (+ b (- x))))

    (setf d (* cgold e))
label2
    (if 
     (>= (abs d) tol1)
     (setf u (+ x d))
     (setf u (+ x (signp tol1 d))))

    (setf fu (dfloat (funcall f u)))
    (cond 
     ((<= fu fx)
      (if 
       (>= u x) 
       (setf a x)
       (setf b x))
      (setf v w) 
      (setf fv fw) (setf w x) (setf fw fx) (setf x u) (setf fx fu))
     (t
      (if 
       (< u x)  
       (setf a u) 
       (setf b u))
      (cond 
       ((or (<= fu fw) (= w x)) 
        (setf v w)
        (setf fv fw) 
        (setf w u) 
        (setf fw fu))
       ((or (<= fu fv) (= v x) (= v w)) 
        (setf v u)
        (setf fv fu)))))) 
   
  (error " brent exceed maximum iterations. ") 
  label3 
  (setf xmin x) 
  (setf brent fx) 
   
  (return (values brent xmin))))

