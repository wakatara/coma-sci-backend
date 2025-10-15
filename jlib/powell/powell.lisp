
;; powell optimizer - see test example at bottom
;; for example usage

(defpackage #:powell
  (:use #:common-lisp) 
  (:export #:powell-struct #:powell-struct-p
	   #:powell-struct-name #:powell-struct-n
	   #:powell-struct-x-vec #:powell-struct-xi-matrix
	   #:powell-struct-optim-flags #:powell-struct-y
	   #:powell-struct-neval #:powell-struct-params
	   ;;
	   #:build-powell-struct #:init-powell-struct
	   ;;
	   #:run-powell-on-ps 
	   #:*powell-max-steps*))  

  
(in-package powell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *powell-max-steps*  100000) ;; max number of steps allowed


;; a structure that is passed back to the function being minimized
;; note that it does not contain an actual simplex (the P matrix in
;; NR notation) because the simplex has a different dimensionality
;; from x-vec - this is a subclass of MD-OPTIM-STRUCT in md-optim.lisp
(defstruct (powell-struct (:include md-optim:md-optim-struct))
  (xi-matrix (make-array '(0 0)  :element-type 'double-float)
	     :type (simple-array double-float (* *)))
  ;; a characteristic scale of the problem, so that we don't expect
  ;; ABSOLUTE (not just ftol fractional) precision greater than this
  ;; value - useful for problems that have an optimum near y(x)=0, where
  ;; the minimizer might try to continue forever 
  (scale 1d-30 :type double-float)
  ;; a structure for the one-d minimization - set to NIL before return
  (1ds nil))



;; a structure used for the 1d optimizations, contained in the 1ds slot
;; of powell-struct
(defstruct 1ds
  (pcom (make-array 0  :element-type 'double-float)
	 :type (simple-array double-float (*)))
  (xicom (make-array 0  :element-type 'double-float)
	 :type (simple-array double-float (*)))
  (xtmp (make-array 0  :element-type 'double-float)
	 :type (simple-array double-float (*)))
  (linmin 0d0 :type double-float)
  ;; return values for mnbrak/powell
  (ax 0d0 :type double-float)
  (bx 0d0 :type double-float)
  (cx 0d0 :type double-float)
  (fa 0d0 :type double-float)  ;; fa,fb,fc returned by mnbrak but not used
  (fb 0d0 :type double-float)  ;; by linmin in powell
  (fc 0d0 :type double-float)
  ;; return values for brent/powell
  (brent  0d0 :type double-float)
  (xmin   0d0 :type double-float) 
  ;;
  ;; passed to mnbrak - these don't need to change, probably
  (glimit 100d0 :type double-float) ;; maximum magnification allowed for parabolic-fit step
  (tiny 1d-100 :type double-float)  ;; prevents division by zero 
  ;; passed to brent
  (itmax *powell-max-steps* :type (unsigned-byte 28))
  (zeps  1d-30 :type double-float) ;; a number that sets the scale of
				   ;; the problem when the minimum is
				   ;; exactly zero - we set it equal
				   ;; to powell-struct-scale
  ;;
  (n 0d0 :type (unsigned-byte 28))) ;; dimensions being used
  
 

(defun build-powell-struct (ndims &key (name "un-named-powell-struct")
				    (params nil) (scale 1d-30)
				    (optim-flags nil))
  "Build an powell structure for NDIMS dimensions - it must be then
be initialized with init-powell-struct.  PARAMS is a keyword for an optional
set of user-defined parameters that the function can take, and SCALE is
the characteristic absolute scale (maximum absolute precision) of the problem."
  (when optim-flags
    (when (not (and (= (length optim-flags) ndims)
		    (every (lambda (x) (member x '(t nil))) optim-flags)))
      (error "OPTIM-FLAGS is not a vector of T/NIL equal in length to NDIMS=~A" ndims)))
  
  (make-powell-struct :n ndims
		      :x-vec (make-array ndims :element-type 'double-float)
		      :xi-matrix (make-array (list ndims ndims)
					     :element-type 'double-float)
;;		      :y-vec (make-array (1+ ndims) :element-type 'double-float)
		      :optim-flags (or optim-flags
				       (make-array ndims :initial-element t))
		      :name name
		      :scale (float scale 1d0)
		      :params params))





(defun init-powell-struct (ps func x-vec &key optim-flags xi-matrix)
  "Initialize a powell-struct at x-vec, with the optional
argument optim-flags being a vector containing T where the X values
are free paramaters, and NIL where they are fixed at the starting value.
XI-MATRIX is an optional NxN matrix of the initial directions.

If OPTIM-FLAGS is NIL, then current optim-flags in PS are used."
  (declare (type powell-struct ps)
	   (type function func)
	   (type vector x-vec)
	   ;; initial directions
	   (type (or null (simple-array double-float (* *))) xi-matrix) 
	   (type (or null vector) optim-flags))
	   
  (when (not (= (powell-struct-n ps)
		(length x-vec)))
    (error "Bad length of x-vec compared to powell-struct PS"))
  (when (and optim-flags
	     (not (= (powell-struct-n ps) (length optim-flags))))
    (error "Bad length of optim-flags vector compared to powell-struct PS"))
  ;; copy over optim flags and x-vec
  (cond ((not optim-flags)
	 nil) ;; respect current optim flags if optim-flags not given
	(t
	 (loop
	    for i of-type (unsigned-byte 28) below (powell-struct-n ps)
	    do
	      (setf (aref (powell-struct-optim-flags ps) i)
		    (not (not (aref optim-flags i)))))))

  ;; build the 1d structure
  (let ((nopt (count t (powell-struct-optim-flags ps)))) ;; number of vars optimized
    (setf (powell-struct-1ds ps)
	  (make-1ds :pcom  (make-array nopt :element-type 'double-float)
		    :xicom (make-array nopt :element-type 'double-float)
		    :xtmp (make-array nopt :element-type 'double-float)
		    :n nopt
		    :zeps (powell-struct-scale ps))))

  ;;
  ;; fill the xi in ps from xi-matrix if xi-matrix is set, else make
  ;; it diagonal
  (loop 
     with n = (powell-struct-n ps)
     with xi-ps = (powell-struct-xi-matrix ps)
     for i below n
     do (loop for j below n
	     do
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
;; optimized - if FORWARD is T, then map from X[I] into
;; X & POWELL-STRUCT-X[I]-MATRIX, otherwise map from POWELL-STRUCT-X[I]-MATRIX
;; into X[I]
(defun map-reduced-x-and-xi-to-powell-struct (x xi ps forward)
  (declare (type (simple-array double-float (*)) x)
	   (type (simple-array double-float (* *)) xi)
	   (type powell-struct ps)
	   (optimize speed))
  (loop 
     with x-ps = (powell-struct-x-vec ps)
     with xi-ps = (powell-struct-xi-matrix ps)
     with flags = (powell-struct-optim-flags ps)
     with n of-type (unsigned-byte 28) = (powell-struct-n ps)
     ;; nred = number of active dimensions 
     with nred of-type (unsigned-byte 28) = (array-dimension xi 0)
     with ii of-type (unsigned-byte 28) = 0
     for i of-type (unsigned-byte 28) below n
     for iflag across flags
     when iflag ;; do only live dimensions
     do
       (if forward
	   (setf (aref x-ps i) (aref x ii))
	   (setf (aref x ii) (aref x-ps i)))
       (loop ;; over x elements
	  with jj of-type (unsigned-byte 28) = 0
	  for jflag across flags
	  for j of-type (unsigned-byte 28) below n
	  when jflag
	  do
	    (if forward
		(setf (aref xi-ps i j) (aref xi ii jj))
		(setf (aref xi ii jj) (aref xi-ps i j)))
	    (incf jj))
       (incf ii)
       ;;
       finally (return ps)))
  
 
(declaim (inline call-func))
;; call the function func (that takes a powell struct), filling
;; in only the active slots from x vector
(defun call-func (func ps x)
  (declare (type function func)
	   (type powell-struct ps)
	   (type (simple-array double-float (*)) x)
	   (optimize speed))
  (loop 
     with n of-type (unsigned-byte 28) = (powell-struct-n ps)
     with x-vec of-type (simple-array double-float (*)) = 
       (powell-struct-x-vec ps)
     with j of-type (unsigned-byte 28) = 0
     for i of-type (unsigned-byte 28) below n
     for flag across (powell-struct-optim-flags ps)
     do
       (when flag
	 (setf (aref x-vec i) (aref x j))
	 (incf j)))
  (funcall func ps)
  ;; error on invalid return value
  (when (float-utils:double-float-nan-or-infinity-p (powell-struct-y ps))
    (locally (declare ;; don't complain about boxing the error note
	      #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
      (error "Function returned invalid NaN or Inf value ~A during Powell optimization with parameters ~A"
	     (powell-struct-y ps)
	     (powell-struct-x-vec ps))))
  ;;
  (incf (powell-struct-neval ps))
  (powell-struct-y ps)) ;; return the y value (called inline, no boxing)
  
 
;; call the function at 1d location x along vector x*xcom+pcom, where
;; xcom and pcom are vectors contained in the 1ds inside ps

(declaim (inline call-func-1d))

(defun call-func-1d (func ps x)
  (declare (type function func)
	   (type powell-struct ps)
	   (type double-float x)
	   (optimize speed))
  
  
  (loop 
    with 1ds of-type 1ds = (powell-struct-1ds ps)
    with pcom of-type (simple-array double-float (*)) = (1ds-pcom 1ds)
    with xicom of-type (simple-array double-float (*)) = (1ds-xicom 1ds)
    with xtmp of-type (simple-array double-float (*)) = (1ds-xtmp 1ds)
    with n of-type fixnum = (1ds-n 1ds)
    for j of-type fixnum below n
    do
       (setf (aref xtmp j)  (+ (aref pcom j) (* x (aref xicom j))))
    finally
	 (return (call-func func ps xtmp))))

	 
 


(defun run-powell-on-ps (func ps ftol)
  "Run powell on an powell struct that hps been set up, so that
powell-struct-xi-matrix contains the initial directions, and
powell-struct-x-vec contains the initial point, powell-struct-y
contains the y values for this point.  Return final values in the
powell-struct-x-vec and in powell-struct-y.

FTOL is minimum fractional decrease in the value of FUNC.  If
FTOL fails to fall by fraction FTOL in one iteration, then optimization
is deemed to be complete.
"
  (declare (type (function (powell-struct) t) func)
	   (type powell-struct ps)
	   (type real ftol))
  (when (not (powell-struct-1ds PS))
    (error "POWELL-STRUCT-1DS one-d minimization struction is
NIL. Forgot to call INIT-POWELL-STRUCT?"))
  (let* ((n (powell-struct-n ps))
	 (nred (- n (count nil (powell-struct-optim-flags ps))))
	 (xi (make-array (list nred nred) :element-type 'double-float))
	 (x (make-array nred :element-type 'double-float)))
    (map-reduced-x-and-xi-to-powell-struct x xi ps nil) ;; from x,xi into PS
    (powell-internal x xi ps func :ftol (float ftol 1d0))
    (map-reduced-x-and-xi-to-powell-struct x xi ps t)
    ;; outputs in powell-struct-x-vec and powell-struct-y should be all set
    ;;
    ps))




(deftype short-fixnum () '(signed-byte 27)) ;; short fixnum

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(defmacro signp (a b)
  `(let ((aa ,a)
	 (bb ,b))
     (if (>= bb 0d0)
	 (abs aa)
	 (- (abs aa)))))
     

(declaim (inline mnbrak/powell brent/powell linmin))

 
(defmacro %return-from-mnbrak/powell ()
  `(progn (setf (1ds-ax 1ds) ax
		(1ds-bx 1ds) bx
		(1ds-cx 1ds) cx
		(1ds-fa 1ds) fa
		(1ds-fb 1ds) fb
		(1ds-fc 1ds) fc)
	  ;;
	  (return t)))

;; DANGER - MNBRAK somtimes runs off to infinity for well behaved functions
#+nil 
(format t 
"WARNING - MNBRAK/POWELL  seems to run off to infinity for well behaved 
           functions sometimes.~%")

(defun mnbrak/powell (ax bx ps func)
 (declare (type double-float ax bx)
	  (type function func)
	  (type powell-struct ps)
	  (optimize speed))


 (prog ((ax ax) (bx bx) (cx 0d0) (fa 0d0) (fb 0d0) (fc 0d0) (fu 0d0) 
	(tiny 0d0) (glimit 0d0)
        (gold 0d0) ;; (dum 0d0) 
	(u 0d0) (ulim 0d0) (r 0d0) (q 0d0)
	(1ds (powell-struct-1ds ps)))
    (declare (type double-float ax bx cx fa fb fc fu 
		   tiny glimit gold ;; dum 
		   u ulim r q)
	     (type 1ds 1ds))

  (setf tiny   (1ds-tiny 1ds)
	glimit (1ds-glimit 1ds))
  ;;
  (setf gold 1.618034d0) 
  (setf fa (call-func-1d func ps ax))
  (setf fb (call-func-1d func ps bx))
  (when 
   (> fb fa) 

;;   (setf dum ax) 
;;   (setf ax bx)
;;   (setf bx dum) 
    (rotatef ax bx) ;; for some reason, using DUM to swap vars produced boxing

;;   (setf dum fb) 
;;   (setf fb fa)  
;;   (setf fa dum)
    (rotatef fa fb)) 

  (setf cx (+ bx (* gold (- bx ax))))  
  (setf fc (call-func-1d func ps cx))
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
     (setf fu (call-func-1d func ps u))
     (cond 
      ((< fu fc)  
       (setf ax bx) 
       (setf fa fb)
       (setf bx u) 
       (setf fb fu) 
       (%return-from-mnbrak/powell))
      ((> fu fb) 
       (setf cx u) 
       (setf fc fu) 
       (%return-from-mnbrak/powell)))
     (setf u (+ cx (* gold (- cx bx)))) 
     (setf fu (call-func-1d func ps u)))
    ((> (* (- cx u) (- u ulim)) 0d0) 
     (setf fu (call-func-1d func ps u))
     (when 
	 (< fu fc) 
       (setf bx cx) 
       (setf cx u)  
       (setf u (+ cx (* gold (- cx bx))))  
       (setf fb fc) 
       (setf fc fu)
       (setf fu (call-func-1d func ps u))))
    ((>= (* (- u ulim) (- ulim cx)) 0d0) 
     (setf u ulim) 
     (setf fu (call-func-1d func ps u)))
    (t
     (setf u (+ cx (* gold (- cx bx))))  
     (setf fu (call-func-1d func ps u))))
   (setf ax bx) 
   (setf bx cx) 
   (setf cx u)  
   (setf fa fb) 
   (setf fb fc) 
   (setf fc fu)
   (go label1)) 
    
  ;; package return values to avoid boxing
  (%return-from-mnbrak/powell)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 

(defun brent/powell (ax bx cx ps func tol)
 (declare (type double-float ax bx cx  tol)
	  (type powell-struct ps)
	  (type function func)
	  (optimize speed))

 (prog ((a 0d0) (b 0d0) (v 0d0) (w 0d0) (x 0d0) (brent 0d0) 
        (cgold 0d0) (fv 0d0) (fw 0d0) (e 0d0) (fx 0d0) (xm 0d0) 
        (tol1 0d0) (tol2 0d0) (r 0d0) (q 0d0) (p 0d0) (etemp 0d0) 
        (d 0d0) (u 0d0) (fu 0d0) (xmin 0d0) (zeps 0d0) (itmax 0)
	(1ds (powell-struct-1ds ps)))
  (declare (type double-float a b v w x brent
             cgold fv fw e fx xm tol1 tol2 r q p etemp d u fu xmin zeps)
	   (type 1ds 1ds)
	   (type short-fixnum itmax))
	   
  (setf itmax (1ds-itmax 1ds)
	zeps  (1ds-zeps 1ds))

  (setf cgold 0.381966d0) 
  (setf a (min ax cx)) 
  (setf b (max ax cx)) 
  (setf v bx) 
  (setf w v) 
  (setf x v) 
  (setf e 0d0) 
  (setf fx (call-func-1d func ps x))
  (setf fv fx) 
  (setf fw fx) 
  (do ((iter 1 (+ iter 1)))
      ((> iter itmax) t)
      (declare (type short-fixnum iter))
    (setf xm (* 0.5d0 (+ a b)))
    (setf tol1 (+ (* tol (abs x)) zeps))
    (setf tol2 (* 2d0 tol1))
    ;;
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
    (setf fu (call-func-1d func ps u))
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
   
  ;; package return values in structure
  (setf (1ds-brent 1ds) brent
	(1ds-xmin 1ds)  xmin)

  (return t)))
 
(defun linmin (p xi func ps &key (tol 1.0d-4))
  (declare (type (simple-array double-float (*)) p)
	   (type (simple-array double-float (*)) xi) 
	   (type powell-struct ps)
	   (type double-float tol)
	   (optimize speed))

 (prog* (
  (n (array-dimension p 0))
  ;(xtmp (make-array n :element-type 'double-float :initial-element 0d0))
  (1ds (powell-struct-1ds ps))
  (pcom (1ds-pcom 1ds))
  (xicom (1ds-xicom 1ds))
;;  (xtmp (1ds-pcom 1ds))
	 
;  (setf ncom n) 
;  (pcom (make-array n :element-type 'double-float :initial-element 0d0))
;  (xicom (make-array n :element-type 'double-float :initial-element 0d0))
  (fret 0d0) (ncom 0)
  (ax 0d0) (xx 0d0) (bx 0d0) 
  ;;(f1dim 0d0) 
  (xmin 0d0))
  (declare (type short-fixnum n ncom))
  (declare (type double-float ax xx bx xx fret xmin))
  (declare (type (simple-array double-float (*)) pcom xicom)) 
  (declare (ignore ncom))

  (do ((j 0 (+ j 1)))
      ((> j (1- n)) t)
      (declare (type short-fixnum j))
    (setf (aref pcom j) (aref p j))
    (setf (aref xicom j) (aref xi j))) 

  (setf ax 0d0) 
  (setf xx 1d0) 
;;  (multiple-value-setf (ax xx bx) (mnbrak ax xx f1dim)) 
;;  (multiple-value-setf (fret xmin) (brent ax xx bx f1dim :tol tol)) 
;;  (multiple-value-setf (ax xx bx) (mnbrak/powell ax xx ps func)) 
;;  (multiple-value-setf (fret xmin) (brent/powell ax xx bx ps func :tol tol)) 


  (mnbrak/powell ax xx ps func)
  (setf ax (1ds-ax 1ds)
	xx (1ds-bx 1ds)
	bx (1ds-cx 1ds)) 

  (brent/powell ax xx bx ps func tol)
  (setf fret (1ds-brent 1ds)
	xmin (1ds-xmin 1ds))
  

  (do ((j 0 (+ j 1)))
      ((> j (1- n)) t)
      (declare (type short-fixnum j))
    (setf (aref xi j) (* xmin (aref xi j)))
    (setf (aref p j) (+ (aref p j) (aref xi j)))) 
   
  (setf (1ds-linmin 1ds) fret) ;; put return value in 1df
  (return))) 



(defun powell-internal (p xi ps func  &key (ftol 1.0d-5) (itmax 200))
  (declare (type (simple-array double-float (*)) p) ;; p is synonymous with x
	   (type (simple-array double-float (* *)) xi)
	   (type (function (powell-struct) t) func)
	   (type powell-struct ps)
	   (type double-float ftol)
	   (type short-fixnum itmax)
	   (optimize speed))



 (prog* (
  (n (array-dimension p 0))
  (pt (make-array n :element-type 'double-float :initial-element 0d0))
  (ptt (make-array n :element-type 'double-float :initial-element 0d0))
  (xit (make-array n :element-type 'double-float :initial-element 0d0))
  (iter 0) (fret 0d0)  (1ds nil)
  (ibig 0) (t0 0d0) (fp 0d0) (del 0d0) (fptt 0d0)) 

  (declare (type (simple-array double-float (*)) pt)) 
  (declare (type (simple-array double-float (*)) ptt)) 
  (declare (type (simple-array double-float (*)) xit)) 
  (declare (type short-fixnum n itmax iter ibig))
  (declare (type double-float fret t0 fp del fptt)) 
  (declare (type (or null 1ds) 1ds))

  
  (setf 1ds  (powell-struct-1ds ps))

;;  (setf fret (dfloat (apply func (list-array p))) )
  (setf fret (call-func func ps p))


  (do ((j 0 (+ j 1)))
      ((> j (1- n)) t)
      (declare (type short-fixnum j))
    (setf (aref pt j) (aref p j))) 

  (setf iter 0) 
label1 
  (setf iter (+ iter 1)) 
  (setf fp fret) 
  (setf ibig 0) 
  (setf del 0d0) 

  (do ((i 0 (+ i 1)))
      ((> i (1- n)) t)
      (declare (type short-fixnum i))
    (do ((j 0 (+ j 1)))
        ((> j (1- n)) t)
        (declare (type short-fixnum j))
      (setf (aref xit j) (aref xi j i)))

    (setf fptt fret)

    (linmin p xit func ps)
    (setf fret (1ds-linmin 1ds))
 
    (when 
     (> (abs (- fptt fret)) del)
     (setf del (abs (- fptt fret))) 
     (setf ibig i))) 

  (when (<= (* 2d0 (abs (- fp fret))) (* ftol (+ (abs fp) (abs fret))))
      ;; return values are in powell-struct
    (return iter))

  (if (= iter itmax) (error " powell exceeding maximum iterations ")) 

  (do ((j 0 (+ j 1)))
      ((> j (1- n)) t)
      (declare (type short-fixnum j))
    (setf (aref ptt j) (- (* 2 (aref p j)) (aref pt j)))
    (setf (aref xit j) (- (aref p j) (aref pt j)))
    (setf (aref pt j) (aref p j))) 
;  (setf fptt (dfloat (apply func (list-array ptt)))) 
  (setf fptt (call-func func ps ptt))
 
  (if (>= fptt fp) (go label1)) 
  (setf t0 (- (* (* 2d0 (+ (- fp (* 2d0 fret)) fptt))
                (expt (- (- fp fret) del) 2))
             (* del (expt (- fp fptt) 2)))) 
  (if (>= t0 0d0) (go label1))
 
  (linmin p xit func ps)
  (setf fret (1ds-linmin 1ds))

  (do ((j 0 (+ j 1)))
      ((> j (1- n)) t)
      (declare (type short-fixnum j))
    (setf (aref xi j ibig) (aref xit j))) 

  (go label1)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-func-even-powers (ps)
  (declare (type powell-struct ps))
  (let* ((xvec (powell-struct-x-vec ps))
	 (x1 (aref xvec 0))
	 (x2 (aref xvec 1))
	 (x3 (aref xvec 2))
	 (x4 (aref xvec 3))
	 (x5 (aref xvec 4))
	 ;;
	 (y (+
	     1d0 ;; put in 1 so that ftol becomes nicely defined
	     (+ (expt (- x1 1d0) 2)
		(expt (- x2 2d0) 4)
		(expt (- x3 3d0) 6)
		(expt (- x4 4d0) 8)
		(expt (- x5 5d0) 10)
		))))
    
    ;;(format t "f(~A,~A,~A,~A,~A)=~A~%" x1 x2 x3 x4 x5 y)
    (if (> (powell-struct-neval ps) 20000)
	(error "Too many evaulations"))
    (setf (powell-struct-y ps) y)
    t))
    

(defun test-func-quadratic (ps)
  (declare (type powell-struct ps))
  (let* ((xvec (powell-struct-x-vec ps))
	 (x1 (aref xvec 0))
	 (x2 (aref xvec 1))
	 (x3 (aref xvec 2))
	 (x4 (aref xvec 3))
	 (x5 (aref xvec 4))
	 ;;
	 (y (+
	     1d0
	     (+ (expt (- x1 1d0) 2)
		(expt (- x2 2d0) 2)
		(expt (- x3 3d0) 2)
		(expt (- x4 4d0) 2)
		(expt (- x5 5d0) 2)
		))))
    
    ;;(format t "f(~A,~A,~A,~A,~A)=~A~%" x1 x2 x3 x4 x5 y)
    (if (> (powell-struct-neval ps) 20000)
	(error "Too many evaulations"))
    (setf (powell-struct-y ps) y)
    t))



;; a test function that is a product of absolute values - ugly derivatives,
;; so it should give powell a run for its money - in fact, this function
;; needs the 1d0 constant term to be optimizable at all
(defun test-func-abs (ps)
  (declare (type powell-struct ps))
  (let* ((xvec (powell-struct-x-vec ps))
	 (x1 (aref xvec 0))
	 (x2 (aref xvec 1))
	 (x3 (aref xvec 2))
	 (x4 (aref xvec 3))
	 (x5 (aref xvec 4))
	 ;;
	 (y (+
	     1d0
	     (* (abs (- x1 1d0))
		(abs (- x2 2d0))
		(abs (- x3 3d0))
		(abs (- x4 4d0))
		(abs (- x5 5d0))
		))))
    
    ;;(format t "f(~A,~A,~A,~A,~A)=~A~%" x1 x2 x3 x4 x5 y)
    (if (> (powell-struct-neval ps) 20000)
	(error "Too many evaulations"))
    (setf (powell-struct-y ps) y)
    t))
	 

(defun test-powell (&key (flags #(t t t t t))
		    (test-func #'test-func-even-powers)
		    (ftol 1d-7))
  (let* ((ps (build-powell-struct 5)))
    (init-powell-struct ps test-func #(99 35 98 23 77))
    (setf (powell-struct-optim-flags ps) flags)
;;    (print ps)
    ;;
    (run-powell-on-ps test-func ps ftol)))
