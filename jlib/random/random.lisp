;; 2007-05-25 - Converted all to double float - JTK

;; NOTE - random states have BORDEAUX-THREADS locks to allow multithreaded operations.
;;
;;        These threads slow down random number generation by a factor of about 3.
;;        To avoid this slowdown, use a custom random state without a lock, eg
;;              (make-uniform-random-state :lock nil)
;;              (make-gaussian-random-state :lock nil)
;;              (make-poisson-random-state :lock nil) 
;;
;;        There is also a risk of interrupting the lisp process and leaving the lock
;;        frozen, because (for speed) the locking is done without an UNWIND-PROTECT

(defpackage #:random
  (:use #:common-lisp)
  (:export
   #:set-randseed    #:set-gaussian-randseed
   #:make-uniform-random-state #:make-gaussian-random-state
   #:make-poisson-random-state
   #:uniform #:uniform-in-range
   #:gaussian
   #:powerlaw         #:powerlaw-from-uniform
   #:exponential      #:exponential-from-uniform
   #:exponential10    #:exponential10-from-uniform 
   #:poisson          ;; we don't have #:poisson-from-uniform
                      ;; because poisson is weird
   #:shuffle-vector
   #:make-random-string
   ;;
   ;; fast repeating gaussians - correlated, so use only for 
   ;; stuff like image backgrounds
   #:build-fast-gaussian-struct
   #:fast-repeating-gaussian
   ;;
   ;; generated correlated gaussians
   #:prepare-corgauss
   #:correlated-gaussian
   #:test-corgauss
   ;;
   ;; fast medium quality random numbers using Park & Miller
   #:pmran28
   #:pmrann
   #:pmranf
   ))


(in-package random)

#+(or cmu sbcl)
(declaim (#+cmu ext:maybe-inline #+sbcl sb-ext:maybe-inline
		uniform gaussian 
		powerlaw     powerlaw-from-uniform
		exponential  exponential-from-uniform
		exponential10 exponential10-from-uniform
		poisson        
		shuffle-vector))


(defstruct uniform-random-state
  (lock  (bordeaux-threads:make-lock "uniform-random-state")) ;; if NIL, no locking is done
  (ran3Inext 0 :type (signed-byte 28))
  (ran3Inextp 0 :type (signed-byte 28))
  (ran3MA (make-array 56 :element-type 'double-float 
			  :initial-element 0d0)
	  :type (simple-array double-float (56)))
  (rseed -1 :type (signed-byte 28)))

(defvar *uniform-random-state* (make-uniform-random-state))


;;uniform random deviate between 0.0 and 1.0
;;taken from Knuth, Seminumerical Alogrithms
;;original uses a VAR (reference) parameter, hence rseed contained
;;in uniform-random-state
;;
(defun uniform (&optional (uniform-random-state *uniform-random-state*))
  "return a uniform double-float in [0,1] using the optional random
number state argument, defaulting to *UNIFORM-RANDOM-STATE*"
  (declare (optimize speed))
  ;;
  (when (uniform-random-state-lock uniform-random-state)
    (bordeaux-threads:acquire-lock (uniform-random-state-lock uniform-random-state)))
  ;;
   (let ((ran3Inext (uniform-random-state-ran3Inext uniform-random-state))
	 (ran3Inextp (uniform-random-state-ran3Inextp uniform-random-state))
	 (ran3Ma (uniform-random-state-ran3Ma uniform-random-state))
	 (rseed (uniform-random-state-rseed uniform-random-state))
	 (mbig 4.0d6)
	 (1/mbig 2.5d-7)
	 (mseed 1618033d0)
	 (fac 2.5d-7)
	 (mj 0d0) (mk 0d0) (ii 0)) 
     (declare (type double-float mbig mseed fac mj mk)
	      (type (signed-byte 28) ran3Inext ran3Inextp)
	      (type (unsigned-byte 28) ii))		 
     (cond ((minusp rseed)             ;signals initialization
	    (setf mj (+ mseed rseed)) ;initialize Ma[55] -- the 55 is magic!
	    (if (>= mj 0d0)
		(decf mj (* mbig (truncate (* mj 1/mbig))))
		(setf mj (+ (- mbig (abs mj)) (* mbig (floor (abs mj) mbig)))))
	    ;; that is, mj MOD mbig for real variables
	    (setf (aref ran3Ma 55) mj)
	    (setf mk 1d0)
	    (do ((i 1 (+ i 1)))
		((= i 55))
	      (declare (type (unsigned-byte 10) i))
	      (setf ii (mod (* 21 i) 55))
	      (setf (aref ran3Ma ii) mk)
	      (setf mk (- mj mk))
	      (if (minusp mk) (incf mk mbig))
	      (setf mj (aref ran3Ma ii)))
	    ;;initialize table with not-very-random numbers
	    (do ((k 1 (+ k 1)))
		((= k 5))
	      (declare (type (unsigned-byte 10) k))
	      (do ((i 1 (+ i 1)))
		  ((= i 56))
		(declare (type (unsigned-byte 10) i))
		(decf (aref ran3Ma i) (aref ran3Ma (+ 1 (mod (+ i 30) 55))))
		(if (minusp (aref ran3Ma i))
		    (incf (aref ran3Ma i) mbig))))
	    (setf ran3Inext 0)		;prepare indices for first
	                                ;  generated number
	    (setf ran3Inextp 31)	;31 is magic -- see Knuth
	    (setf rseed 1)))		;end of intialization
     (incf ran3Inext)
     (if (= ran3Inext 56) (setf ran3Inext 1))
     (incf ran3Inextp)
     (if (= ran3Inextp 56) (setf ran3Inextp 1))
     (setf mj (- (aref ran3Ma ran3Inext) (aref ran3Ma ran3Inextp)))
     (if (minusp mj) (incf mj mbig))
     (setf (aref ran3Ma ran3Inext) mj)
     ;; now patch up the uniform random state
     (setf (uniform-random-state-ran3Inext uniform-random-state) ran3Inext)
     (setf (uniform-random-state-ran3Inextp uniform-random-state) ran3Inextp)
     (setf (uniform-random-state-ran3Ma uniform-random-state) ran3Ma)
     (setf (uniform-random-state-rseed uniform-random-state) rseed)
     
     (when (uniform-random-state-lock uniform-random-state)
       (bordeaux-threads:release-lock
	(uniform-random-state-lock uniform-random-state)))
     ;;
     (* mj fac)))


(defun uniform-in-range
    (x1 x2
     &optional (uniform-random-state *uniform-random-state*))
  "return a uniform double-float in [x1,x2] using the optional random
number state argument, defaulting to *UNIFORM-RANDOM-STATE*"
  (declare (type double-float x1 x1)
	   (optimize speed))
  (+ x1 (* (- x2 x1) (uniform uniform-random-state))))
  

;; if LOCK is a lock, then lock on it and do body, else just do body
(defmacro %maybe-lock (lock &body body)
  `(let ((%the-lock% ,lock))
     (flet ((%maybe-lock-func% ()  ,@body))
       (if %the-lock%
	   (bordeaux-threads:with-lock-held (%the-lock%) (%maybe-lock-func%))
	   (%maybe-lock-func%)))))
       
;;
(defun set-randseed (idum &optional (uniform-random-state *uniform-random-state*))
  "Set the random seed of UNIFORM-RANDOM-STATE to the positive number idum"
  (if (> 0 idum) (error "random seed must be > 0"))
  (%maybe-lock (uniform-random-state-lock uniform-random-state)
	       (setf (uniform-random-state-rseed uniform-random-state)  (- idum))))



(defstruct gaussian-random-state
  (lock  (bordeaux-threads:make-lock "gaussian-random-state")) ;; if NIL, no locking is done
  ;; no locking in UNIFORM-RANDOM-STATE because locking is the gaussian layer
  (urs (make-uniform-random-state :lock nil) :type uniform-random-state)
  (GasdevIset 0d0 :type double-float)
  (GasdevGset 0d0 :type double-float))

(defvar *gaussian-random-state* (make-gaussian-random-state))


  ;;gaussian noise, zero mean, unit variance, uses ran3
(defun gaussian (&optional (gaussian-random-state *gaussian-random-state*))
  "Generate a double-float gaussian deviate using given gaussian-random-state,
or *gaussian-random-state* by default"
  (declare (inline uniform)
	   (optimize speed))
  ;; 
  (when (gaussian-random-state-lock gaussian-random-state)
    (bordeaux-threads:acquire-lock (gaussian-random-state-lock gaussian-random-state)))
  ;;
  (let* ((GasdevIset (gaussian-random-state-GasdevIset gaussian-random-state))
	 (GasdevGset (gaussian-random-state-GasdevGset gaussian-random-state))
	 (urs (gaussian-random-state-urs gaussian-random-state))
	 (gaussian-result
	   ;;no extra number handy -- go calulate two and save one  
	   (cond ((zerop GasdevIset) 
		  (let ((v1 0d0) (v2 0d0) (r 0d0) (fac 0d0))
		    (declare (type double-float v1 v2 r fac) 
			     (optimize speed))
		    (do ()
			((< 0d0 r 1d0))
		      (setf v1 (- (* 2d0 (uniform urs)) 1d0))
		      (setf v2 (- (* 2d0 (uniform urs)) 1d0))
		      ;; random(0) because we don't have reference args 
		 
		      (setf r (+ (expt v1 2) (expt v2 2))))
		    ;;
		    (locally
			(declare (type (double-float #.least-positive-double-float
						     #.most-positive-double-float)
				       r))
		      (setf fac 
			    (sqrt (the (double-float 0d0)
				       (/ (* -2d0 (log r)) r))))
		      )
		    ;;
		    (setf (gaussian-random-state-GasdevGset gaussian-random-state)
			  (* v1 fac))
		    (setf (gaussian-random-state-GasdevIset gaussian-random-state)
			  1d0)
		    (* v2 fac)))
		 (t
		  (setf (gaussian-random-state-GasdevIset gaussian-random-state) 0d0)
		  GasdevGset))))
    ;;
    (when (gaussian-random-state-lock gaussian-random-state)
      (bordeaux-threads:release-lock (gaussian-random-state-lock gaussian-random-state)))
    ;;
    gaussian-result))




(defun set-gaussian-randseed (idum &optional (gaussian-random-state 
					      *gaussian-random-state*))
  "Set the random seed of *GAUSSIAN-UNIFORM-RANDOM-STATE* to the
positive number idum"
  (if (> 0 idum) (error "random seed must be > 0"))
  (set-randseed idum (gaussian-random-state-urs gaussian-random-state)))


  
(defun powerlaw-from-uniform (a x0 x1 urand)
  "Turn a random number URAND in [0,1] into a powerlaw random number,
from f P(X)~X^A for 0 <= X0 <= X <= X1.  A, X0,
and X1 must be double-float.  The special case of A=-1 is fudged to
A=-0.99999 for simplicity of implementation, because the integral of
X^-1 is log(X), not a power law X^(A+1)."
  (declare (type (double-float 0d0) x0 x1 urand)
	   (type double-float a)
	   (optimize (speed 3) (safety 0)))
  (let* ((a (if (< (abs (- a -1d0)) 0.000001d0)
		-0.999999d0  ;; special case of A=1 is fudged
		a)) 
	 ;;
	 (a1 (+ a 1d0))
	 (x0a1 (expt x0 a1)) 
	 (x1a1 (expt x1 a1))
	 (c (- x1a1 x0a1))
	 (y (+ (* urand c) x0a1)))
    (declare (type double-float a a1 x0a1 x1a1 c y)
	     (optimize speed))
    (expt (the (double-float 0d0) y) (/ 1d0 a1))))


(defun powerlaw (a x0 x1 &key (uniform-random-state *uniform-random-state*))
  "Generate a realization of P(X)~X^A for 0 <= X0 <= X <= X1.  A, X0,
and X1 must be double-float.  The special case of A=-1 is fudged to
A=-0.99999 for simplicity of implementation, because the integral of
X^-1 is log(X), not a power law X^(A+1)."
  (declare (type (double-float 0d0) x0 x1)
	   (type double-float a)
	   (inline uniform  powerlaw-from-uniform)
	   (optimize speed))
  (powerlaw-from-uniform a x0 x1 (random:uniform uniform-random-state)))
    
    
(defun exponential-from-uniform (a x0 x1 urand)
    "Generate a realization of P(X)~exp(A*X) for  X0 <= X <= X1 from uniform
random URAND"
    (declare (type double-float a x0 x1 urand)
	     (inline uniform)
	     (optimize speed))
    ;; convert internally to double precision for larger range
    (if (= a 0d0)
	;; special case of zero exponent
	(+ x0 (* (- x1 x0) urand))
	;; otherwise true exponent
	(let* ((a  (float a 1d0))
	       (x0 (float x0 1d0))
	       (x1 (float x1 1d0))
	       (e0 (exp (* a x0)))
	       (e1 (exp (* a x1)))
	       (c (- e1 e0))
	       (y (+ (* c urand) e0))
	       (exprandom
		(/ (log y) a)))
	  (declare (type double-float a x0 x1 e0 e1 c)
		   (type (double-float (0d0)) y))
	  (float exprandom 1d0))))


(defun exponential (a x0 x1 &key (uniform-random-state *uniform-random-state*))
    "generate a realization of P(X)~exp(A*X) for  X0 <= X <= X1."
    (declare (type double-float a x0 x1)
	     (inline uniform)
	     (optimize speed))
    (exponential-from-uniform a x0 x1 (uniform uniform-random-state)))


(defun exponential10-from-uniform (a x0 x1 urand)
    "Generate a realization of P(X)~10^(A*X) for  X0 <= X <= X1 from uniform
random URAND"
    (exponential-from-uniform (* a #.(log 10d0)) x0 x1 urand))

(defun exponential10 (a x0 x1 &key (uniform-random-state *uniform-random-state*))
  "generate a realization of P(X)~10^(A*X) for  X0 <= X <= X1."
  (declare (type double-float a x0 x1)
	   (inline exponential)
	   (optimize speed))
  (exponential (* a #.(log 10d0)) x0 x1 :uniform-random-state uniform-random-state))
      
    


;; this attempt at implementing NR random function did not do
;; well because it involves 32bit*32bit which won't overflow in
;; lisp - NO, WAIT, THEY ONLY SQUARE THE LOWER 16 BITS OF THE NUMBER
;; SO IT CAN BE DONE IN LISP
;;
;; (defstruct ri-hash-struct
;;   (lword 0d0 :type (unsigned-byte 32))
;;   (irword 0d0 :type (unsigned-byte 32)))

;; (defun random-int (ri-hash-struct)
;;   (declare (type ri-hash-struct ri-hash-struct)
;; 	   (optimize speed))
;;   (loop
;;      with ia of-type (unsigned-byte 28) = 0
;;      with ib of-type (unsigned-byte 28) = 0
;;      with iswap of-type (unsigned-byte 28) = 0
;;      with itmpl of-type (unsigned-byte 28) = 0
;;      with itmph of-type (unsigned-byte 28) = 0
;;      with lword = (ri-hash-struct-lword ri-hash-struct)
;;      with irword = (ri-hash-struct-irword ri-hash-struct)
;;      for i of-type (unsigned-byte 20) below 4 ;; number of hashes
;;      do
;;        (setf iswap irword)
;;        (setf ia (logxor iswap (aref c1 i)))
;;        (setf itmp1 (logand ia #0xffff))
;;        (setf itmph (ash ia 16))
       
  
  




(defstruct poisson-random-state
  (lock (bordeaux-threads:make-lock "poisson-random-state"))
  (urs (make-uniform-random-state :lock nil)
   :type uniform-random-state)
  (oldm -1d0 :type double-float)
  (y -1d0 :type double-float)
  (sq -1d0 :type double-float)
  (alxm -1d0 :type double-float))


(defvar *poisson-random-state* (make-poisson-random-state))



(declaim (inline gammln))

(defun gammln (xx)
 (declare (type double-float xx)) 

 (prog ((cof #.(make-array 6 :element-type 'double-float :initial-contents
			   '(76.18009173d0 -86.50532033d0 24.01409822d0 
			     -1.231739516d0 0.120858003d-2 -0.536382d-5)))
        (stp 0d0) (half 0d0) (one 0d0) (fpf 0d0) 
        (tmp 0d0) (ser 0d0) (gammln 0d0) (x 0d0))

  (declare (type (simple-array double-float (*)) cof)) 
  (declare (type double-float stp half one fpf tmp ser x gammln)) 

  (setq stp  2.50662827465d0)

  (setq half 0.5d0 one 1.0d0 fpf 5.5d0) 
 
  (setf x (- xx 1d0)) 
  (setf tmp (+ x fpf)) 
  (setf tmp (- (* (+ x half) (log tmp)) tmp)) 
  (setf ser one) 
  (do ((j 0 (+ j 1)))
      ((> j 5) t)
    (declare (type (signed-byte 28) j))
    (setf x (+ 1d0 x))
    (setf ser (+ ser (/ (aref cof j) x)))) 

  (setf gammln (+ tmp (log (the (double-float (0d0)) (* stp ser)))))
  (return (the double-float gammln))))



(defun poisson (xm &key (poisson-random-state *poisson-random-state*))
  "Return a poisson deviate with expectation XM>=0, expressed as an
integer. Do not use for very large (>1d7) XM, because of internal
declarations that constrain magnitudes of numbers."
  (declare (type double-float xm)
	   (type poisson-random-state poisson-random-state)
	   (inline gammln uniform)
	   (optimize speed))
  (when (minusp xm) (error "XM<0 in POISSON"))
  ;;
  (when (poisson-random-state-lock poisson-random-state)
    (bordeaux-threads:acquire-lock (poisson-random-state-lock poisson-random-state)))
  ;;
  (let ((t0 0d0)
	(g 0d0)
	(em 0d0)
	(poidev 0d0)
	(urs (poisson-random-state-urs poisson-random-state)))
    (declare (type double-float t0 g em poidev))
    ;;
    (macrolet ((%floor (x)
		 `(nth-value 0 (floor (the (double-float -1d8 1d8) ,x))))
	       ;; rounding that takes integer value float to an integer; the built-in
	       ;; ROUND in sbcl tends to give compilation notes and non-inline calls
	       (%round-down (x) 
		 `(nth-value 0 (floor (the (double-float -1d8 1d8) (+ ,x 0.5d0))))))
      ;;
      (cond 
	((< xm 12)
	 (when 
	     (not (= xm 12d0))
	   (setf (poisson-random-state-oldm poisson-random-state) xm)
	   (setf g (exp (- xm))))
	 (setf em -1d0) 
	 (setf t0 1d0) 
	 (tagbody
	  label2 
	    (setf em (+ 1d0 em))
	    (setf t0 (* t0 (uniform urs)))
	    (if (> t0 g) (go label2))))
	
	(t 
	 (when 
	     (not (= xm (poisson-random-state-oldm poisson-random-state)))
	   (setf (poisson-random-state-oldm poisson-random-state) xm)
	   (setf (poisson-random-state-sq poisson-random-state) (sqrt (* 2d0 xm))) 
	   (setf (poisson-random-state-alxm poisson-random-state) (log xm))
	   (setf g (- (* xm (poisson-random-state-alxm poisson-random-state)) 
		      (gammln (+ 1d0 xm)))))
	 (tagbody
	  label1 
	    (setf (poisson-random-state-y poisson-random-state) 
		  (tan (* pi (uniform urs)))) 
	    (setf em (+ (* (poisson-random-state-sq poisson-random-state) 
			   (poisson-random-state-y poisson-random-state)) xm))
	    (if (< em 0d0) (go label1)) 
	    (setf em (float (%floor em) 1d0))
	    (setf t0 (* (* 0.9d0 
			   (+ 1d0 
			      (expt (poisson-random-state-y poisson-random-state) 2)))
			(exp
			 (- (- (* em (poisson-random-state-alxm poisson-random-state)) 
			       (gammln (+ em 1)))
			    g))))
	    (if (> (uniform urs) t0) (go label1))))) 
      (setf poidev em)
      ;;
      (when (poisson-random-state-lock poisson-random-state)
	(bordeaux-threads:release-lock (poisson-random-state-lock poisson-random-state)))
      ;;
      (%round-down poidev))))


  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shuffle-vector (v)
  "Destructively randomly shuffle vector V using modern Fisher-Yates method"
  (declare (type vector v))
  (loop for n of-type fixnum from (1- (length v)) downto 1
	for k of-type fixnum = (random (1+ n))
	do
	(rotatef (aref v n) (aref v k)))
  v)
	  
#+nil ;; don't really understand what a cyclic permutation is, so we leave this commented out
(defun shuffle-vector-cyclic (v)
  "destructively shuffle vector v using modern Sattolo method"
  (declare (type vector v))
  (loop for n of-type fixnum from (1- (length v)) downto 1
	for k of-type fixnum = (random n) ;; only change from shuffle-vector / Fisher-Yates
	do
	(rotatef (aref v n) (aref v k)))
  v)




(defun make-random-string (n &key (prefix "") (suffix ""))
  "generate a random string of length N from chars 0-9a-zA-Z with additional PREFIX and SUFFIX given"
  (let ((v "01234567890abcdefghijklmnopqurstuvwxyzABCDEFGHIJKLMNOPQURSTUVWXYZ")
	(str (make-string n)))
    (loop for i below n 
	  do (setf (aref str i)
		   (aref v (random (length v)))))
    (concatenate 'string prefix str suffix)))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fast gaussian using repeating set - good for generating noise 
;; over many points if correlation does not matter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defstruct fast-gaussian-struct 
  ;; a vector of gaussians
  (gvec (make-array 0 :element-type 'double-float)
	:type (simple-array double-float (*)))
  (n    0 :type (unsigned-byte 28)) ;; total number = (length gvec)
  (nptr 0 :type (unsigned-byte 28));; current index
  (nskip 1 :type (unsigned-byte 20)) 
  (kptr 0 :type (unsigned-byte 28))) ;; index in kvec

(defun build-fast-gaussian-struct
    (&key (n 10000) (gaussian-random-state *gaussian-random-state*))
  "Create a FAST-GAUSSIAN-STRUCT with N different values to be
  used by FAST-REPEATING-GAUSSIAN.  Not threadsafe."
  (when (< n 5000)
    (error "You should have N>5000"))
  (let ((fgs (make-fast-gaussian-struct 
	      :n n
	      :nptr 0
	      :gvec (make-array n :element-type 'double-float))))
    (loop 
       for i below n
       with v = (fast-gaussian-struct-gvec fgs)
       do 
	 (setf (aref v i) (random:gaussian gaussian-random-state)))
    fgs))

(declaim (inline fast-repeating-gaussian %reset-fgs))  


;; reset FAST-GAUSSIAN-STRUCT to a random starting point and a random skip
;; factor
(defun %reset-fgs (fgs)
  (declare (type fast-gaussian-struct fgs)
	   (optimize speed))
  (let ((n (fast-gaussian-struct-n fgs)))
  (setf (fast-gaussian-struct-nskip fgs)
	(+ 1 (floor (* (random 1.0) 0.001 n))))
  (setf (fast-gaussian-struct-nptr fgs) 
	(floor (* (random 1.0) 0.01 n)))))


(defun fast-repeating-gaussian (fast-gaussian-struct)
  "Given a FAST-GAUSSIAN-STRUCT made by BUILD-FAST-GAUSSIAN-STRUCT
get the next Gaussian variate from it.  This picks random
numbers in a fixed series, but semi-randomly selects the starting point
from which to begin picking to break some of the obvious correlations.
The random numbers probably contain various weird internal correlations, but
they are not really seen on 2d images formed from them."
  (declare (type fast-gaussian-struct fast-gaussian-struct)
	   (optimize speed)) 
  (prog1
      (aref (fast-gaussian-struct-gvec fast-gaussian-struct)
	    (fast-gaussian-struct-nptr fast-gaussian-struct))
    (incf (fast-gaussian-struct-nptr fast-gaussian-struct)
	  (fast-gaussian-struct-nskip fast-gaussian-struct))
    (when (>= (fast-gaussian-struct-nptr fast-gaussian-struct)
	     (fast-gaussian-struct-n fast-gaussian-struct))
      (%reset-fgs fast-gaussian-struct))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; structure for making correlated gaussian variables using
;; a covariance matrix
(defstruct corgauss 
  (n 0      :type (unsigned-byte 28))
  (yout nil :type (or null (simple-array double-float (*))))
  (vtmp nil :type (or null (simple-array double-float (*))))
  (sigmavec nil :type (or null (simple-array double-float (*))))
  (umatrix  nil :type (or null (simple-array double-float (* *)))))

(defun prepare-corgauss (covar)
  "Given a COVAR symmetric real coveriance matrix, create a 
CORGAUSS structure by diagonalizing the matrix; this can then
be passed to CORRELATED-GAUSSIAN to produce vectors of correlated
gaussian variables."
  (let ((covar (matrix:copy-matrix covar :output-type 'double-float))
	(n (array-dimension covar 0))
	eigenvals u)
    
    (loop for i below n 
	 do 
	 (loop for j from i below n
	      do
	      (when (not (= (aref covar i j)
			    (aref covar j i)))
		(error "COVAR is not symmetric"))))

    (multiple-value-setq (eigenvals u)
      (matrix:jacobi covar))
    ;;
    (loop for i below (length eigenvals) 
	 do (setf (aref eigenvals i)
		  (sqrt (aref eigenvals i))))
    
    (make-corgauss
     :n n
     :yout (make-array n :element-type 'double-float)
     :vtmp (make-array n :element-type 'double-float)
     :sigmavec eigenvals
     :umatrix u)))
     
    
    
;; this could also have been done using Cholesky
(defun correlated-gaussian (corgauss &optional (gaussian-random-state
						*gaussian-random-state*))
  "Given a corgauss made from a covariance matrix by PREPARE-CORGAUSS,
fill CORGAUSS's YOUT vector with correlated gaussian values, and return
YOUT.

If C is the covariance matrix, then P(x)~exp(-1/2 x^T C^-1 x)
and we have C=U D UT  where D contains the squared sigma in each
eigen-direction.    Hence C^-1=UT D^-1 U   and we can define
y=UT.x  so that the probability P(y)~exp(-1/2  y^T D^-1 y)
Then a realization of y is y=sqrt(D).s where s is a Gaussian vector.
and we have a realization of x as x=U.y=U.sqrt(D).s
"
  (declare (type corgauss corgauss))
  (let ((y (corgauss-yout corgauss))
	(vtmp (corgauss-vtmp corgauss))
	(n (corgauss-n corgauss))
	(u (corgauss-umatrix corgauss))
	(s (corgauss-sigmavec corgauss)))
    ;;
    (loop for i below n
	 do 
	 (setf (aref vtmp i) 
	       (* (aref s i)
		  (random:gaussian gaussian-random-state))))
    ;;
    ;; multiply vtmp (with random gaussians) by U and put into y
    (loop for i below n  ;; i loops down output
	 do
	 (setf (aref y i) 0d0)
	 (loop for j below n ;; j loops across array
	    do
	      (incf (aref y i)
		    (* (aref u i j) (aref vtmp j)))))
    ;;
    y))


(defun test-corgauss (a &key (niter 10000))
  "Run a test of corgauss, given a covariance matric covar; should
return something numerically close to the covariance matrix"
  (loop 
     with corgauss = (prepare-corgauss a)
     with n = (corgauss-n corgauss)
     with m = (make-array (list n n) :element-type 'double-float
			  :initial-element 0d0)
     for k below niter
     for y = (correlated-gaussian corgauss)
     do
       (loop for i below n
	    do 
	    (loop for j below n
	       do
		 (incf (aref m i j)
		       (/ (* (aref y i) (aref y j))
			  niter))))
     finally
       (return m)))
		 
	 
    
    
  


(defun pmran28 (vseed)
  "Uses a Park and Miller linear generator to make very
fast but medium quality pseudo-random numbers between 0 and 2^28;
Generates about 20 million random numbers per second.
VSEED is a vector containing one unsigned-byte 32."
  (declare (type (simple-array (unsigned-byte 32) (1)) vseed)
	   (optimize (speed 3) (safety 2))) ;; safety2 to check vseed

  (locally ;; this prevents consing in sbcl
      (declare (optimize (speed 3) (safety 0)))
    (let ((ia 16807)
	  (im 2147483647)
	  (iq 127773)
	  (ir 2836)
	  (mask 123459876)
	  (k 0)
	  (idum (aref vseed 0))
	  (ians 0))
      (declare (type (signed-byte 32) ia im ir mask idum)
	       (type (unsigned-byte 28) ians))
      
      (setf idum (logxor idum mask))
      (setf k (truncate idum iq)) ;; integer idum/iq 
      (setf idum
	    (the (signed-byte 32)
	      (- 
	       (the (signed-byte 32)
		 (* ia 
		    (the (signed-byte 32)
		      (- idum
			 (the (signed-byte 32) (* k iq))))))
	       (the (signed-byte 32)
		 (* ir k)))))
      (if (minusp idum) (incf idum im))
      (setf ians (logand idum #.(1- (expt 2 28))))
      (setf idum (logxor idum mask))
      (setf (aref vseed 0) idum)
      ians)))
   
(defun pmrann (n vseed)
  "Use Park and Miller generator to make a medium quality pseudorandom
number from 0..N-1, taking the result of iran28 modulo N, and retrying
several times if necessary if the result is in the last interval of N
in 0..2^28-1."
  (declare (type (simple-array (unsigned-byte 32) (1)) vseed)
	   (type (unsigned-byte 28) n)
	   (optimize speed))
  (loop 
     ;; the max value of n28 that is an even multiple of n
     with i28-max of-type (unsigned-byte 28) =
       (* n (truncate #.(1- (expt 2 28)) n))
     for itry of-type (unsigned-byte 28) from 0
     for i28 of-type (unsigned-byte 28) = (pmran28 vseed)
     when (or
	   (<= i28 i28-max)
	   (= itry 50)) ;; don't loop forever, but return a slightly bogus result
     do
       (return (nth-value 1 (truncate i28 n))))) 
	
(declaim (inline pmranf))
(defun pmranf (vseed)
  "Use Park and miller generator to make a medium quality but fast
random single float between 0.0 and 1.0."
  (declare (type (simple-array (unsigned-byte 32) (1)) vseed))
  (/ (the (unsigned-byte 28) (pmran28 vseed))
     #.(float (1- (expt 2 28)))))
     
     
