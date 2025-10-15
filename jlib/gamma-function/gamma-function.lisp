
(defpackage #:gamma-function
  (:use #:common-lisp 
	#+cmu #:alien #+cmu #:c-call
	#+sbcl #:sb-alien #+sbcl #:sb-c-call)
  (:export #:log-gamma
	   #:gamma
	   #:log-gamma-complex-double
	   #:log-gamma-real-double
	   #:gamma-real-double #+cmu #:log-gamma-native #+cmu #:gamma-native
	   #:beta-function
	   #:incomplete-beta-function
	   #:incomplete-gamma-function
	   #:complementary-incomplete-gamma-function
	   #:erf
	   #:erfc
	   ))

(in-package gamma-function)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  calculate the log of the gamma function using Lanczo's method, as
;;  described in "The Special Functions and their Approximations",
;;  Yudell Luke, 1969, Academic Press
;;
;;  this routine is (cross fingers) valid throughout the complex plane, 
;;  except where Im(z) is so big that exp(z) has indeterminate phase
;;  (same problem as trying to calculate sin(x) for x so big that you
;;   don't know exactly how many times Pi goes into it)
;;  and constrains Im(log-gamma(x)) to be in the range (-Pi,Pi]
;;
;;  Other formulae exist (eg, Stirling's) , but they don't converge 
;;  nicely everywhere and require shifting to get them to 
;;  work for all z
;;
;;
;;  let G(x) be the Gamma function
;;
;;  for an integer "s", and for Re(z+s+1/2)>0:
;;
;;                                         z+1/2
;;        G(z+1) = Sqrt(2 Pi) (z + s + 1/2)     exp[-(z + s + 1/2)] *
;;
;;                      N-1
;;                    \-----
;;                     \
;;                     /      g(k) H(k,z)
;;                    /_____
;;                     k=0
;;
;;
;;
;;               H(k+1,z)       z-k
;;       where  -----------  = ------     and   H(0,z) = 1
;;                H(k,z)        z+k+1
;;
;;                  
;;                
;;       and g(k) is given in Mathematica notation as:
;;
;;
;;     g[k_,s_] := A(k) (2 Pi)^-0.5 Exp[s] (-1)^k  Sum[(-1)^r Binomial[k,r] 
;;                 Product[k+rr,{rr,0,r-1}] (E/(r+s+1/2))^(r+1/2) , {r,0,k}] 
;;
;;                where A(0)=1,  A(k)=2 for k>1
;;       
;;
;;
;;   Luke suggests that s=5, N=11 should give accuracy comparable to the 
;;   one part in 10^15 of double precision.  We will use s=5, N=12.
;;
;;   Stirling's asymptotic expansion is simpler for Re(x)>0, |z|>15
;;   or so, but convergence is mediocre inside |z|<10.   One improvement
;;   might to be have the asymptotic version available for large
;;   |z|, but the version below is simple enough not to bother.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(declaim (#+sbcl sb-ext:maybe-inline 
	  #+cmucl ext:maybe-inline
	  #-(or cmucl sbcl) inline
	  log-gamma gamma log-gamma-complex-double))



;; unfortunately complex LOG and EXP don't inline 
(defun log-gamma-complex-double (z)
  (declare (type  (complex double-float) z)
	   (optimize speed))

  (flet ((pos-real-log-gamma (z)  ;; function to return LogGamma(z) for Re(z)>0
         (declare (type  (complex double-float) z)
		  (optimize speed))
          (let* ((zz (- z #C(1.d0 0.0d0)))  ; because this returns log-gamma(Z+1)
		 (H #C(1.d0 0.0d0)) ;;(/ zz (+ zz 1.0d0)))  ;; first H term
		 (sum  #C(4.162443691643908d1 0.0d0)))  ;; load the 0th and 1st terms
	    (declare (type (complex double-float) zz H sum))
	    ;;
	    ;; the following sum could be expressed more simply as a loop, but we unroll it
	    ;; because the coefficient array would then have to be transferred into a simple array
	    ;; for efficienty
	    (setq H (* H (/ (- zz 0.0d0) (+ zz 1.d0))))	    (setq sum (+ sum (* H -5.122424102237478d1)))
	    (setq H (* H (/ (- zz 1.0d0) (+ zz 2.d0))))	    (setq sum (+ sum (* H 1.133875581348898d1)))
	    (setq H (* H (/ (- zz 2.0d0) (+ zz 3.d0))))	    (setq sum (+ sum (* H -7.477326877723884d-1)))
	    (setq H (* H (/ (- zz 3.0d0) (+ zz 4.d0))))	    (setq sum (+ sum (* H 8.7828774930613d-3)))
	    (setq H (* H (/ (- zz 4.0d0) (+ zz 5.d0))))	    (setq sum (+ sum (* H -1.899030263766831d-6)))
	    (setq H (* H (/ (- zz 5.0d0) (+ zz 6.d0))))	    (setq sum (+ sum (* H 1.946334554456852d-9)))
	    (setq H (* H (/ (- zz 6.0d0) (+ zz 7.d0))))	    (setq sum (+ sum (* H -1.993452308865567d-10)))
	    (setq H (* H (/ (- zz 7.0d0) (+ zz 8.d0))))	    (setq sum (+ sum (* H 8.43318428062705d-12)))
	    (setq H (* H (/ (- zz 8.0d0) (+ zz  9.d0))))    (setq sum (+ sum (* H 1.486111394972753d-12)))
	    (setq H (* H (/ (- zz 9.0d0) (+ zz 10.d0))))    (setq sum (+ sum (* H -8.06056050283971d-13)))
	    (setq H (* H (/ (- zz 10.0d0) (+ zz 11.d0))))   (setq sum (+ sum (* H 2.925949721659105d-13)))
	    (setq H (* H (/ (- zz 11.0d0) (+ zz 12.d0))))   (setq sum (+ sum (* H -1.0218297851058d-13)))
	    ;; additional terms are 3.665253931602144d-14 and -1.373290025386337d-14 but really
	    ;; terms beyond first 9 do not contribute
	    
             (- (+ (log sum)
		   0.91893853320467274d0   ;; 0.5 log(2 Pi)
		   (* (+ zz #C(0.5d0 0.0d0)) (log (+ zz  #C(5.5d0 0.0d0)))))
		(+ zz #C(5.5d0 0.0d0)))))
        ;;
	 (log-sin-pi-z  (z);; function to return log(sin(PI z))
            (declare (type  (complex double-float) z)
		     (optimize speed)
		     (inline exp log))
	     (if (< (imagpart z) 0.d0)
		 (+ (* #C(0.0d0 3.14159265358979323846d0) z) 
		    #C(-0.69314718055994531d0 0.0d0)
		    #C(0.0d0 -1.570796326794897d0)
		    (log (- #C(1.0d0 0.0d0)  (exp (* #C(0.d0 -6.283185307179587d0) z)))))
	       (+ (* #C(0.0d0 -3.14159265358979323846d0) z) 
		  #C(-0.69314718055994530942d0 0.0d0)
		  #C(0.0d0  -1.570796326794897d0)
		  (log (- (exp (* #C(0.d0 6.283185307179587d0) z)) #C(1.d0 0.0d0) ))))))
    ;;
    (declare (ftype (function ((complex double-float)) (complex double-float))  pos-real-log-gamma log-sin-pi-z))
    
	 ;;
	 (let* ((unfixed-log-gamma ;; log(gamma(z)) with Im part unconstrained
		 (if (> (realpart z) 0.d0)   ;;  if in the right 1/2 of complex plane
		     (pos-real-log-gamma z) ;;   apply above formula
		   ;; otherwise, reflect using Gamma(z)=PI/[Gamma(1-z) sin(PI z)]
		   (- #C(1.1447298858494002d0 0.d0) ;; log(Pi)  
		      (pos-real-log-gamma (- #C(1.d0 0.d0)  z))
		      (log-sin-pi-z z))))
		(r-part (realpart unfixed-log-gamma))
		(i-part (imagpart unfixed-log-gamma)))
	   (declare (type (complex double-float) unfixed-log-gamma)
		    (type (double-float -1d10 1d10) i-part) ;; because final log-gamma is meaningless
	                                                    ;; unless imaginary part has resolution of <<PI
		    (type double-float r-part))
	   ;;
	   ;; now fix the logarithm by making the imaginary part be in (-Pi,Pi]
	   (if (= i-part 0.0d0)
	       (complex r-part 0.d0)  ;; not unfixed-log-gamma, because im part might be -0.d0
	     ;; else put the imaginary part in (-Pi, Pi].
	     (progn
	       (setq i-part  ;; put i-part in [0,2Pi)
		     (- i-part 
			(* (floor (/ i-part 6.28318530717958647693d0)) 
			   6.28318530717958647693d0)))
	       (if (> i-part 3.14159265358979323846d0)
		   (setq i-part (- i-part 6.28318530717958647693d0)))
	       ;; now perform a final fix -- if input z was real, imaginary part of output log-gamma must be 0 or
	       ;; exactly PI -- so we get rid of residual noise in the imaginary part
	       (if (= 0.d0 (imagpart z))
		   (setq i-part (if (< (abs i-part) 0.1d0) 0.d0 3.14159265358979323846d0)))	   
	       ;;
	       (complex r-part i-part))))))


(defun log-gamma (z)
  (declare (type number z))
  (log-gamma-complex-double (coerce z '(complex double-float))))

(defun gamma (z)
  (let ((g (exp (log-gamma z))))
    (declare (type  (complex double-float) g))
    (if (realp z) (realpart g) g)))
      


(defmacro pos-log (z) ;; log that isn't paranoid about imaginary result
  `(log (the (double-float #.least-positive-double-float #.most-positive-double-float) ,z)))






(declaim (inline log-gamma-real-double gamma-real-double))
(declaim (ftype (function (double-float) double-float) log-gamma-real-double))
(declaim (ftype (function (double-float) double-float) gamma-real-double))

;; returns (values abs-log-gamma-of-z sign-log-gamma-of-z)
(defun log-gamma-real-double (z)
  (declare (type double-float z)
	   (optimize speed))

  (flet ((pos-real-log-gamma (z)  ;; function to return LogGamma(z) for Re(z)>0
         (declare (type  double-float z)
		  (optimize speed))
          (let* ((zz (- z 1.d0))  ; because this returns log-gamma(Z+1)
		 (H 1.d0 ) ;;(/ zz (+ zz 1.0d0)))  ;; first H term
		 (sum  4.162443691643908d1))  ;; load the 0th and 1st terms
	    (declare (type double-float zz H sum))
	    ;;
	    ;; the following sum could be expressed more simply as a loop, but we unroll it
	    ;; because the coefficient array would then have to be transferred into a simple array
	    ;; for efficienty
	    (setq H (* H (/ (- zz 0.0d0) (+ zz 1.d0))))	    (setq sum (+ sum (* H -5.122424102237478d1)))
	    (setq H (* H (/ (- zz 1.0d0) (+ zz 2.d0))))	    (setq sum (+ sum (* H 1.133875581348898d1)))
	    (setq H (* H (/ (- zz 2.0d0) (+ zz 3.d0))))	    (setq sum (+ sum (* H -7.477326877723884d-1)))
	    (setq H (* H (/ (- zz 3.0d0) (+ zz 4.d0))))	    (setq sum (+ sum (* H 8.7828774930613d-3)))
	    (setq H (* H (/ (- zz 4.0d0) (+ zz 5.d0))))	    (setq sum (+ sum (* H -1.899030263766831d-6)))
	    (setq H (* H (/ (- zz 5.0d0) (+ zz 6.d0))))	    (setq sum (+ sum (* H 1.946334554456852d-9)))
	    (setq H (* H (/ (- zz 6.0d0) (+ zz 7.d0))))	    (setq sum (+ sum (* H -1.993452308865567d-10)))
	    (setq H (* H (/ (- zz 7.0d0) (+ zz 8.d0))))	    (setq sum (+ sum (* H 8.43318428062705d-12)))
	    (setq H (* H (/ (- zz 8.0d0) (+ zz  9.d0))))    (setq sum (+ sum (* H 1.486111394972753d-12)))
	    (setq H (* H (/ (- zz 9.0d0) (+ zz 10.d0))))    (setq sum (+ sum (* H -8.06056050283971d-13)))
	    (setq H (* H (/ (- zz 10.0d0) (+ zz 11.d0))))   (setq sum (+ sum (* H 2.925949721659105d-13)))
	    (setq H (* H (/ (- zz 11.0d0) (+ zz 12.d0))))   (setq sum (+ sum (* H -1.0218297851058d-13)))
	    ;; additional terms are 3.665253931602144d-14 and -1.373290025386337d-14 but really
	    ;; terms beyond first 9 do not contribute
	    ;;
	    (- (+ (pos-log sum)
		  0.91893853320467274d0   ;; 0.5 log(2 Pi)
		  (* (+ zz 0.5d0) (pos-log (+ zz  5.5d0))))
	       (+ zz 5.5d0 0.0d0)))))
    (declare (ftype  (function (double-float) double-float) pos-real-log-gamma)
	     (inline pos-real-log-gamma))
    (if (>= z 0.0d0)
	(values (pos-real-log-gamma z) 1.0d0)
      (let* ((sin-pi-z (sin (the double-float  (* pi z))))
	     (sign (cond ((plusp sin-pi-z) 1.d0)
			 ((minusp sin-pi-z) 1.d0)
			 (t (error "at singularity in gamma function")))))
	(declare (type double-float sin-pi-z sign))
	(setf sin-pi-z (abs sin-pi-z))
	(values (- 1.1447298858494002d0  (pos-real-log-gamma (- 1.0d0 z))  (pos-log sin-pi-z)) sign)))))


(defun gamma-real-double (z)
  (multiple-value-bind (lg sign)
      (log-gamma-real-double z)
    (declare (type double-float lg sign))
    (* sign (exp lg))))



;; the native versions of erf/erfc
#+(or cmu sbcl)
(#+cmu def-alien-variable #+sbcl define-alien-variable "signgam" int)
#+(or cmu sbcl) 
(#+cmu def-alien-routine #+sbcl define-alien-routine
       ("lgamma" %lgamma) double (x-dbl double))
#+(or cmu sbcl) (declaim (inline %lgamma log-gamma-native gamma-native)) 
#+(or cmu sbcl)
(defun log-gamma-native (x-dbl) 
  "log(Gamma(x)) using native lgamma - returns
 (values (log (abs gamma))  sign-gamma)" 
  (declare (type double-float x-dbl)  
	   (optimize speed)) 
  (let* ((lgam (alien-funcall
		(extern-alien "lgamma" (function double double))
		x-dbl))
	 (sign-gamma signgam))
    (declare (type double-float lgam)
	     (type (integer -1 1) sign-gamma))
    (values lgam sign-gamma)))

#+(or cmu sbcl)
(defun gamma-native (x-dbl)
  "compute Gamma(x) using native lgamma"
  (declare (type double-float x-dbl)  
	   (optimize speed))
  (multiple-value-bind (lgam sign) (log-gamma-native x-dbl)
    (* sign (exp lgam))))




;; the beta function, defined as beta(a,b)=Gamma(a) Gamma(b) / Gamma(a+b)
;;   or log(beta(a,b)) = log(Gamma(a))+log(Gamma(b))-log(Gamma(a+b))

(in-package gamma-function)

(declaim (inline beta-function))
(defun beta-function (a b)
  (declare (ftype (function (double-float) double-float) log-gamma-real-double)
	   (type double-float a b)
	   (optimize (speed 3) (safety 0)))
  (exp (- (+ (log-gamma-real-double a) (log-gamma-real-double b))
          (log-gamma-real-double (+ a b)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calculates a continued fraction function using modified Lenz's rule.
;;
;; specifically, returns  f   = b(0) + a(1)
;;                                     -----------------------
;;                                     b(1) + a(2)
;;                                            -----------------
;;                                            b(2) + a(3)
;;                                                   -----------
;;                                                   ...   
;; a-func and b-func are functions that take the step number
;; as arguments.
;;  
;; tiny-guess is an optional number used in the computation; it is required
;; to be much less than b-func, and is set to 1e-30 by default.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline continued-fraction))
(declaim (ftype (function ((function (fixnum) double-float)
			   (function (fixnum) double-float)
			   &optional double-float)
			  double-float)
		continued-fraction))
	  

(defun continued-fraction (a-func b-func &optional (tiny-guess 1.0d-30))
  (declare (type (function (fixnum) double-float)
		 a-func b-func)
	   (type double-float tiny-guess))
  ;;
  (let* ((tiny tiny-guess)
         (a-j 0.0d0)
         (epsilon 1.0d-15) ;; floating point precision (assume double)
         (b-j (funcall b-func 0))
         (f-j-1 (if (= 0.0d0 b-j) tiny b-j))
         (f-j 0.0d0)  
         (c-j-1 f-j-1)
         (c-j 0.0d0)
         (d-j-1 0.0d0)
         (d-j 0.0d0)
         (delta-j 0.0d0)
         (j 1))
    (declare (type double-float tiny a-j epsilon
		   b-j f-j-1 f-j c-j-1 c-j d-j-1 d-j delta-j)
	     (type (integer 0 #.(floor most-positive-fixnum 2)) j))
    (labels
        ((iter-func ()
		    (setf a-j (funcall a-func j))
		    (setf b-j (funcall b-func j))
		    (setf d-j (+ b-j (* a-j d-j-1)))
		    (if (= d-j 0.0d0) (setf d-j tiny))
		    (setf c-j (+ b-j (/ a-j c-j-1)))
		    (if (= c-j 0.0d0) (setf c-j tiny))
		    (setf d-j (/ 1.0 d-j))
		    (setf delta-j (* c-j d-j))
		    (setf f-j (* f-j-1 delta-j))
		    ;; set the old values to be the present ones
		    (setf f-j-1 f-j) (setf c-j-1 c-j) (setf d-j-1 d-j) 
		    (setf j (+ j 1))
		    (if (< (abs (- delta-j 1.0)) epsilon) ;; have we converged?
			f-j
		      (iter-func))))
      (iter-func))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; calculates the incomplete beta function via continued fractions
;; in the range x=[0,1]
(declaim (inline incomplete-beta-function))

(defun incomplete-beta-function (a b x)
  "This is the REGULARIZED incomplete beta function, defined as B(x;a,b)/B(a,b) -
it is equal to 1 for x=1"
  (declare (type double-float a b x)
	   (optimize speed))
  (if (or (< x 0.0d0) (> x 1.0d0))
      (error "x not in range [0,1] in (incomplete-beta-function a b x)"))
  ;; depending on where x is relative to a&b, decide which side of the
  ;; equation I_x(a,b) = 1-I_x(b,a) we calculate
  (let ((aa a) (bb b) (which-side -1) (xx x)
	(leading-term 0.0d0) (cont-frac-term 0.0d0))
    (declare (type double-float aa bb xx leading-term cont-frac-term)
	     (type (integer -1 1) which-side )) ;; -1 is left, 1 is right
    (labels 
     ((a-func (mm) ;; function that returns the "a" coeff in
	           ;; in the continued fraction; the "a" coeff
                   ;; is different for odd/even mm
	      (if (oddp mm)
		  (let ((m (- (/ mm 2.0d0) 0.5d0)))
		    (/ (* -1.0d0 (+ aa m) (+ aa bb m) xx)
		       (* (+ aa m m) (+ aa m m 1.0d0))))
		(let ((m (/ mm 2.0d0)))
		  (/ (* m (- bb m) xx)
		     (* (+ aa m m -1.0d0) (+ aa m m))))))
      (b-func (mm) (declare (ignore mm))  1.0d0))
     (declare (ftype (function (fixnum) double-float) a-func b-func)
	      (inline a-func b-func))
     (if (> x (/ (+ a 1.0d0) (+ a b 2.0))) ;; use the right side if
	 ;;  x > (a+1)/(a+b+2)
	 (progn (setf aa b) (setf bb a) (setf xx (- 1.0d0 x))
		(setf which-side 1)))
     ;; set the leading term to xx^aa * (1-xx)^bb / (aa * Beta(aa,bb))
     (if (or (= x 0.0d0) (= x 1.0d0))
	 (setf leading-term 0.0d0) ;; to avoid singularity, not needed with normal
                                   ;;  expt rather than hacked one
       (setf leading-term (/ (* (expt (the (double-float 0.0d0) xx) aa)
				(expt (the (double-float 0.0d0) (- 1.0d0 xx)) bb))
			     (* aa (the double-float (beta-function aa bb))))))
     (setf cont-frac-term (/ 1.0d0 (the double-float (continued-fraction #'a-func #'b-func))))
     (if (= which-side -1)
	 (* leading-term cont-frac-term)
       (- 1.0d0 (* leading-term cont-frac-term))))))





(declaim (inline incomplete-gamma-small-x
		 incomplete-complementary-gamma-large-x))

(defun incomplete-gamma-function (a x)
  (declare (type double-float a x))
  (check-type a double-float)
  (check-type x double-float)
  (locally
   (declare (optimize (speed  3) (safety 0)))
   (if (< x 0.0d0) (error "negative x in incomplete-gamma-function"))
   (if (< x (+ a 1.0d0))
       (the double-float (incomplete-gamma-small-x a x))
     (- 1.0d0 (the double-float (incomplete-complementary-gamma-large-x a x))))))

(defun complementary-incomplete-gamma-function (a x)
  (declare (type double-float a x))
  (if (< x 0.0d0) (error "negative x in complementary-incomplete-gamma-function"))
  (if (< x (+ a 1.0d0))
      (- 1.0d0 (incomplete-gamma-small-x a x))
      (incomplete-complementary-gamma-large-x a x)))

(defun incomplete-gamma-small-x (a x)
  (declare (type double-float a x)
	   (optimize (speed 0)))
  (when (zerop x) (return-from incomplete-gamma-small-x 0d0))
  (* ;(exp (- x)) (expt x a)
   (exp (-  (* a (pos-log x)) x))   ;; calculating this way avoids overflow
   (loop :with j fixnum = 0
	 :with g double-float = (the double-float (gamma-real-double (+ a 1.0d0)))
	 :with xx double-float = 1.0d0
	 :with sum double-float = 0.0d0
	 :with this-term double-float = 1.0d0
	 :until (or (> j 200)
		    (and (> j 0) (< (/ this-term sum) 1.0e-15)))
	 :finally (return sum)
	 :do (progn (setf this-term (/ xx g))
		    (incf sum this-term)
		    (setf g (* g (+ a 1.0d0 j)))
		    (setf xx (* x xx))
		    (incf j)))))
    
(defun incomplete-complementary-gamma-large-x  (a x)
  (declare (type double-float a x)
	   (optimize (speed 3)))
  (* (exp (- (* a (pos-log x)) x))  
     (/ 1.0d0 (gamma-real-double a))
     (- (continued-fraction (lambda (n)
			      (declare (type (unsigned-byte 28) n))
			      (coerce (if (= n 1) 1d0 (* (- 1 n) (- n 1 a)))
				      'double-float))
			    (lambda (n) (declare (type fixnum n))
			      (coerce (+ x (- (* 2 n) 1) (- a))
				      'double-float)))
	(- x a 1.0d0))))



       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; error function, using incomplete gamma function, or using
;; built-in erf via FFI if possible
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+cmu (declaim (inline erf erfc))
#+cmu (def-alien-routine ("erf" erf) double (x-dbl double))
#+cmu (def-alien-routine ("erfc" erfc) double (x-dbl double))
#+sbcl (declaim (inline erf erfc))
#+sbcl (define-alien-routine ("erf" erf) double (x-dbl double))
#+sbcl (define-alien-routine ("erfc" erfc) double (x-dbl double))


#-(or cmu sbcl)
(defun erf (x)
  (declare (type double-float x))
  (if (> x 0.0d0)
      (incomplete-gamma-function 0.5d0 (* x x))
      (- (incomplete-gamma-function 0.5d0 (* x x)))))
#-(or cmu sbcl)
(defun erfc (x)
  (declare (type double-float x))
  (if (> x 0.d0)
      (complementary-incomplete-gamma-function 0.5d0 (* x x))
      (+ 1.0 (incomplete-gamma-function 0.5d0 (* x x)))))


  





