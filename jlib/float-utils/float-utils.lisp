
#|

define some cross-implementation floating point utilities, like
infinity and NaN

This is not defined for CLISP, which apparently has no NaN and Inf,
so the tests will never happen

|#


 
(defpackage float-utils
  (:use #:cl)

  (:export
   ;; WARNING - these are just representative values and may not be unique.
   ;; testing whether a number is equal to one of these infinities does not
   ;; prove that it isn't infinite
   #:+single-float-positive-infinity+
   #:+single-float-negative-infinity+
   #:+double-float-positive-infinity+
   #:+double-float-negative-infinity+
   #:+single-float-nan+
   #:+double-float-nan+
   ;; variable (not constant) forms of NaN as well - NaN constants can
   ;; trigger compilation exceptions, so these are preferable for most
   ;; uses
   #:*single-float-nan*
   #:*double-float-nan*
   ;;
   #:double-float-infinity-p
   #:single-float-infinity-p
   #:float-infinity-p
   #:double-float-nan-p  ;; does not include infinity
   #:single-float-nan-p  ;; does not include infinity
   #:float-nan-p         ;; does not include infinity
   #:double-float-nan-or-infinity-p
   #:single-float-nan-or-infinity-p
   #:float-nan-or-infinity-p
   ;;
   #:safe-single-float=
   #:safe-double-float=
   ;;
   #:with-float-traps-masked
   ))

(in-package float-utils)

#-(or sbcl ccl abcl ecl clisp)
(error "FLOAT-UTILS is defined only for SBCL,CCL,ABCL, and dummy for CLISP")

#-clisp
(declaim (type single-float
	       +single-float-positive-infinity+
	       +single-float-negative-infinity+
	       +single-float-nan+
	       *single-float-nan*))
#-clisp
(declaim (type double-float
	       +double-float-positive-infinity+
	       +double-float-negative-infinity+
	       +double-float-nan+
	       *double-float-nan*))

	 
	       


(defconstant +single-float-positive-infinity+
  #+sbcl  sb-ext:single-float-positive-infinity
  #+ccl   +1e++0
  #+abcl  extensions:single-float-positive-infinity
  #+ecl ext:single-float-positive-infinity
  #+clisp nil
  #-(or sbcl ccl abcl ecl clisp)
  (error
   "single float positive infinity is undefined for this Lisp implementation."))
 

(defconstant +single-float-negative-infinity+
  #+sbcl  sb-ext:single-float-negative-infinity
  #+ccl   -1e++0
  #+abcl extensions:single-float-negative-infinity
  #+ecl ext:single-float-negative-infinity
  #+clisp nil
  #-(or sbcl ccl abcl ecl clisp)
  (error
   "single float negative infinity is undefined for this Lisp implementation."))

 

(defconstant +double-float-positive-infinity+
  #+sbcl  sb-ext:double-float-positive-infinity
  #+ccl   +1e++0
  #+abcl  extensions:double-float-positive-infinity
  #+ecl ext:double-float-positive-infinity
  #+clisp nil
  #-(or sbcl ccl abcl ecl clisp)
  (error
   "double float positive infinity is undefined for this Lisp implementation."))


(defconstant +double-float-negative-infinity+
  #+sbcl  sb-ext:double-float-negative-infinity
  #+ccl   -1e++0
  #+abcl extensions:double-float-negative-infinity
  #+ecl ext:double-float-negative-infinity
  #+clisp nil
  #-(or sbcl ccl abcl ecl clisp)
  (error
   "double float negative infinity is undefined for this Lisp implementation."))
 

(defconstant +single-float-nan+
  #+sbcl (sb-int:with-float-traps-masked
	     (:invalid :overflow :underflow :inexact :divide-by-zero)
	   (- sb-ext:single-float-positive-infinity 
	      sb-ext:single-float-positive-infinity))
  #+ccl  +1E+-0 
  #+abcl (- extensions:single-float-negative-infinity
	    extensions:single-float-negative-infinity)
  #+ecl (float (ext:nan) 1.0)
  #+clisp nil
  #-(or sbcl ccl abcl ecl clisp)
  (error "single-float-nan is undefined for this lisp implementation."))




(defconstant +double-float-nan+
  #+sbcl  (sb-int:with-float-traps-masked
	      (:invalid :overflow :underflow :inexact :divide-by-zero)
	    (- sb-ext:double-float-positive-infinity 
	       sb-ext:double-float-positive-infinity))
  #+ccl +1D+-0 
  #+abcl (- extensions:double-float-negative-infinity
	    extensions:double-float-negative-infinity)
  #+ecl (float (ext:nan) 1d0)
  #+clisp nil
  #-(or sbcl ccl abcl ecl clisp)
  (error "single-float-nan is undefined for this lisp implementation."))



;; define a variable in addition to a constant, because compiling a
;; NaN constant can generate exceptions, but a variable is evaluated
;; at runtime (and protectable against exceptions)
(defvar *single-float-nan* +single-float-nan+)
(defvar *double-float-nan* +double-float-nan+)


(declaim (inline 
	  double-float-infinity-p double-float-nan-p double-float-nan-or-infinity-p
	  single-float-infinity-p single-float-nan-p single-float-nan-or-infinity-p
	  float-infinity-p  float-nan-p float-nan-or-infinity-p
	  safe-single-float= safe-double-float=))
	  
(declaim (type double-float 
	       +double-float-positive-infinity+ +double-float-negative-infinity+
	       +double-float-nan+)
	 (type single-float 
	       +single-float-positive-infinity+ +single-float-negative-infinity+
	       +single-float-nan+))
	 

(defun double-float-infinity-p (x)
  "Test whether a double-float is infinite."
  (declare (type double-float x)
	   (optimize speed))
  #+sbcl (locally
	     (declare (inline sb-ext:float-infinity-p))
	   (sb-ext:float-infinity-p x))
  ;; ccl is tricky because 
  #+ccl  (or (< x most-negative-double-float)  ;; CHECKME
	     (> x most-positive-double-float))
  #+abcl (system:float-infinity-p x)
  #+ecl (ext:float-infinity-p x)
  #+clisp nil)

 
(defun single-float-infinity-p (x)
  "Test whether a single-float is infinite."
  (declare (type single-float x)
	   (optimize speed))
  #+sbcl (locally 
	     (declare (inline sb-ext:float-infinity-p))
	   (sb-ext:float-infinity-p x))
  #+ccl  (or (< x most-negative-single-float)  ;; CHECKME - seems to work
	     (> x most-positive-single-float))
  ;; can't us (sb-ext:float-infinity-p x) because it boxes in sbcl
  #+abcl (system:float-infinity-p x)
  #+ecl (ext:float-infinity-p x)
  #+clisp nil)

(defun float-infinity-p (x)
  "Test whether a float is infinite."
  (declare (type (or single-float double-float) x))
  #+sbcl (locally 
	     (declare (inline sb-ext:float-infinity-p))
	   (sb-ext:float-infinity-p x))
  #+ccl  (or (< x most-negative-single-float)  ;; CHECKME - seems to work
	     (> x most-positive-single-float))
  ;; can't us (sb-ext:float-infinity-p x) because it boxes in sbcl
  #+abcl (system:float-infinity-p x)
  #+ecl (ext:float-infinity-p x)
  #+clisp nil)

 
(defun double-float-nan-p (x) 
  "Test whether a double-float is NaN (but 
returns NIL for infinity)."
  (declare (type double-float x)
	   (optimize speed))
  #+sbcl (locally 
	     (declare (inline sb-ext:float-nan-p))
	   (sb-ext:float-nan-p x))
  #+ccl  (and (ccl::nan-or-infinity-p x)
	      (not (float-infinity-p x)))
  #+abcl (SYSTEM:FLOAT-NAN-P x)
  #+ecl (ext:float-nan-p x)
  #+clisp nil)

(defun single-float-nan-p (x) 
  "Test whether a single-float is NaN (but 
returns NIL for infinity)."
  (declare (type single-float x)
	   (optimize speed))
  #+sbcl (locally 
	     (declare (inline sb-ext:float-nan-p))
	   (sb-ext:float-nan-p x))
  #+ccl  (and (ccl::nan-or-infinity-p x)
	      (not (float-infinity-p x)))
  #+abcl (SYSTEM:FLOAT-NAN-P x)
  #+ecl (ext:float-nan-p x)
  #+clisp nil)

(defun float-nan-p (x)
  "Test whether a single or double-float is NaN (but 
returns NIL for infinity)."
  (declare (type (or single-float double-float) x))
  #+sbcl (locally 
	     (declare (inline sb-ext:float-nan-p))
	   (sb-ext:float-nan-p x))
  #+ccl  (and (ccl::nan-or-infinity-p x)
	      (not (float-infinity-p x)))
  #+abcl (SYSTEM:FLOAT-NAN-P x)
  #+ecl (ext:float-nan-p x)
  #+clisp nil)

#|


NOTE: it is easier to check for both NaN and Inf, than either, because
the significand can be ignored.


Thus we can make a simutaneous test for NaN and Inf more efficient by using
sb-kernel::single-float-bits and using the fact that both Inf and NaN
has an exponent of #xFF, and Inf has a significand of 23 0 bits (plus
a sign).

NaN can have any bits in the significand.  If the first bit is '1' then
it is a quiet NaN, and if 0 it is a signalling NaN.

The following seems to work on little-endian x86, but there are no
big endian machines to test it on. But endian-ness should not matter because
integers used for bitwise comparison have the same endian-nes.

They are about half the instructions of using offial NaN plus InF tests.

The first bit is the sign, the next 8 bits are the exponent, and the 23 others
are the significant.  .(ash #xff 23)  is  0.1111.1111.[23 zeros], matching
the exponent.

(defun sbcl-single-float-is-nan-or-inf (x)
   (declare (type single-float x)
            (optimize (speed 3) (safety 0)))
   (= (logand #.(ash #xff 23) (sb-kernel::single-float-bits x))
       #.(ash #xff 23)))


;; the double float version has 11 bits in the exponent, after the sign

(defun sbcl-double-float-is-nan-or-inf (x)
   (declare (type double-float x)
            (optimize (speed 3) (safety 0)))
   (= (logand #.(ash #b11111111111 52) (sb-kernel::double-float-bits x))
      #.(ash #b11111111111 52)))


|#


#+nil
(defun double-float-nan-or-infinity-p (x)
  "Tests whether a double-float is NaN or Inf."
  (declare (type double-float x)
	   (optimize speed))
  #+sbcl ;; test for all 1s in the 11 bits after sign
  (let ((ebits  #.(ash #b11111111111 52)))
    (= (logand ebits (sb-kernel::double-float-bits x))
       ebits))
  #-sbcl
  (or (double-float-nan-p x)
      (double-float-infinity-p x)))

;; it seems some sbcls don't have sb-kernel:double-float-bits but
;; only high/low bits.  It's probably faster using just the 32
;; high bits anyway.
(defun double-float-nan-or-infinity-p (x)
  "Tests whether a double-float is NaN or Inf."
  (declare (type double-float x)
	   (optimize speed))
  #+sbcl ;; test for all 1s in the 11 bits after sign
  (let ((ebits  #.(ash #b11111111111 #.(- 52 32))))
    (= (logand ebits (sb-kernel:double-float-high-bits x))
       ebits))
  #-sbcl
  (or (double-float-nan-p x)
      (double-float-infinity-p x)))

(defun single-float-nan-or-infinity-p (x)
  "Tests whether a single-float is NaN or Inf."
  (declare (type single-float x)
	   (optimize speed))
  #+sbcl ;; test for all 1s in 8 bits after sign
  (let ((ebits #.(ash #xff 23)))
    (= (logand ebits (sb-kernel::single-float-bits x))
       ebits))
  #-sbcl
  (or (single-float-nan-p x)
      (single-float-infinity-p x)))

(defun float-nan-or-infinity-p (x)
  "Tests whether a double or single-float is NaN or Inf."
  (declare (type (or single-float double-float) x))
  (cond ((typep x 'single-float)
	 (single-float-nan-or-infinity-p x))
	(t
	 (double-float-nan-or-infinity-p x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test these on load because we don't quite trust these optimized routines
;; on all architectures
#+sbcl
(eval-when (:load-toplevel)
  (when (not (and (single-float-nan-or-infinity-p *single-float-nan*)
		  (single-float-nan-or-infinity-p +single-float-positive-infinity+)
		  (single-float-nan-or-infinity-p +single-float-negative-infinity+)
		  (not (single-float-nan-or-infinity-p 3.1415e20))
		  (not (single-float-nan-or-infinity-p most-positive-single-float))
		  (not (single-float-nan-or-infinity-p most-negative-single-float))
		  (not (single-float-nan-or-infinity-p least-positive-single-float))
		  (not (single-float-nan-or-infinity-p least-negative-single-float))))
    (error "FAILURE OF ENHANCED SBCL SINGLE-FLOAT-NAN-OR-INFINITY-P"))
  ;;
  (when (not (and (double-float-nan-or-infinity-p *double-float-nan*)
		  (double-float-nan-or-infinity-p +double-float-positive-infinity+)
		  (double-float-nan-or-infinity-p +double-float-negative-infinity+)
		  (not (double-float-nan-or-infinity-p 3.1415d100))
		  (not (double-float-nan-or-infinity-p most-positive-double-float))
		  (not (double-float-nan-or-infinity-p most-negative-double-float))
		  (not (double-float-nan-or-infinity-p least-positive-double-float))
		  (not (double-float-nan-or-infinity-p least-negative-double-float))))
    (error "FAILURE OF ENHANCED SBCL DOUBLE-FLOAT-NAN-OR-INFINITY-P")))
    
	      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun safe-single-float= (x y)
  "Test if two floats are equal, but don't throw errors on NaN; 
   we define NaN=NaN; 
             +Infinity = +Infinity.
             -Infinity = -Infinity."
  (declare (type single-float x y))
  (cond ((single-float-nan-p x) ;; NaN=NaN
	 (single-float-nan-p y))
	;;
	((single-float-infinity-p x) ;; NaN=NaN, respecting the sign
	 (and (single-float-infinity-p y)
	      (if (plusp x)
		  (plusp y)
		  (minusp y))))
	;; 
	(t ;; just a number
	 (= x y))))

(defun safe-double-float= (x y)
  "Test if two floats are equal, but don't throw errors on NaN; 
   we define NaN=NaN; 
             +Infinity = +Infinity.
             -Infinity = -Infinity."
  (declare (type double-float x y))
  (cond ((double-float-nan-p x) ;; NaN=NaN
	 (double-float-nan-p y))
	;;
	((double-float-infinity-p x) ;; NaN=NaN, respecting the sign
	 (and (double-float-infinity-p y)
	      (if (plusp x)
		  (plusp y)
		  (minusp y))))
	;; 
	(t ;; just a number
	 (= x y))))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-float-traps-masked ((&key all invalid inexact overflow underflow 
				      divide-by-zero) &body body )
  "Mask float traps with keywords 
     :ALL :INVALID :INEXACT :OVERFLOW :UNDERFLOW :DIVIDE-BY-ZERO"
  #+sbcl
  `(sb-int:with-float-traps-masked
       ,(remove nil
		(list (if (or inexact   all)  :inexact)
		      (if (or invalid   all)  :invalid)
		      (if (or overflow  all)  :overflow)
		      (if (or underflow all)  :underflow)
		      (if (or divide-by-zero all) :divide-by-zero)))
     ,@body)
  ;;
  #+ccl
  (let ((old-fpu-mode-var (gensym "fpu-mode")))
    `(let ((,old-fpu-mode-var (ccl:get-fpu-mode)))
	(unwind-protect
	     (progn (ccl:set-fpu-mode :overflow ,(or overflow all)
				      :underflow ,(or underflow all)
				      :division-by-zero ,(or divide-by-zero all)
				      :invalid ,(or invalid all)
				      :inexact ,(or inexact all))
		    ,@body)
	  (apply #'ccl:set-fpu-mode ,old-fpu-mode-var))))
  ;;
  #-(or ccl sbcl)
  (declare (ignore all invalid inexact overflow underflow 
		   divide-by-zero))
  #-(or ccl sbcl ecl)
  (progn
    (format t "Warning - float-utils.lisp (with-float-traps-masked-macro) -
No float trap masking defined for this lisp implementation.  Not masking.~%")
    `(progn ,@body)))
     
			  
			  
