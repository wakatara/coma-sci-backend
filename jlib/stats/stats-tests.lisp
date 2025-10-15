

(in-package stats)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; kolmogorov-smirnov routines


(defun %dbl-vec-is-sorted (v)
  (declare (type (simple-array double-float (*)) v)
	   (optimize speed))
  (loop for i from 0 to (- (length v) 2)
       when (> (aref v i) (aref v (1+ i))) 
       do (return nil)
       finally (return t)))

(defun to-double-vec-and-sort-< (seq)
  "Convert a sequence to a double-float vec and sort it, unless
it is already so, in which case just return it"
  (when (and (typep seq '(simple-array double-float (*)))
	     (%dbl-vec-is-sorted seq))
    (return-from to-double-vec-and-sort-< seq))
  ;;
  (let ((vv (make-array (length seq) :element-type 'double-float)))
    (cond ((listp seq)
	   (loop for x in seq for i from 0
	      do (setf (aref vv i) (coerce x 'double-float))))
	  ((vectorp seq)
	   (loop for x across seq  for i from 0
	      do (setf (aref vv i) (coerce x 'double-float)))))
    (locally (declare (type (simple-array double-float (*)) vv))
      (sort vv #'<))))

(defun ks-prob-func (x)
  (declare (type double-float x)
	   (optimize speed))
  (let ((-2xx (* -2.0d0 x x))
        (sum 0.0d0) )
    (declare (type double-float -2xx sum))
    (if (< x 0.1d0)
        1d0
        (progn  ;; begin at i=2 because of the late modification of exp-term
          (do ((i 2 (+ 1 i))  (exp-term -2xx) (sign 1 (* sign -1)) )
              ((< exp-term -400d0)) ;; we've converged (long ago)
	    (declare (type double-float exp-term)
		     (type (integer -1 1) sign)
		     (type (unsigned-byte 28) i))
            (setf sum (+ sum (* sign (exp exp-term))))
            (setf exp-term (* -2xx i i)))
          (* 2.0d0 sum)))))


;; NOTE - we verified that this gives the same D value as 
;; the NR-LISP translation of the code, and it gives
;; the same probability as an online KS-test
;; the histogram of the probability distribution is NOT flat, however, but
;; neither is it for the NR translation 
(defun ks-test--sequences (v1 v2)
  "perform ks test of sequence v1 against v2"
  (setf v1 (to-double-vec-and-sort-< v1))
  (setf v2 (to-double-vec-and-sort-< v2))
  (let* ((n1 (length v1))
         (n2 (length v2))
         (sqrt-n12 (sqrt (/ (* 1d0 n1 n2) (+ n1 n2))))
         (max-d 0.0d0)
         (cumul-dist-1 0.0d0)
         (cumul-dist-2 0.0d0)
         (tmp1 0.0d0) (tmp2 0.0d0))

    (locally
	(declare (type (simple-array double-float (*)) v1 v2)
		 (type (unsigned-byte 28) n1 n2)
		 (type double-float sqrt-n12 max-d cumul-dist-1 cumul-dist-2
		       tmp1 tmp2)
		 (optimize speed))
      ;;
      (do ((i1 0)  
	   (i2 0)) 
	  ((or (= i1 n1) (= i2 n2)))
	(declare (type (unsigned-byte 28) i1 i2))
	(setf tmp1 (aref v1 i1))
	(setf tmp2 (aref v2 i2))
	(if (<= tmp1 tmp2)
	    (progn
	      (setf i1 (+ i1 1))
	      (setf cumul-dist-1 (/ (float i1 1d0) n1))))
	(if (<= tmp2 tmp1)
	    (progn
	      (setf i2 (+ i2 1))
	      (setf cumul-dist-2 (/ (float i2 1d0) n2 1.0d0))))
	(setf tmp1 (abs (- cumul-dist-1 cumul-dist-2)))
	(if (> tmp1 max-d)
	    (setf max-d tmp1))))
    #+nil
    (format t "Callng ks-prob-func with ~A~%"
	    (* max-d (+ sqrt-n12 0.12 (/ 0.11 sqrt-n12))))
    (values
     (ks-prob-func (* max-d (+ sqrt-n12 0.12 (/ 0.11 sqrt-n12))))
     max-d)))

(defun ks-test--vectors (v1 v2)
  "Deprecated name for ks-test--sequences"
  (ks-test--sequences v1 v2))


(defun ks-test--function (v cumulative-prob-func)
  "perform ks test of v against cumulative-prob-func"
  (setf v (to-double-vec-and-sort-< v))
  (let* ((d 0.0d0) (dt 0.0d0)  (n (length v))  (sqrt-n (sqrt n))
         (ff 0.0d0) (fn 0.0d0) (fo 0.0d0) (step (/ 1.0d0 n)))
    (dotimes (i n)
      (setf fn (+ fn step))
      (setf ff (funcall cumulative-prob-func (aref v i)))
      (setf dt (max (abs (- fo ff)) (abs (- fn ff))))
      (if (> dt d) (setf d dt))
      (setf fo fn))
    (values
     (ks-prob-func (* d (+ sqrt-n 0.12 (/ 0.11 sqrt-n))))
     d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kuiper (circular) variant of KS
(defun kuiper-prob-func (x) ;; see equation 14.3.14 in NR
  (declare (type double-float x)
	   (optimize speed))
  (if (< x 0.4d0)
      1d0 ;; to seven figures according to NR
      (loop 
       with xx of-type double-float = (* x x)
       with sum of-type double-float = 0d0
       for j of-type (unsigned-byte 28) from 1 to 200 
       for jj of-type double-float = (* 1d0 j j)
       for lead-term of-type double-float = (- (* 4d0 jj xx) 1d0)
       for exp-term of-type double-float = (* -2d0 jj xx)
       do (incf sum (* lead-term (exp exp-term)))
       until (< exp-term -400) ;; should really have better convergence criterion
       finally (return (* 2d0 sum)))))

;; tested against IDL routine kuipertwo.pro
(defun kuiper-test--sequences (v1 v2)
  "perform Kuiper variant of KS test of sequence v1 against v2. The p
values produced are not uniform, so this test is useful only for
small-p cases, which are the important ones."
  (setf v1 (to-double-vec-and-sort-< v1))
  (setf v2 (to-double-vec-and-sort-< v2))
  (let* ((n1 (length v1))
         (n2 (length v2))
         (sqrt-n12 (sqrt (/ (* n1 n2) (+ n1 n2))))
         (max-dp 0.0d0)
         (max-dm 0.0d0)
	 (d 0d0)
	 (tmp1 0d0) (tmp2 0d0)
         (cumul-dist-1 0.0d0)
         (cumul-dist-2 0.0d0))
    ;;
    (do ((i1 0)  
         (i2 0)) 
        ((or (= i1 n1) (= i2 n2)))
      (setf tmp1 (aref v1 i1))
      (setf tmp2 (aref v2 i2))
      (if (<= tmp1 tmp2)
          (progn
            (setf i1 (+ i1 1))
            (setf cumul-dist-1 (/ (float i1 1d0) n1))))
      (if (<= tmp2 tmp1)
          (progn
            (setf i2 (+ i2 1))
            (setf cumul-dist-2 (/ (float i2 1d0) n2))))

      (setf max-dp (max max-dp (- cumul-dist-1 cumul-dist-2)))
      (setf max-dm (max max-dm (- cumul-dist-2 cumul-dist-1))))
    ;;
    (setf d (+ max-dp max-dm))
    ;;
    (values
     (kuiper-prob-func (* d (+ sqrt-n12 0.155d0 (/ 0.24d0 sqrt-n12))))
     d)))

(defun kuiper-test--vectors (v1 v2)
  "Deprecated name for kuiper-test--sequences"
  (kuiper-test--sequences v1 v2))


(defun kuiper-test--function (v cumulative-prob-func)
  "perform Kuiper variant of KS test of v against cumulative-prob-func. The p values
produced do appear flat but with a small bias to p~1, unlike the 
very  choppy distribution for the 2-vector Kuiper test."
  (setf v (to-double-vec-and-sort-< v))
  (let* ((dp 0.0d0) (dm 0.0d0)   ;; D+ and D- in NR notation
	 (dp+ 0.0d0) (dp- 0.0d0) ;; their local best values above/below 
	 (dm+ 0.0d0) (dm- 0.0d0) ;; this discrete point
	 ;;
	 (d 0d0) 
	 (n (length v))  (sqrt-n (sqrt n))
         (ff 0.0d0) (fn 0.0d0) (fo 0.0d0) (step (/ 1.0d0 n)))
    (dotimes (i n)
      (setf fn (+ fn step))
      (setf ff  (funcall cumulative-prob-func (aref v i)))
      (setf dp (max dp
		    (- fo ff)
		    (- fn ff)))
      (setf dm (max dm
		    (- ff fo)
		    (- ff fn)))
      (setf fo fn))
    ;;
    (setf d (+ dp dm))  ;; D is V in NR notation
    (values
     (kuiper-prob-func (* d (+ sqrt-n 0.155d0 (/ 0.24d0 sqrt-n))))
     d)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chisqr-bin-count-prob (bins1 bins2 &key (normalize nil)
			      (err1 t) (err2 t) (npar 0))
  "Given counts (possibly not integers) in sequences BINS1 and BINS2,
compute (VALUES PROB-CHISQR CHISQR N-DEG-OF-FREEDOM).   

If NORMALIZE is set (NIL by default), then constrain both sets to have
the same total counts.  The number of degrees of freedom falls by 1 if
NORMALIZE is set.

If ERR1 and ERR2 are set, then assume the bins to have errors given by
the square root of the counts.

NPAR is the number of parameters used in the history of these data; 
the number of degrees of freedom is decreased by NPAR when computing
the probability."
  
  (when (not (or err1 err2))
    (error "One of ERR1 or ERR2 must be T - otherwise, infinite denominator in chisqr"))
  (when (not (= (length bins1) (length bins2)))
    (error "BINS1 and BINS2 not of equal length"))
  
  (let* ((bins1 (seq-to-dbl-vec bins1))
	 (bins2 (seq-to-dbl-vec bins2))
	 (np (+ npar (if normalize 1 0)))
	 (ndof (- (length bins1) np))
	 (ntot1 (loop for x across bins1 sum x of-type double-float))
	 (ntot2 (loop for x across bins2 sum x of-type double-float)))
    ;;
    (loop 
     with r1 = (if normalize (/ 1d0 ntot1) 1d0) ;; rescaling factor
     with r2 = (if normalize (/ 1d0 ntot2) 1d0)
     with chisqr = 0d0
     for i below (length bins1)
     for x1 of-type double-float = (* r1 (aref bins1 i))
     for x2 of-type double-float = (* r2 (aref bins2 i))
     for e1 = (if err1 (* r1 (sqrt (aref bins1 i))) 0d0)
     for e2 = (if err2 (* r2 (sqrt (aref bins2 i))) 0d0)
     when (not (and (zerop x1) (zerop x2))) ;; ignore points with zero in both
     do (incf chisqr (/ (expt (- x1 x2) 2) 
			(+ (expt e1 2) (expt e2 2))))
     finally
     (return (values 
	      (complementary-chi-square-probability ndof chisqr)
	      chisqr
	      ndof)))))
  



