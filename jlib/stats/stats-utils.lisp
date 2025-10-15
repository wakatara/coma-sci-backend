

(in-package stats)



(defun seq-to-dbl-vec (seq)
  (map  
   '(simple-array double-float (*))
   (lambda (x) (float x 1d0))
   seq))


;; dovector is non-standard cmucl
(defmacro do-vector ((var vec) &body body)
  (let ((ivar (gensym)))
    `(let ((,var)) ;; should this go after dotimes?
       (dotimes (,ivar (length ,vec))
	 (setf ,var (aref ,vec ,ivar))
	 ,@body))))

(defmacro do-array ((var arr) &body body)
  (let ((ivar (gensym)))
    `(let ((,var)) ;; should this go after dotimes?
       (dotimes (,ivar (array-total-size ,arr))
	 (setf ,var (row-major-aref ,arr ,ivar))
	 ,@body))))

(defmacro do-elements ((var container) &body body)
  `(cond ((listp ,container)
	  (dolist (,var ,container) ,@body))
	 ((vectorp ,container)
	  (do-vector (,var ,container) ,@body))
         ((arrayp ,container)
	  (do-array (,var ,container) ,@body))
	 (T
	  (error "non list or vector or array in do-elements"))))

;; this is a legacy - already done by do-sequence
(defmacro do-sequence ((var sequence) &body body)
  `(cond ((listp ,sequence)
	  (dolist (,var ,sequence) ,@body))
	 ((vectorp ,sequence)
	  (do-vector (,var ,sequence) ,@body))
	 (T
	  (error "non list or vector in do-sequence"))))



  
(defmacro is-sequence (s)
  `(or (listp ,s) (vectorp ,s)))

;; (+ v1 v2 v3 ...)
(defun sum-of-elements (v)
  (if (not (is-sequence v))
      (error "Non array/list passed to sum-of-elements"))
  (let ((sum 0)) ;; start with integer - will convert to float automatically if needed
    (do-sequence (x v) (incf sum x))
    sum))

;; (/ (+ v1 v2 ...) N)
(defun mean-of-elements (v)
  (if (not (is-sequence v))
      (error "Non array/list passed to mean-of-elements"))
  (if (< (length v) 1)
      (error "Cannot compute mean of set with 0 elements"))
  (/ (sum-of-elements v) (length v)))


;; (sqrt (+ (* v1 v1) (* v2 v2) ..))
(defun sum-in-quadrature-of-elements (v)
  (if (not (is-sequence v))
      (error "Non array/list passed to quadrature-mean-of-elements"))
  (sqrt (sum-scatter-of-elements v 0.0d0)))

;; compute geometric mean using logarithms - negative values throw error
;; (expt (* v1 v2 ...) 1/N)
(defun geometric-mean-of-elements (v)
  (if (not (is-sequence v))
      (error "Non array/list passed to geometric-mean-of-elements"))
  (if (< (length v) 1)
      (error "Cannot compute geometric mean of set with 0 elements"))
  (let ((sumlogs 0.0d0)
	(zero nil))
    (do-sequence (x v)
		 (if (< x 0)
		     (error "negative number in geometric-mean-of-elements "))
		 (if (= x 0)
		     (setf zero t)
		   (incf sumlogs (log x))))
    (if zero
	0.0d0
      (exp (/ sumlogs (length v))))))


;; calculate scatter about mean, or about the number in mean
;; (+ (square (- v1 mean))  (square (- v2 mean)) ...)
(defun sum-scatter-of-elements (v &optional (mean nil))
  (if (not (is-sequence v))
      (error "Non array/list passed to scatter-of-elements"))
  (if (< (length v) 1)
      (error "Cannot compute scatter of set with 0 elements"))
  (setf mean (or mean (mean-of-elements v)))
  (let ((sum-of-squares 0))
    (do-sequence (x v) (incf sum-of-squares (* (- x mean) (- x mean))))
    sum-of-squares))

;; (/ (+ (square (- v1 mean))  (square (- v2 mean)) ...)    (- N 1))
(defun variance-of-elements (v)
  (if (not (is-sequence v))
      (error "Non array/list passed to variance-of-elements"))
  (if (< (length v) 1)
      (error "Cannot compute scatter of set with < 2 elements"))
  (/ (sum-scatter-of-elements v) (+ -1 (length v))))


;; sigma, the square root of the variance
;; (sqrt
;;   (/ (+ (square (- v1 mean))  (square (- v2 mean)) ...)    (- N 1)) )
(defun sigma-of-elements (v)
  (if (not (is-sequence v))
        (error "Non array/list passed to sigma-of-elements"))
  (sqrt (variance-of-elements v)))


;; maximum of one or more sequences - deprecated; use max-of-elements
(defun seq-max (&rest sequences)
  (let ((max nil))
    (dolist (s sequences)
      (do-sequence (x s) (setf max (if max (max max x) x))))	
    max))

(defun max-of-elements (&rest containers)
  (let ((max nil))
    (dolist (s containers)
      (do-elements (x s) (setf max (if max (max max x) x))))	
    max))



;; minimum of one or more sequences
(defun seq-min (&rest sequences)
  (let ((min nil))
    (dolist (s sequences)
      (do-sequence (x s) (setf min (if min (min min x) x))))	
    min))

(defun min-of-elements (&rest containers)
  (let ((min nil))
    (dolist (s containers)
      (do-elements (x s) (setf min (if min (min min x) x))))	
    min))


(defun %make-it-a-vector (thing)
  "Convert a THING (vector, list, array) into a 1d vector, preserving its array type."
  (cond ((vectorp thing) thing)
	((listp thing) (make-array (length thing) :initial-contents thing))
	((arrayp thing)
	 (loop with vec = (make-array (array-total-size thing) :element-type 
				      (array-element-type thing))
	       for i below (array-total-size thing)
	       do (setf (aref vec i) (row-major-aref thing i))
	       finally (return vec)))
	(t
	 (error "Cannot convert THING of type ~A to a vector" (type-of thing)))))


(defun %mode-of-vector (v &key (nwindow 5000))
  "Estimate the mode of a vector V by sorting V and finding the
narrowest window containing NWINDOW points  Destructively sorts V."
  (declare (type vector v)
	   (type (unsigned-byte 20) nwindow))
  (let ((vv (sort v '<))
	(ntot (array-total-size v)))
    (loop
       with max-density  = -1d0
       with ibest of-type (unsigned-byte 28) = 0
       with nw/2 of-type (unsigned-byte 20) = (ash nwindow -1)
       with nmin = nw/2 and nmax = (- ntot nw/2 1)
       for i of-type (unsigned-byte 28) from nmin to nmax
       for x1  = (aref vv (- i nw/2))
       for x2  = (aref vv (+ i nw/2))
       for density = (/ 1d0 (- x2 x1)) ;; to a scale factor
       when (> density max-density)
       do
	 (setf max-density density)
	 (setf ibest i)
       finally
	 (return (values (aref vv ibest) ibest)))))


(defun mode-of-elements (v &key (fwindow 0.05))
  "Estimate the mode of a set of elements by copying to a double float
vector, sorting, and running a box of count FWINDOW*NUM_ELEMENTS
over the sorted box, finding the maximum density.  The resolution
is one element.

WARNING: this does not work if the if the window is FWINDOW/2 
away from an edge."
  (let* ((vv (%make-it-a-vector v))
	 (nwindow (round (* fwindow (length vv)))))
    (nth-value 0 (%mode-of-vector vv :nwindow nwindow))))
  

	 




(defun median-of-elements (v)
  (setf v (%make-it-a-vector v))
  (let ((vv (sort (copy-seq v) #'<)))
    (if (oddp (length vv))
	(aref vv (/ (- (length vv) 1) 2))
      (/ (+ (aref vv (/ (length vv) 2))
	    (aref vv (+ -1 (/ (length vv) 2)))) 
	 2))))


(defun fraction-of-elements (v frac &optional is-sorted)
  "Return element X of sequence v such that fraction FRAC
of elements is smaller"
  (setf v (%make-it-a-vector v))
  (if (or (> frac 1.0d0) (< frac 0.0d0))
      (error "out of range frac in percentile"))
  (if (= (length v) 1)
      (aref v 0)
    (let* ((y (if is-sorted v (sort (copy-seq v) #'<)))
           (n (length y))
           (f (* 1.0d0 (1- n) frac)))
      (multiple-value-bind (bot ff) (floor f)
        (when (= bot (1- n))
          (decf bot)
          (setf ff 1.0d0))
        (+ (* (- 1.0d0 ff) (aref y bot)) 
           (* ff (aref y (1+ bot))))))))

(defun percentile-of-elements (&rest args)
  (error "PERCENTILE-OF-ELEMENTS is obsolete; use FRACTION-OF-ELEMENTS."))


#+sbcl(declaim (sb-ext:maybe-inline 
		fast-double-float-vec-median
		wirth-index-of-kth-smallest
		find-index-of-next-largest-or-equal-element))
#+cmu (declaim (ext:maybe-inline fast-double-float-vec-median
				 wirth-index-of-kth-smallest
				 find-index-of-next-largest-or-equal-element))




(defun wirth-index-of-kth-smallest (v k &optional nv)
"Using.Wirth's algorithm, return index J of double-float array V such that
K elements are smaller than (AREF V J) - note that V IS REARRANGED so that
the index J returned is the index in the new, rearranged array - in truth, 
this rearranges V and always returns the input index K.  NV is the optional
length of v to use - by default, NV is set to (length V) "
    (declare (type (simple-array double-float (*)) v)
             (type (unsigned-byte 28) k)
	     (type (or null (unsigned-byte 28)) nv)
             (optimize speed))
    (let ((i 0) (j 0) (l 0) (m 0) (x 0d0)
	  (n (or nv (length v))))
      (declare (type (unsigned-byte 28) i j l m n)
               (type double-float x))
      (when (>= k n)
        (error "k=~D is too big; must be <~D" k n))
      (setf m (1- n))
      (loop
         while (< l m)
         do
           (setf x (aref v k))
           (setf i l)
           (setf j m)
           (loop
              do
                (loop while (< (aref v i) x) do (incf i))
                (loop while (< x (aref v j)) do (decf j))
                (when (<= i j)
                  (rotatef (aref v i) (aref v j))
                  (incf i)
                  (decf j))
              while 
                (<= i j))
           (when (< j k) (setf l i))
           (when (< k i) (setf m j)))
      ;;
      k))


(defun find-index-of-next-largest-or-equal-element (v k nv)
  "Given an index k in double-float array v, return the index
of the next largest or equal element.  NV is length of
V to use"
  (declare (type (simple-array double-float (*)) v)
	   (type (unsigned-byte 28) k)
	   (type (or null (unsigned-byte 28)) nv)
	   (optimize speed))
  (loop
     with n of-type (unsigned-byte 28) = (or nv (length v))
     with ibest of-type (unsigned-byte 28) = k ;;default value - nothing bigger
     with x of-type double-float = (aref v k)
     with xbest of-type double-float = most-positive-double-float
     with xx of-type double-float = 0d0
     for i below n
     when (not (= i k))
     do
       (setf xx (aref v i))
       (when (and (>= xx x) (< xx xbest))
	 (setf xbest xx)
	 (setf ibest i))
     finally (return ibest)))




(defun fast-double-float-vec-median (v &optional nv)
  "quickly return the median of a vector V using Wirth's method -
NV is the optional length of V to use, by default (length V).
WARNING - this rearranges V"
  (declare (type (simple-array double-float (*)) v)
	   (type (or null (unsigned-byte 28)) nv)
	   (optimize speed))
  (let ((n (or nv (length v)))
	(k1 0)
	(k2 0))
    (declare (type (unsigned-byte 28) k1 k2))
    (cond ((= n 0)
	   (error "Cannot take median of a set of 0 elements"))
	  ((= n 1)
	   (aref v 0))
	  ((oddp n)
	   (aref v (wirth-index-of-kth-smallest v  (ash n -1) nv)))
	  (t
	   (setf k1 (wirth-index-of-kth-smallest v (1- (ash n -1)) nv))
	   (setf k2 (find-index-of-next-largest-or-equal-element v k1 nv))
	   (* 0.5d0 (+ (aref v k1) (aref v k2)))))))
	   
	   
(defun fast-double-float-vec-fraction (v frac &optional nv)
  "quickly return the value X in double float vector V such that
fraction FRAC is smaller than X - uses Wirth's method -
NV is the optional length of V to use, by default (length V). -
WARNING - this rearranges V"
  (declare (type (simple-array double-float (*)) v)
	   (type double-float frac)
	   (optimize speed))
  (if (or (> frac 1.0d0) (< frac 0.0d0))
      (error "out of range frac in percentile"))
  (let* ((n (or nv (length v)))
	 (f (* 1.0d0 (1- n) frac))
	 (ff 0d0)
	 (bot 0)
	 (k1 0)
	 (k2 0))
    (declare (type (unsigned-byte 28) k1 k2 bot)
	     (type double-float f ff))
    (cond ((= n 0)
	   (error "Cannot take fraction of a set of 0 elements"))
	  ((= n 1)
	   (aref v 0))
	  (t
	   (multiple-value-setq (bot ff) (floor f))
	   (setf k1 (wirth-index-of-kth-smallest v bot nv))
	   (setf k2 (find-index-of-next-largest-or-equal-element v k1 nv))
	   (+ (* (- 1.0d0 ff) (aref v k1)) 
	      (* ff (aref v k2)))))))
	   	   
	 


;; calculate the best estimate and error of a set of numbers "v" with gaussian
;; errors sigma-v; returns (values mean err-of-mean chi-sqr prob-of-chi-sqr)
;;;                                                        
(defun best-mean-and-sigma (v v-err)
  "Given sequences V and V-ERR, return the best (gaussian optimal)
mean and errors. Returns: the following values:
  mean uncertainty-of-mean chi-squared probability-of-chi-squared"
  (if (listp v) (setf v (map 'vector #'(lambda (x) x) v))) 
  (if (listp v-err) (setf v-err (map 'vector #'(lambda (x) x) v-err)))
  (if (not (and (vectorp v) (vectorp v-err) (= (length v) (length v-err))))
      (error "Bad arguments to stats:best-mean-and-sigma"))
  (let* ((best-mean 0.0d0)
	 (sigma-mean 0.0d0)
	 (chi-sqr 0.0d0)
	 (p-chi-sqr 0.0d0)
	 (n-tot (length v))
	 (n-max (+ -1 n-tot)))
    (do ((s1 0.0d0)(s2 0.0d0)(tmp 0.0d0)(i 0 (+ 1 i)))   
        ((> i n-max) (setf best-mean (/ s1 s2)) )
      (setf tmp (aref v-err i)) 
      (setf tmp (/ 1.0 (* tmp tmp)))
      (setf s1 (+ s1 (* tmp (aref v i))))
      (setf s2 (+ s2 tmp)))
    ;; calculate chi squared
    (do ((tmp 0) (i 0 (+ 1 i)))
        ((> i n-max))
      (setf tmp (/ (- (aref v i) best-mean) (aref v-err i)))
      (setf chi-sqr (+ chi-sqr (* tmp tmp))))
    ;; calculate the sigma of the mean
    (do ((tmp 0) (sum 0) (i 0 (+ 1 i)))
        ((> i n-max)  (setf sigma-mean (sqrt (/ 1.0 sum))))
      (setf tmp (aref v-err i))
      (setf sum (+ sum (/ 1.0 (* tmp tmp)))))

    ;; number of dimensions is number of points -1 
    (setf p-chi-sqr 
          (if (> n-tot 1)  ;; if there is one point, p(chisqr)=1
              (- 1.0d0 (chi-square-probability n-max chi-sqr))
	    1.0))
;    (setf p-chi-sqr :p-chi-sqr-not-computed-yet)
    (values best-mean sigma-mean chi-sqr p-chi-sqr)))



(defun hodges-lehman-estimator/dbl (v &key (vscratch nil))
  "Compute the Hodges-Lehman estimator for a vector of double floats.

Hodges-Lehman is a superior alternative to the median as a estimator
for the central value of a population.  For documentation, see (eg)
Wikipedia and references therein.

V is a vector of N double-float inputs
VSCRATCH is an optional scrath vector of N*(N-1)/1 double floats.
"
  (declare (type (simple-array double-float (*)) v)
	   (type (or null (simple-array double-float (*))) vscratch)
	   (optimize (speed 3) (safety 1)))
  (let* ((nv (length v))
	 ;; length of scratch vector
	 (nscr (cond ((> nv #.(expt 2 24))
		      (error "Input vector too long."))
		     ((= nv 0)
		      (error "Input vector has zero length."))
		     (t
		      (ash (the fixnum (* nv (1- nv))) -1))))
	 (vscr (or vscratch (make-array nscr :element-type 'double-float))))
    (declare (type (integer 1 #.(ash most-positive-fixnum -3)) nv) 
	     (type fixnum nscr)) 
    (when (< (length vscr) nscr)
      (error "VSCRATCH is too short"))
    (loop with k of-type fixnum = 0
	  for i of-type fixnum from 0 below nv
	  do (loop for j from (1+ i) below nv
		   do (setf (aref vscr k)
			    (* 0.5d0 (+ (aref v i) (aref v j))))
		      (incf k)))
    (fast-double-float-vec-median vscr nscr)))
			    
(defun hodges-lehman-estimator (v)
  "Compute the Hodges-Lehman estimator for a sequence of reals.
Hodges-Lehman is a superior alternative to the median as a estimator
for the central value of a population.  For documentation, see (eg)
Wikipedia and references therein."  
  (hodges-lehman-estimator/dbl (seq-to-dbl-vec v)))
  
							    

	
;; certain common uncertainties
(defun uncertainty-a/b (a da b db)
  "compute uncertainty in a/b given uncertainties da,db in a,b"
  (declare (type real a da b db))
  (sqrt (+ (expt (/ da b) 2)
	   (expt (/ (* a db) (* b b)) 2)))) 


(defun uncertainty-a*b (a da b db)
  "compute uncertainty in a*b given uncertainties da,db in a,b"
  (declare (type real a da b db)) 
  (sqrt (+ (expt (* a db) 2) 
	   (expt (* b da) 2)))) 

(defun add-in-quadrature (&rest xlist) 
  "add arguments x_i in quadrature, returning
   (sqrt (+ (expt x_1 2) (expt x_2 2) ... ))" 
  (loop for x in xlist
	do
	(if (not (realp x)) (error "~A not real in add-in-quadrature" x))
	sum (* x x) into summed-squares of-type double-float
	finally (return (sqrt summed-squares))))
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun fisher-prob-combine (prob-seq)
  "Given a sequence of probabilities in PROB-SEQ, representing the
p values in [0,1] of independent tests, combine them using Fisher's
method into a single p value representing a meta-score for the
ensemble."
  ;;
  (loop 
     with v = (seq-to-dbl-vec prob-seq)
     initially ;; any zero prob produces a zero prob output 
       (when (find 0d0 v :test 'eql) (return 0d0))
     for p of-type double-float across  v
     sum (* -2d0 (log p)) into chisqr of-type double-float
     finally (return (complementary-chi-square-probability
		      (* 2 (length v)) ;; 2 N degrees of freedom
		      chisqr))))



(defun sigma-from-quartiles (q25 q75)
  "Give quartiles q25 and q75, estimate the sigma assuming a Gaussian
distribution."
  (/ (- q75 q25) 1.3489795003921632d0))
					
