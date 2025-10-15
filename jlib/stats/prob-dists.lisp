

(in-package stats)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; probability distributions

(defun cumulative-gaussian-probability (x1 x2 &key (x0 0d0) (sigma 1d0))
  (let ((y1 (/ (- x1 x0) sigma))
	(y2 (/ (- x2 x0) sigma)))
    ;;
    (if 
     (< (* y1 y2) 0)  
     ;; if y1, y2 straddle zero, ok to use erf
     (abs
      (-
       (* 0.5d0 (the double-float
		  (gamma-function:erf (* y1 -0.5d0 1.4142135623730951d0 ))))
       (* 0.5d0 (the double-float
		  (gamma-function:erf (* y2 -0.5d0 1.4142135623730951d0))))))
     ;; otherwise, use erfc to avoid roundoff error
     (abs
      (-
       (* 0.5d0 (the double-float
		     (gamma-function:erfc (* y1 0.5d0 1.4142135623730951d0))))
       (* 0.5d0 (the double-float
		  (gamma-function:erfc (* y2 0.5d0 1.4142135623730951d0)))))
      ))))


(defun compute-two-sided-gaussian-z-value-for-prob (p)
"Compute X such that the integral of a gaussian from -X to X is P.   Should not be trusted
for P very close to 0 or 1."
  (declare (type (real 0 1) p))
  (let ((pp (float p 1d0)))
    (cond ((= pp 1d0)
	   10000d0)
	  ((= pp 0d0)
	   10000d0)
	  (t
	   (bisection-root:findroot   
	    (lambda (x) (- (cumulative-gaussian-probability (- x) x) pp))
	    -100d0 100d0
	    1d-11)))))

(defun compute-one-sided-gaussian-z-value-for-prob (p)
"Compute X such that the integral of a gaussian from -Infinity to X is P. Should not be trusted
for P very close to 0 or 1."
  (declare (type (real 0 1) p))
  (let ((pp (float p 1d0)))
    (cond ((= pp 1d0)
	   10000d0)
	  ((= pp 0d0)
	   10000d0)
	  (t
	   (bisection-root:findroot   
	    (lambda (x) (- (cumulative-gaussian-probability -100d0 x) pp))
	    -100d0 100d0
	    1d-11)))))




(defun gaussian-probability (x &key (x0 0d0) (sigma 1d0))
  (* 
   (/ 1d0 sigma)
   0.398942280401433d0 (exp (* -0.5d0 (expt (/ (- x x0) sigma)
					      2)))))


(declaim (inline %log-factorial log-factorial))

(defun %log-factorial (n)
  (declare ((integer 0 #.most-positive-fixnum) n)
	   (optimize (speed 3) (safety 0)))
  (let ((sum 0.0d0))
    (declare (double-float sum))
    (loop for i fixnum from 2 to n do
      (incf sum (+ (the double-float (log (* 1.0d0 i))))))
    sum))

;; a version of log-factorial that remembers the first 1000, for speed
(defvar *lf-vec*
  (make-array 1000 :element-type 'double-float
		   :initial-contents (loop for i below 1000
					   collect (%log-factorial i))))
(declaim (type (simple-array double-float (1000)) *lf-vec*))

(defun log-factorial (n)
  (declare ((integer 0 #.most-positive-fixnum) n)
	   (optimize (speed 3) (safety 0)))
  (if (< n 1000)
      (aref *lf-vec* n)
      (%log-factorial n)))
	
  
(defun poisson-probability (n u)
  (declare (type (integer 0) n))
  (cond ((= u 0) ;; special case that breaks down for n=0,u=u
	 (if (= n 0) 1d0 0d0))
	(t
	 (exp  (- (* n (log u))
		  (log-factorial n)
		  u)))))

;; currently brute-force sum, but will change to use incomplete gamma
;; function
(defun cumulative-poisson-probability (n1 n2 u)
  (if (> n1 n2) (rotatef n1 n2))
  (let ((sum 0.0d0))
    (loop for i from n1 to n2 do
	  (incf sum (poisson-probability i u)))
    sum))
		

;; works by adding together the logarithms of the various terms, then
;;  exponentiating
(defun binomial-probability (n m p)
    "What is probability of observing M events, given N opportunities 
each with probability P?"
  (declare (type (integer 0 #.most-positive-fixnum) n m)
	   (type real p))
  (let ((p (float p 1d0)))
    (declare (type double-float p)
	     (optimize speed))
    (cond 
      ((or (> p 1) (< p 0))
       (error "P=~A out of range [0,1]" p))
      ;; for p=1, special case of always observing m=n
      ((= p 1)
       (if (= n m) 1d0 0d0))
      ;; for p=0 observe m=0 all the time
      ((= p 0)
       (if (= m 0) 1.0 0.0))
      (t
       (locally (declare (type (double-float (0d0) (1d0)) p))
	 (exp
	  (+
	   (- (log-factorial n)
	      (+ (log-factorial m)
		 (log-factorial (- n m))))
	   (* m (log p))
	   (* (- n m) (log (- 1.0 p))))))))))

(defun cumulative-binomial-probability (n m1 m2 p)
  "What is probability of observing M1 to M2 events, given N 
opportunities, if each event has probability of P?"
  (declare (type (integer 0 #.most-positive-fixnum) n m1 m1)
	   (type real p))
  (if (> m1 m2) (rotatef m1 m2))
  (let ((sum 0.0d0)
	(p (float p 1d0)))
    (declare (type double-float sum p)
	     (optimize speed))
    (loop for i of-type fixnum from m1 to m2 do
      (incf sum (binomial-probability n i p)))
    sum))
       






(defun chi-square-probability (N chi-squared)
  (gamma-function:incomplete-gamma-function
   (coerce  (/ N 2) 'double-float)
   (coerce (/ chi-squared 2) 'double-float)))

(defun complementary-chi-square-probability (N chi-squared)
  (gamma-function:complementary-incomplete-gamma-function
   (coerce  (/ N 2) 'double-float)
   (coerce (/ chi-squared 2) 'double-float)))


;; compute x and P_cumul of (x)
(defun cumulative-probability (x)
  "given a sequence X, return (values X-sorted cumul-prob-of-X), where
both X-sorted cumul-prob-of-X are double-float vectors of the same
length as X"
  (declare (type (or vector list) x))
  (let* ((xx (sort (map '(simple-array double-float (*))
			(lambda (z) (coerce z 'double-float))
			x)
		   #'<))
	 (yy (loop for i from 0 to (1- (length xx))
		   with n = (* 1.0d0 (1- (length x)))
		   with y = (make-array (length x) :element-type 'double-float)
		   do (setf (aref y i)
			    (/ i n))
		   finally (return y))))
    (values xx yy)))
    
    
(defun pearson-r (xseq yseq)
  "Compute Pearson's R for linear correlation between sequences X and Y,
returning (VALUES R PROB-R FISHERS-Z)"
  (let ((x (seq-to-dbl-vec xseq))
	(y (seq-to-dbl-vec yseq))) 
    ;;
    (declare (type (simple-array double-float (*)) x)) 
    (declare (type (simple-array double-float (*)) y)) 
    ;;
    (when (not (= (length x) (length y)))
      (error "Sequences of unequal length"))
    ;;
    (prog ((r 0d0) (prob 0d0) (z 0d0) (tiny 0d0) (ax 0d0) (ay 0d0) 
	   (sxx 0d0) (syy 0d0) (sxy 0d0) (xt 0d0) (yt 0d0) (t0 0d0)
	   (df 0d0) (n 0))
       (declare (type double-float r prob z tiny ax ay sxx syy sxy xt yt t0 df))
       (declare (type fixnum n))
       
       (setq tiny 1.d-20) 
       (setf ax 0d0) 
       (setf ay 0d0)
       (setq n (array-dimension x 0))
       
       (do ((j 0 (+ j 1)))
	   ((> j (1- n)) t)
	 (declare (type fixnum j))
	 (setf ax (+ ax (aref x j)))
	 (setf ay (+ ay (aref y j)))) 
       
       (setf ax (/ ax n)) 
       (setf ay (/ ay n)) 
       (setf sxx 0d0) 
       (setf syy 0d0) 
       (setf sxy 0d0) 
       (do ((j 0 (+ j 1)))
	   ((> j (1- n)) t)
	 (declare (type fixnum j))
	 (setf xt (- (aref x j) ax))
	 (setf yt (- (aref y j) ay))
	 (setf sxx (+ sxx (expt xt 2)))
	 (setf syy (+ syy (expt yt 2)))
	 (setf sxy (+ sxy (* xt yt)))) 
       
       (setf r (/ sxy (sqrt (* sxx syy)))) 
       (setf z (* 0.5d0 (log (/ (+ 1d0 r tiny) (+ (- 1d0 r) tiny))))) 
       (setf df (float (- n 2) 1d0))
       (setf t0 (* r (sqrt (/ df (* (+ (- 1d0 r) tiny) (+ 1d0 r tiny)))))) 
       (setf prob (gamma-function:incomplete-beta-function
		   (* 0.5d0 df) 0.5d0 (/ df (+ df (expt t0 2)))))
       ;; (setf prob (erfcc (/ (abs (* z (sqrt (1- n)))) 1.414214d0))) 
       
       (return (values r prob z)))))	 



(defun spearman-r (xseq yseq)
  "Compute Spearman's Rank Order Coefficient for sequences XSEQ, YSEQ.
Return (VALUES D ZD PROBD RS PROBRS), where 

 D is the number of standard deviations
 from null hypothesis value of ZD

 PROBD is the 2-sided significance of D

 RS is Spearman's R

 PROBRS is the significance of RS.

This routine has been tested and the output values are the same as the C 
version of Numerical Recipes."
  (let ((data1 (seq-to-dbl-vec xseq))
	(data2 (seq-to-dbl-vec yseq))) 

    (declare (type (simple-array double-float (*)) data1)) 
    (declare (type (simple-array double-float (*)) data2)) 
    ;;
    (when (not (= (length data1) (length data2)))
      (error "Sequences of unequal length"))
    ;;
    (flet ((crank (w) ;; replace elements of vector by their rank
	     (declare (type (simple-array double-float (*)) w))
	     
	     (prog ((j 0) (jt 0) (t0 0d0) (n 0) (rank 0d0) (s 0d0))
		(declare (type fixnum j jt n))
		(declare (type double-float t0 rank s))
		
		(setq n (array-dimension w 0))
		(setf s 0d0) 
		(setf j 1) 
		label1 
		(when 
		    (< j n) 
		  (cond
		    ((not (= (aref w j) (aref w (- j 1))))  
		     (setf (aref w (1- j)) (float j 1d0))
		     (setf j (+ j 1)))
		    (t
		     (tagbody
			(do ((jtt (+ j 1) (+ jtt 1)))
			    ((> jtt n) t)
			  (declare (type fixnum j))
			  (setq jt jtt)
			  (if (not (= (aref w (1- jt)) (aref w (1- j)))) (go label2)))
			
			(setf jt (+ n 1))
		      label2 )
		     (setf rank (* 0.5d0 (float (1- (+ j jt)) 1d0)))
		     (do ((ji j (+ ji 1)))
			 ((> ji (1- jt)) t)
		       (declare (type fixnum ji))
		       (setf (aref w (1- ji)) rank))
		     
		     (setf t0 (float (- jt j) 1d0)) 
		     (setf s (- (+ s (expt t0 3)) t0)) 
		     (setf j jt)))
		  (go label1)) 
		(if (= j n) (setf (aref w (1- n)) (float n 1d0))) 
		;;
		(return (values w s))))
	   ;;
	   (sort2 (ra rb)
	     (declare (type (simple-array double-float (*)) ra))
	     (declare (type (simple-array double-float (*)) rb))
	     
	     (prog ((n 0) (rra 0d0) (rrb 0d0) (ir 0) (j 0) (l 0) (i 0))
		(declare (type fixnum n ir j l i))
		(declare (type double-float rra rrb))
		
		(setq n (array-dimension ra 0))
		(setf l (1+ (floor (/ n 2))))
		(setf ir n)
		label10
		(cond 
		  ((> l 1)
		   (setf l (1- l))
		   (setf rra (aref ra (1- l)))
		   (setf rrb (aref rb (1- l))))
		  (t
		   (setf rra (aref ra (1- ir)))
		   (setf rrb (aref rb (1- ir)))
		   (setf (aref ra (1- ir)) (aref ra 0))
		   (setf (aref rb (1- ir)) (aref rb 0))
		   (setf ir (1- ir))
		   (when 
		       (= ir 1)
		     (setf (aref ra 0) rra) 
		     (setf (aref rb 0) rrb) 
		     (return (values ra rb)))))
		
		(setf i l)
		(setf j (+ l l))
		label20
		(when
		    (<= j ir)
		  (if (and (< j ir) 
			   (< (aref ra (1- j)) (aref ra j)))
		      (setf j (1+ j)))
		  (cond
		    ((< rra (aref ra (1- j))) 
		     (setf (aref ra (1- i)) (aref ra (1- j)))
		     (setf (aref rb (1- i)) (aref rb (1- j)))
		     (setf i j)
		     (setf j (+ j j)))
		    (t 
		     (setf j (1+ ir))))
		  (go label20))
		(setf (aref ra (1- i)) rra)
		(setf (aref rb (1- i)) rrb)
		(go label10))))
      ;;
      (prog* ((t0 0d0) 
	      (n (array-dimension data1 0))
	      (wksp1 (make-array n :element-type 'double-float :initial-element 0d0))
	      (wksp2 (make-array n :element-type 'double-float :initial-element 0d0))
	      (d 0d0) (zd 0d0) 
	      (probd 0d0) (rs 0d0) (probrs 0d0) (sf 0d0) (sg 0d0)
	      (en 0d0) (en3n 0d0) (aved 0d0) (fac 0d0) (vard 0d0) (df 0d0))
	 
	 (declare (type (simple-array double-float (*)) wksp1)) 
	 (declare (type (simple-array double-float (*)) wksp2))
	 (declare (type fixnum n))
	 (declare (type double-float d zd probd rs probrs sf sg en en3n aved 
			fac vard df t0))
	 
	 (do ((j 0 (1+ j)))
	     ((> j (1- n)) t)
	   (declare (type fixnum j))
	   (setf (aref wksp1 j) (aref data1 j))
	   (setf (aref wksp2 j) (aref data2 j)))
	      (multiple-value-setq (wksp1 wksp2) (sort2 wksp1 wksp2)) 
	      (multiple-value-setq (wksp1 sf) (crank wksp1)) 
	      (multiple-value-setq (wksp2 wksp1) (sort2 wksp2 wksp1)) 
	      (multiple-value-setq (wksp2 sg) (crank wksp2)) 
	      (setf d 0d0) 
	      
	      (do ((j 0 (1+ j)))
		  ((> j (1- n)) t)
		(declare (type fixnum j))
		(setf d (+ d (expt (+ (aref wksp1 j) (- (aref wksp2 j))) 2)))) 
	      
	      (setf en (float n 1d0)) 
	      (setf en3n (+ (expt en 3) (- en))) 
	      (setf aved (+ (/ en3n 6d0) (/ (- (+ sf sg)) 12d0))) 
	      (setf fac (* (1+ (/ (- sf) en3n)) (1+ (/ (- sg) en3n)))) 
	      (setf vard (* (/ (* (* (1- en) (expt en 2)) (expt (1+ en) 2)) 36d0)
			    fac)) 
	      (setf zd (/ (+ d (- aved)) (sqrt vard))) 
	      (setf probd (gamma-function:erfc (/ (abs zd) 1.414214d0))) 
	      (setf rs (/ (1+ (* (- (/ 6d0 en3n)) (+ d (/ (+ sf sg) 12d0)))) (sqrt fac))) 
	      (setf fac (* (1+ rs) (1+ (- rs)))) 
	      (cond 
		((> fac 0d0)
		 (setf t0 (* rs (sqrt (/ (+ en (- 2d0)) fac)))) 
		 (setf df (+ en (- 2d0)))
		 (setf probrs (gamma-function:incomplete-beta-function
			       (* 0.5d0 df) 0.5d0 (/ df (+ df (expt t0 2))))))
		(t 
		 (setf probrs 0d0))) 
	      
	      (return (values d zd probd rs probrs))))))
  
  




