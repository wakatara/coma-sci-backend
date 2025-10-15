
(in-package cfht-mbc)

(eval-when (load eval compile)
  (require 'pgplot)
  (require 'numio)
  (require 'stats)
  )
#|

Adjustment of completeness

We know that we find 0 in our TALCS sample.

We then consider subsets - for example a=[1,2],
and count the number of orbits Ns in this subset.

We examine ASTORB to find the number of orbits Na
in this subset of Ns asteroids

We compute the completeness C of ASTORB for this subset as follows:
   1. Count orbits brighter than H<14.8 in this subset in ASTORB.
      This gives the ASTORB number Na for H where ASTORB is complete.
   2. Now compute Nm, the number of orbits in a model exponential
      distribution, also H<14.8
   3. The ratio f=Na/Nm is now the fraction of all orbits within the
      completeness limit H<14.8 that belong to the subset
   4. Now multiply f by the total number Nmt in the model with H<21 to get
      the true number Nt of orbits in this subset in the entire
      distribution, even where ASTORB is incomplete:

      Nt = f*Nmt = (Na/Nm)*Nmt True number in this subset in whole
                               belt

   5. The completeness is then  C = Ns/Nt = (Ns Nm)/(Na Nmt)
    
   See function (COMPUTE-ASTORB-COMPLETENESS  ..)
           and  (COMPUTE-ASTORB-COMPLETENESS-IN-H ...)

   


Next, to compute the 90% error limit giving a detection of 0 MBCs among
Ns, we compute the 90% limit on finding fewer than K MBCs using either
Bayesian or frequentist methods (frequentist always gives 2.3/Ns).

We then divide K by the completeness limit C to give the 90% upper
bound within the whole population:

    N90=K/C

This N90 is what we report at end computed in 
  (COMPUTE-FRACTIONS-OF-ORBIT-SAMPLE-VS-TOTAL-SAMPLE)
which takes the upper bound functin (Bayes,Poisson) as an argument



|#


(defvar *ast-a-vec* nil)
(defvar *ast-e-vec* nil)
(defvar *ast-ph-vec* nil)
(defvar *ast-i-vec* nil)
(defvar *ast-h-vec* nil)
(defvar *ast-diam-vec* nil)


;; caching of quantiles for speed
(defvar *ast-e-1/3* nil)
(defvar *ast-e-2/3* nil)
(defvar *ast-i-1/3* nil)
(defvar *ast-i-2/3* nil)
(defvar *ast-h-1/3* nil)
(defvar *ast-h-2/3* nil)


;(setf *ast-e-1/3* nil)
;(setf *ast-e-2/3* nil)
;(setf *ast-i-1/3* nil)
;(setf *ast-h-1/3* nil)
;(setf *ast-h-2/3* nil)

(defparameter *astorb-file* 
  (concatenate 'string *data-dir* "/astorb.dat"))


;; astorb is a real PITA to read because not all fields are present
(defun read-astorb (&key (infile *astorb-file*))
  (let ((n (numio:file-count-lines infile)))
    (setf *ast-a-vec* (make-array n :element-type 'single-float))
    (setf *ast-e-vec* (make-array n :element-type 'single-float))
    (setf *ast-ph-vec* (make-array n :element-type 'single-float))
    (setf *ast-i-vec* (make-array n :element-type 'single-float))
    (setf *ast-h-vec* (make-array n :element-type 'single-float))
    (setf *ast-diam-vec* (make-array n :element-type T)) ;; can be NIL
    ;;
    (with-open-file (s infile)
      (loop 
	 for i below n
	 for line of-type simple-string = (or (read-line s nil nil)
					      (error "premature end of infile"))
	 for h = (float (numio:parse-float line :start 42 :junk-allowed t) 1.0)
	 for inc = (float  (numio:parse-float line :start 147 :junk-allowed t)  1.0)
	 for ecc = (float (numio:parse-float line :start 157 :junk-allowed t) 1.0)
	 for a = (float (numio:parse-float line :start 170 :junk-allowed t) 1.0)
	 for diam = (ignore-errors (numio:parse-float line :start 60 :end 65))
	 do
	   (setf (aref *ast-a-vec* i) a)
	   (setf (aref *ast-i-vec* i) inc)
	   (setf (aref *ast-h-vec* i) h)
	   (setf (aref *ast-e-vec* i) ecc)
	   (setf (aref *ast-diam-vec* i) diam)
	   (setf (aref *ast-ph-vec* i) (* (- 1.0 ecc) a))))
    ;;
    n))
		 
	   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fudge has to be put in to make the theoretical distribution match the 
(defun pred-h-df (h  &key (fudge 0.5))  ;; predicted h distribution function
  (* 0.0059 fudge (expt 10.0 (/ h 2.0))))

(defun pred-h-cdf (h1 h2 &key (fudge 0.5)) ;; predicted h CUMUL distribution function
  (flet ((cdf (h)
	   (* 0.0059  fudge #.(/ 2 (log 10.0))  
	      (expt 10.0 (/ h 2.0)))))
    (- (cdf h2)
       (cdf h1))))


(defun get-bright-ast (ast-vec &key (h-complete 14.8))
  "Get the subset of some AST quality ast-vec (eg *ast-a-vec*) such that the
elements correspond to bright *ast-h-vec* members; ie, the set over which
the data are complete"
  (let ((n (loop for x across ast-vec and h across *ast-h-vec*
	      when (< h h-complete) sum 1)))
    (loop with v = (make-array n :element-type 'single-float)
       with i = 0 
       for x across ast-vec and h across *ast-h-vec*
       when (< h h-complete) 
       do 
	 (setf (aref v i) x) 
	 (incf i)
       finally (return v))))


(defun compute-astorb-completeness (selector-function &key (h-complete 14.8))
  "Given selector function (f a e i) that returns T/NIL, compute how
incomplete ASTORB's sample is compared to the theoretical exponential,
assuming that astorb is complete to H = H-COMPLETE

It works by computing Nfound/Nexpected where Nfound is the number
found in the selected sample, and Nexpected is the number that should
be there.

** This method CANNOT work for H, because you can't sample large-H
asteroids in the complete small H-sample.

Completeness = Nfound/Nexpected

Nexpected = (Nbright_in_sample/Nbright_total).Ntotal

In other words, scale Ntotal (all asteroids anywhere, in theoretical
model) by their under-representation in the bright part of the
selected sample relative to the total bright sample."
  (loop
     with n-comp = 0 ;; number in our sample that are complete
     with n-tot = 0 ;; number in our sample
     with ntrue-comp = (pred-h-cdf 6.0 14.8) ;; all ast in complete region
     with ntrue-tot = (pred-h-cdf 6.0 21.0) ;; all asteroids 
     for a across *ast-a-vec* 
     for e across *ast-e-vec* 
     for i across *ast-i-vec* 
     for h across *ast-h-vec* 
     for selected = (funcall selector-function a e i)
     when selected
     do
       (when (<= h h-complete)
	 (incf n-comp))
       (incf n-tot)
     finally
       #+nil
       (format t "ntot=~A  ntrue-comp=~A  ntrue-tot=~A  ncomp=~A~%"
	       n-tot  ntrue-comp ntrue-tot n-comp)
       (return 
	 (/
	  (* n-tot ntrue-comp)       ;; Ns Nm    - in notation of doc above
	  (* ntrue-tot n-comp)))))   ;; Nmt na  



(defun compute-astorb-completeness-in-H (h1 h2  &key (h-complete 14.8))
  "Compute ASTORB completeness over a given H range - requires a bit of fudginess
because the powerlaw is only good below h>14"
  (if (< h2 h-complete)
      1.0
      (min ;; the fact that powerlaw is not good approx means we have to do a MAX
       1.0 
       (let ((nseen (count-if (lambda (h) (< h1 h h2)) *ast-h-vec*))
	     (nactual (pred-h-cdf h1 h2)))
	 (/ nseen nactual)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plot-astorb-cumul (&key (device :x11) (filename nil) (hmin 5.0) (hmax 20.0))
  (let* ((p (pgplot:open-device device :filename filename))
	 (ast-h-vec (remove-if (lambda (h) (< h hmin)) *ast-h-vec*))
	 (n (length ast-h-vec)))
    (pgplot:toplabel p "ASTORB cumul dist vs model dN/dH=c 10\\uH/2\\d" :character-height 1.3)
    (multiple-value-bind (x y)
	(stats:cumulative-probability ast-h-vec)
      (loop for i below n do (setf (aref y i) (* n (aref y i))))
      (pgplot:set-window p hmin hmax 0 8)
      (pgplot:box p :y-log t)
      (pgplot:xlabel p "H" :y-offset -0.03)
      (pgplot:ylabel p "N<H" :x-offset -0.03)
      (pgplot:connect p x (map 'vector (lambda (y) (log (+ 1d-10 y) 10))    y))
      ;(plot:plot-vectors x (map 'vector (lambda (y) (log (+ 1d-10 (* n y)) 10))    y))

      (loop for h from hmin to hmax by 0.25
	   for nca = (pred-h-cdf hmin h :fudge 0.5)
	   for ncb = (pred-h-cdf hmin h :fudge 1)
	   do 
	   (pgplot:points p h (log nca 10) :filled-circle :color :red :size 0.5)
	   (pgplot:points p h (log ncb 10) :filled-circle :color :green :size 0.5))

      (pgplot:draw-plot-legend 
       p 0.1 0.9
       '(("dN/dH=0.00295 10\\uH/2\\d" :point :filled-circle :color :red :size 2.0)
	 ("dN/dH=0.00590 10\\uH/2\\d" :point :filled-circle :color :green :size 2.0)
	 ("ASTORB" :line))
       :draw-box t)
      (when filename (pgplot:close-device p)))))


(defmacro %orset (var exp)  ;; helper macro to reuse var or set it to exp
  `(or ;,var
       (setf ,var
	     ,exp)))



(defun count-talcs-orbits-such-that (faeih)
  "count TALCS orbits that satisfy a predicate (faeih a e i h) applied
to orbits in *orbit-hash*"
  (loop 
     for celem being the hash-value of *orbit-hash*
     when (funcall faeih (celem-a celem) (celem-e celem) 
		   (celem-orbinc celem) (celem-hv celem))
     count celem))


(defun count-astorb-orbits-such-that (faeih)
  (loop 
     for a across *ast-a-vec*
     for e across *ast-e-vec*
     for i across *ast-i-vec*
     for h across *ast-h-vec*
     when (funcall faeih a e i h)
     sum 1))
       




(defun count-talcs+astorb+total-orbits (&key (faei nil) (h1 nil) (h2 nil)
					(h-complete 14.8))
  "Given a test function (faei a e i) returns
  (VALUES N-TALCS N-ASTORB N-TRUE ASTORB-COMPLETENESS) Alternatively,
if H1 and H2 are set instead of FAEI, then assume the range is in H."
  (let ((faeih (lambda (a e i h)
		 (if faei
		     (funcall faei a e i)
		     (and (<= h1 h) (< h h2))))))
		 
    ;;
    (when (and (not (or faei (and h1 h2)))
	       (not (and faei (or h1 h2))))
      (error "Either FAIE or (AND H1 H2) must be true"))
    ;;
    (let* ((n-talcs (count-talcs-orbits-such-that faeih))
	   (n-astorb (count-astorb-orbits-such-that faeih))
	   (completeness
	    (if h1
		(compute-astorb-completeness-in-h h1 h2 :h-complete h-complete)
		(compute-astorb-completeness  faei :h-complete h-complete)))
	   (n-true
	    (/ n-astorb completeness)))
      ;;
      (values n-talcs n-astorb n-true completeness))))
	   





(defun poisson-upper-bound-on-fraction (m x)
  "X% Poisson upper bound on fraction given m observed zero discovered"
  (/ (- (log (- 1.0 x)))
     m))

(defvar *mhsieh* 300) ;; number by hsieh; actual is 600
(defun bayesian-upper-bound-on-fraction (m x &key (mh *mhsieh*))
  (bisection-root:findroot (lambda (f)
			     (-
			      (gamma-function:incomplete-beta-function 
			       (+ 0d0 1d0) (+ m mh 00d0)   (+ 0d0 f))
			      x))
			   0d0 0.10d0))







(defun fastpercentile (v f)
  (fastmedian:fast-single-float-1d-array-fraction 
   (copy-seq v) f))
  


(defun compute-fractions-of-orbit-sample-vs-total-sample 
    (&key
     (func-upper-bound 'poisson-upper-bound-on-fraction)
     (x-upper-bound 0.9)
     (adjust-catalog-completeness t))
  (declare (optimize debug))
  (when (not *ast-a-vec*) (read-astorb))
  (when (zerop (hash-table-count *orbit-hash*))   (read-orbits))
  (let* 
      ;; first set up the parameter ranges
      ((a-inner  2.5) ;; end of inner belt
       (a-middle 2.82) ;;end of middle belt
       ;;
       (nast-inner (count-if (lambda (a) (< a a-inner)) *ast-a-vec*))
       (nast-outer (count-if (lambda (a) (> a a-middle)) *ast-a-vec*))
       (nast-middle (- (length *ast-a-vec*) nast-inner nast-outer))
       ;;
       ;;
       (n-1/3 (/ (length *ast-a-vec*) 3.0)) ;; number in one of our 1/3 bins
       ;;
       (i-1/3 (%orset *ast-i-1/3* 
		      (fastpercentile (get-bright-ast *ast-i-vec*) 0.333)))
       (i-2/3 (%orset *ast-i-2/3* 
		      (fastpercentile (get-bright-ast *ast-i-vec*) 0.666)))
       (i-3/3 (stats:max-of-elements *ast-i-vec*))
       (e-1/3 (%orset *ast-e-1/3* 
		      (fastpercentile (get-bright-ast *ast-e-vec*) 0.333)))
       (e-2/3 (%orset *ast-e-2/3* 
		      (fastpercentile (get-bright-ast *ast-e-vec*) 0.666)))
       (e-3/3 (stats:max-of-elements *ast-e-vec*))
       ;; we divide the h ranges by hand - 6.0 to 21 as 6-11,11-16,16-21
       (h-1/3 (%orset *ast-h-1/3* 11.0))
       (h-2/3 (%orset *ast-h-2/3* 16.0))
       (h-3/3 21.0);(stats:max-of-elements *ast-h-vec*))
       ;;
       ;; output list for TeX table generation
       ;; (PARAM y0 y1  n-talcs f90 N90)
       (stats-list nil))

    ;; fix the limits
    (setf i-3/3 180.0)
    (setf e-3/3 1.0)


       ;;
    (flet ((print-one-bound (param-name ybot ytop &key faei h1 h2)
	     (multiple-value-bind (n-talcs n-astorb n-real completeness)
		 (count-talcs+astorb+total-orbits  :faei faei :h1 h1 :h2 h2)
		 (declare (ignorable  n-astorb n-real))
		 (let* 
		     ;; f is limit on fraction given n-talcs observed, with 0 mbcs
		     ((fXX   (funcall func-upper-bound n-talcs x-upper-bound))
		      ;; nxx is f times total number
		      (nXX  (* n-real fXX)))
		   ;;
		   (format t "~20A   M=~8,1F n=0  f~D=~9,6F   ntot=~,1F~%"
			   (format nil "~A=[~,2F, ~,2F]" param-name ybot ytop )
			    n-talcs
			   (round (* x-upper-bound 100))  fXX 
			   nXX)
		   (push (list param-name ybot ytop  n-talcs fXX nXX)
			 stats-list)))))
	     
      ;;

      (format t "M is number in TALCS sample in this range~%")
      (format t "n is number of MBCS we observed (zero, always)~%")
      (format t "fXX is the upper XX fractional bound, using bayes or poisson~%")
      (format t "ntot is the fXX bound in the total asteroid belt~%~%")

      (print-one-bound "All TALCS" 0 1
		       :faei (lambda (a e i) (declare (ignorable a e i)) t))


      (print-one-bound "a" 0 a-inner       
		       :faei (lambda (a e i) (declare (ignorable a e i))
				     (< 0 a a-inner)))
      (print-one-bound "a" a-inner a-middle   
		       :faei (lambda (a e i) (declare (ignorable a e i))
				     (< a-inner a a-middle)))
      (print-one-bound "a" a-middle 10      
		       :faei (lambda (a e i) (declare (ignorable a e i))
				     (< a-middle a 10)))
      ;;
      (print-one-bound "i" 0     i-1/3        
		       :faei (lambda (a e i) (declare (ignorable a e i))
				     (< 0 i i-1/3)))
      (print-one-bound "i" i-1/3 i-2/3    
		       :faei (lambda (a e i) (declare (ignorable a e i))
				     (< i-1/3 i i-2/3)))    
      (print-one-bound "i" i-2/3 i-3/3        
		       :faei (lambda (a e i) (declare (ignorable a e i))
				     (< i-2/3 i i-3/3)))
      ;;
      (print-one-bound "e" 0     e-1/3        
		       :faei (lambda (a e i) (declare (ignorable a e i))
				     (< 0 e e-1/3)))
      (print-one-bound "e" e-1/3 e-2/3    
		       :faei (lambda (a e i) (declare (ignorable a e i))
				     (< e-1/3 e e-2/3)))    
      (print-one-bound "e" e-2/3 e-3/3        
		       :faei (lambda (a e i) (declare (ignorable a e i))
				     (< e-2/3 e e-3/3)))
      ;;
      (print-one-bound "H" 0     h-1/3  :h1 0     :h2 h-1/3)
      (print-one-bound "H" h-1/3 h-2/3  :h1 h-1/3 :h2 h-2/3)
      (print-one-bound "H" h-2/3 h-3/3  :h1 h-2/3 :h2 h-3/3)
      ;;
      (reverse stats-list)
      )))

		       

;; columns are 
;;  NAME[x,x]   Ntalcs f90 N90   f90 N90  
;;                     Poisson   Bayes
(defun make-mbc-limits-latex-table (&key (outfile "limits.tex"))

  (with-open-file (sout outfile :direction :output :if-exists :supersede)
    (format sout "\\begin{tabular}[h]{r@{$\\,\\in\\,$}lr|cr|cr}\\hline~%")
    (format sout 
	    "\\multicolumn{2}{c}{Subsample} & $N_{\\rm TALCS}$ & $f_{90}$  & $M_{90}$ & $f_{90}$  & $M_{90}$\\\\~%")
    (format sout
    " \\multicolumn{2}{c}{}  &      &\\multicolumn{2}{c|}{Poisson}&\\multicolumn{2}{c}{Bayesian}\\\\\\hline~%")
    (flet ((do-rows (listp listb)
	     (loop 
		with k = (length listp)
		for i from 1
		for sublistb in listb
		for symb = (first sublistb) and yminb = (second sublistb)
		and ymaxb = (third sublistb) and ntalcsb = (fourth sublistb) 
		and f90b = (fifth sublistb) and n90b = (sixth sublistb)
		for sublistp in listp
		for symp = (first sublistp) and yminp = (second sublistp)
		and ymaxp = (third sublistp) and ntalcsp = (fourth sublistp) 
		and f90p = (fifth sublistp) and n90p = (sixth sublistp)
		for lead = ;; lead string
		  (if (= i 1) ;; all talcs
		      "\\multicolumn{2}{l}{All TALCS}"
		      (format nil "$~A$&$[~,2F,\\,~,2F]$" symb yminb ymaxb ))
		;;
		do
		  (format sout
			  "~A& ~D & ~,4F  & ~A   & ~,4F  & ~A \\\\~A~%"
			  lead ntalcsp f90p 
			  (latex:pgplot-encode-float-sci-notation n90p 1 :min-exponent 0) 
			  f90b 
			  (latex:pgplot-encode-float-sci-notation n90b 1 :min-exponent 0) 
			  (if (= i k) "\\hline" "")))))

      (do-rows 
	  (compute-fractions-of-orbit-sample-vs-total-sample 
	   :func-upper-bound 'poisson-upper-bound-on-fraction)
	(compute-fractions-of-orbit-sample-vs-total-sample 
	 :func-upper-bound 'bayesian-upper-bound-on-fraction))

    (format sout "\\end{tabular}~%~%"))))