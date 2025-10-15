;; some simple fitting routines

(in-package imutils)



(declaim (inline 2x2-lin-solve))
    
(defun 2x2-lin-solve (a11 a12 a21 a22 b1 b2)
  "solve 2x2 linear equation A*x=b, returning (values x1 x2)"
  (let ((det (- (* a11 a22) (* a12 a21))))
    (values
     (/ (- (* a22 b1) (* a12 b2)) det)
     (/ (- (* a11 b2) (* a21 b1)) det))))


;; eigenvals - verified  using textbook examples
;; watch out - can be complex, so use 2x2-real-symmetric-eigenvals  
#+nil
(defun 2x2-eigenvals (a11 a12 a21 a22)
  (let* ((det (- (* a11 a22) (* a12 a21)))
	 (tr  (+ a11 a22))
	 (tmp (sqrt (- (expt tr 2) (* 4 det))))
	 (e1 (* 0.5 (+ tr tmp)))
	 (e2 (* 0.5 (- tr tmp))))
    (values e1 e2))) 
    


(defun 2x2-real-symmetric-eigenvals (a11 a12-and-a21 a22)
  (let* ((a12 a12-and-a21)
	 (a21 a12-and-a21)
	 (det (- (* a11 a22) (* a12 a21)))
	 (tr  (+ a11 a22))
	 (tmp (sqrt (abs (- (expt tr 2) (* 4 det)))))
	 (e1 (* 0.5 (+ tr tmp)))
	 (e2 (* 0.5 (- tr tmp))))
    (values e1 e2))) 
    
  
(defun fit-quadratic (image nx0 ny0 npix &key (errors :const))
  "Fit a quadratic function 

   Z=a*X^2 + b*XY + c*Y^2 + d*X + e*Y + f

to an image, where NX0 and NY0 are the center of the fit, and NPIX is
an odd number representing the size (NPIX x NPIX) block to fit.

ERRORS can be :SQRT or :CONST, where :CONST makes all errors equal 1.0

returns (VALUES X0 Y0 SIGN PARAMS COVAR CHISQR) where 

  X0,Y0 are the extremum of the quadratic form, where the derivative is zero.

  SIGN is +1, -1, or 0, if the polynomial is positive definite,
  negative definite, or indefinite, judged using eigenvalues
  SIGN=0 indicates a saddle point.

  PARAMS = #(a b c d e f)

  COVAR and CHISQR are the covariance matrix of PARAMS, and chisqr as
  defined in llsq.lisp.

  Note that any polynomial Z=a*X^2 + b*XY + c*Y^2 + d*X + e*Y + f
  can be expressed as 
   Z = (x-x0,y-y0)^T A (x-x0,y-y0)^T + k
  where      A  = [[a, b/2], [b/2,c]]
        (x0,y0) = -1/2 A^-1 (d,e)
              k = f - (x0,y0)^T A (x0,y0)

"
  (declare (type image image)
	   (type (unsigned-byte 20) nx0 ny0)
	   (type (unsigned-byte 10) npix) 
	   (type (member :const :sqrt) errors))
  ;;
  (when (not (oddp npix))
    (error "Fit region size NPIX is not odd"))
  
  (let* ((npts (* npix npix))
	 (yvec (make-array  npts :element-type 'double-float))
	 (sigvec (make-array  npts :element-type 'double-float))
	 ;; 6 terms are  xx,yy,xy,x,y,1
	 (xbasis (locally 
		     (declare (optimize (speed 0)))
		   (make-array (list 6 npts) :element-type 'double-float)))
	 ;;
	 (n/2 (ash npix -1))
	 (nxmax (1- (array-dimension image 1)))
	 (nymax (1- (array-dimension image 0)))
	 )
	;;
	;;
    (declare (type (simple-array double-float (*)) yvec sigvec)
	     (type (simple-array double-float (* *)) xbasis)
	     (type (unsigned-byte 28) npts n/2 nxmax nymax)
	     (optimize speed))
    ;; check for out of bounds fit
    (when (or (< nx0 n/2) (> (+ n/2 nx0) nxmax)
	      (< ny0 n/2) (> (+ n/2 ny0) nymax))
      (error "Out of bounds fit - fitting ~Dx~D block at nx0=~D, ny0=~D in ~Dx~D image"
	     npix npix nx0 ny0 (1+ nxmax) (1+ nymax)))
    ;;
    ;; compute all the matrix terms and fill in yvec sigvec xbasis
    (loop
       with i of-type (unsigned-byte 28) = 0
       for ix of-type (signed-byte 20) from (- n/2) to n/2
       for x of-type double-float = (float ix 1d0)
       do
	 (loop 
	    for iy of-type (signed-byte 20) from (- n/2) to n/2
	    for y of-type double-float = (float iy 1d0)
	    for z of-type double-float = (float (aref image (+ iy ny0) (+ ix nx0)) 
						1d0)
	    do
	    ;;(print (list ix iy (+ ix nx0) (+ iy ny0) x y  z))
	      (setf (aref yvec i) z)
	      (setf (aref sigvec i)
		    (cond ((eq errors :const)
			   1d0)
			  ((not (plusp z))
			   (locally ;; suppress compilation message
			       (declare (optimize (speed 0)))
			     (error 
			      "ERRORS is :SQRT and image value ~F is non-positive" z)))
			  (t
			   (sqrt (the (double-float (0d0)) z)))))
	    ;;
	    ;;
	    ;; A little bit of trickiness - the quadratic components
	    ;; of XBASIS are the integral of xx,yy,xy over a pixel,
	    ;; and that involves an extra 1/12 component except for
	    ;; xy. Consider a pixel at 0,0 - the xx integral over it
	    ;; is not zero, but 1/12
	      (setf (aref xbasis 0 i) (+ (* x x) #.(/ 1d0 12)))
	      (setf (aref xbasis 1 i) (* x y)) ;; no 1/12 for xy
	      (setf (aref xbasis 2 i) (+ (* y y)  #.(/ 1d0 12)))
	      (setf (aref xbasis 3 i) x)
	      (setf (aref xbasis 4 i) y)
	      (setf (aref xbasis 5 i) 1d0)
	    ;;
	      (incf i)))
    ;;
    (multiple-value-bind (qp covar chisqr) ;; qp=quad-params
	(llsq:lfit-values yvec sigvec xbasis)
      (declare (type (simple-array double-float (*)) qp)
	       (type (simple-array double-float (* *)) covar)
	       (type double-float chisqr))
      (let* ((a (aref qp 0))
	     (b (aref qp 1))
	     (c (aref qp 2))
	     (d (aref qp 3))
	     (e (aref qp 4))
	     ;; elements of 2x2 matrix representing quadratic form
	     (a11 a) 
	     (a12 (* b 0.5)) 
	     ;;(a21 a12) 
	     (a22 c)
	     (sign ;; if both eigenvals are negative, quadform is neg def
	      (multiple-value-bind (e1 e2) ;; e1,2 are eigenvals
		  (2x2-real-symmetric-eigenvals a11 a12 a22)
		(cond ((and (plusp e1) (plusp e2))
		       +1)
		      ((and (minusp e1) (minusp e2))
		       -1)
		      (t 0)))))
	;; do 2x2 lin solve to get center x0,y0 of quadratic 
	(multiple-value-bind (x0 y0)
	    (2x2-lin-solve  (* 2d0 a)  b b (* 2d0 c) (- d) (- e))
	  ;;
	  (values (+ x0 nx0) (+ y0 ny0) 
		  sign
		  qp covar chisqr))))))


;; the formula for the eigenvalues of a 2x2 matrix ((a b)(c d)) is
;; Tr = trace = a+d
;; Det = determinant = (ad-bc)
;; lambda1 = (Tr/2 + sqrt(Tr^2/4-Det))
;; lambda2 = (Tr/2 - sqrt(Tr^2/4-Det))
;; for non-zero c
;;   Evec1   = (lambda1 - d , c)    
;;   Evec1   = (lambda2 - d , c)
;; for non-zero b
;;   Evec1   = (b, lambda1 - a)    
;;   Evec1   = (b, lambda2 - a)
;; for c=b=0
;;   Evec1 = Evec2 = (0,0)
;;
(defun eigenvectors-and-eivenvalues-of-quad-solution (params)
  "Given PARAMS=(a b c ...) that solve
      Z=a*X^2 + b*XY + c*Y^2 + d*X + e*Y + f
   return

    (VALUES
       EIGENVALUE-1 EIGENVECTOR-1 ;; the smaller eigenvalue (long axis)
       EIGENVALUE-2 EIGENVECTOR-2);; the larger eigenvalue (short axis)

where the eigenvalues have been normalized.

The spatial scales associated with the eigenvalues
are 1/sqrt(eigenvalue-n)

The largest absolute value eigenvalue comes first."
  (declare (type (simple-array double-float (*)) params))
  ;; matrix is [[cxx,cxy],[cxy,cyy]]
  (let* ((cxx (aref params 0))  ;; a X^2
	 (cxy (* 0.5d0 (aref params 1))) ;; 2 (b/2) XY
	 (cyy (aref params 2)) ;; C Y^2
	 (tr  (+ cxx cyy))  ;; trace of matrix
	 (det (- (* cxx cyy) (* cxy cxy))) ;; determinant
	 ;; symmetric matrix has real eigenvalues, so sqrt has non-neg arg
	 (sqrt-tr2/4-d (sqrt (the (double-float 0d0)
				  (- (/ (* tr tr) 4) det))))
	 (eigenval1 (+ (/ tr 2) sqrt-tr2/4-d))  ;; eigenvalues
	 (eigenval2 (- (/ tr 2) sqrt-tr2/4-d))
	 (eigenvec1 (make-array 2 :element-type 'double-float))
	 (eigenvec2 (make-array 2 :element-type 'double-float)))
    (flet ((normalize (vec)
	     (let ((norm (sqrt (the (double-float 0d0)
			       (+ (expt (aref vec 0) 2)
				  (expt (aref vec 1) 2))))))
		       (setf (aref vec 0) (/ (aref vec 0) norm))
		       (setf (aref vec 1) (/ (aref vec 1) norm)))))
      (cond ((not (zerop cxy))
	     (setf (aref eigenvec1 0) (- eigenval1 cyy))
	     (setf (aref eigenvec1 1) cxy)
	     (setf (aref eigenvec2 1) (- eigenval2 cyy))
	     (setf (aref eigenvec2 0) cxy)
	     ;;
	     (normalize eigenvec1)
	     (normalize eigenvec2))
	    (t ;; diagonal matrix
	     (setf (aref eigenvec1 0) 1d0)
	     (setf (aref eigenvec1 1) 0d0)
	     (setf (aref eigenvec1 0) 0d0)
	     (setf (aref eigenvec1 1) 1d0)))
      ;;
      ;; ensure smaller eigenval pair is first
      (when (< (abs eigenval2) (abs eigenval1))
	(rotatef eigenval1 eigenval2)
	(rotatef eigenvec2 eigenvec1))
      
      (values eigenval1 eigenvec1 eigenval2 eigenvec2))))


(defstruct quadfit-result
  (x0    0d0 :type double-float)
  (y0    0d0 :type double-float)
  (sign  0   :type (integer -1 1)) ;; -1 for negative definite
  (params nil :type (or null (simple-array double-float (6)))) ;; #(a b c d e f)
  (covar  nil :type (or null (simple-array double-float (6 6)))) ;; covariance matrix of params
  (chisqr 0d0 :type double-float)
  ;; eivenvalues; 1 is long-axis, and 2 is short-axis
  (eigenval-1 0d0 :type double-float)
  (eigenval-2 0d0 :type double-float)
  ;; eigenvectors (normalized); 1 is long-axis, and 2 is short-axis
  (eigenvec-1 nil :type (or null (simple-array double-float (2))))
  (eigenvec-2 nil :type (or null (simple-array double-float (2))))
  ;;
  (pa     0d0 :type double-float) ;; position angle, degrees in -X rotation from +Y axis
  (fwhm-1 0d0 :type double-float) 
  (fwhm-2 0d0 :type double-float)
  ;; background level, necessary for extracting the fwhm scales
  (backd 0d0 :type double-float))
  
  
;; a newer version of fit-quadratic that produces more information in the form
;; of a QUADFIT-RESULT structure
(defun fit-quadratic/deluxe (image nx0 ny0 npix &key (background 0d0) (errors :const))
  "Begin with FIT-QUADRATIC to fit a quadratic function 

   Z=a*X^2 + b*XY + c*Y^2 + d*X + e*Y + f

to an image, where NX0 and NY0 are the center of the fit, and NPIX is
an odd number representing the size (NPIX x NPIX) block to fit.

ERRORS can be :SQRT or :CONST, where :CONST makes all errors equal 1.0, and return
a QUADFIT-RESULT structure with extra information, like axes and FWHM.

BACKGROUND is used only to calculate estimated FWHM of the two
principal directions.

The FWHM for a Gaussian turns out to be fairly accurate."





  (multiple-value-bind (x0 y0 sign params covar chisqr)
      (fit-quadratic image nx0 ny0 npix :errors errors)
    (multiple-value-bind (eigenval-1 eigenvec-1 eigenval-2 eigenvec-2)
	(eigenvectors-and-eivenvalues-of-quad-solution params)
      (let* ((pa (* (/ 180 pi) (acos (aref eigenvec-1 1)))) ;; acos of dot product with #(0 1)
	     (f (aref params 5))
	     (height (- f background)) ;; distance of top of parabola above backd
	     (fwhm-1 (* 2 (sqrt (abs (/ height (* 2 eigenval-1))))))
	     (fwhm-2 (* 2 (sqrt (abs (/ height (* 2 eigenval-2)))))))
	
	(make-quadfit-result 
	 :x0 x0 :y0 y0 :sign sign
	 :params params :covar covar :chisqr chisqr
	 :eigenval-1 eigenval-1 :eigenval-2 eigenval-2
	 :eigenvec-1 eigenvec-1 :eigenvec-2 eigenvec-2
	 :fwhm-1 fwhm-1
	 :fwhm-2 fwhm-2
	 :pa pa)))))
	     
    
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test-fit-quadratic (&key (nx 100) (ny 100) (x0 33.333) (y0 44.444)
			   (a 0.5) (b 0.0) (c 0.5) (d 0.0) (e 0.0) (f 0.0)
			   (nfit 7))
  (let ((image (make-2d-quadratic-test-array :nx nx :ny ny
					     :x0 x0 :y0 y0
					     :a a :b b :c c :d d :e e :f f)))
    (fit-quadratic image (round x0) (round y0) nfit :errors :const)))
