
(in-package imutils)



;; for a given function FUNC and profile, compute the penalty for the fit
;; FUNC is called as (FUNC R PARAMS) where PARAMS is a vector
(declaim (inline %compute-goodness-of-fit-for-profile))
(defun %compute-goodness-of-fit-for-profile (func params r-vec flux-vec flux-err-vec
					     method)
  (declare (type (function (double-float (simple-array double-float (*))) double-float)
		 func)
	   (type (simple-array single-float (*)) r-vec flux-vec flux-err-vec)
	   (optimize speed))
  (loop
    for r of-type single-float across r-vec
    for f of-type single-float across flux-vec
    for err of-type single-float across flux-err-vec
    for funcval of-type double-float = (funcall func (* r 1d0) params)
    for y of-type double-float = (/ (- funcval f) (+ 1d-20 err))
    sum (if (eq method :abs)
	    (abs y)
	    (expt y 2))
      of-type double-float))


;; given a radial profile, return estimates of (VALUES BACKGROUND NORMALIZATION HWHM)
(defun %estimate-1d-profile-backd+norm+fwhm (r-vec flux-vec)
  (declare (type (simple-array single-float (*)) r-vec flux-vec))
  (let* ((nelem (length r-vec))
	 (backd (loop with n = (round (* 0.1 nelem)) ;; average of last 10% of fluxes
		      for i from 1 to n
		      sum (aref flux-vec (- nelem i)) into tail-vals
		      finally (return (/ tail-vals n))))
	 (hwhm (loop with f0/2 = (* 0.5 (-  (aref flux-vec 0) backd))
		     for i below nelem
		     for r across r-vec
		     for f across flux-vec
		     when (< (- f backd) f0/2)
		       do (return r)))
	 (norm (loop with dr = (- (aref r-vec 1) (aref r-vec 0))
		     for r across r-vec
		     for f across flux-vec
		     sum (* 2.0 pi r (- f backd) dr))))
    (values backd norm hwhm)))
    
						   
  


;; for a given function (FUNC r parameters), and radial profile,
;; compute the best-fit parameters.  PARAMS are the starting parameters,
;; and OPTIM-FLAGS is NIL where the params are to be frozen and not fit
(defun fit-1d-profile (func r-vec flux-vec flux-err-vec
			     params optim-flags
			     &key
			       (method :square)
			       (ftol 1d-10))
  "Fit a 1D radial profile represented by single-float vectors R-VEC,
FLUX-VEC, FLUX-ERR-VEC with a double-float function (FUNC R PARAMS)
where R is the double-float radius, and PARAMS is a vector of parameters
the function depends on.  Fit is done with METHOD equal to :ABS or :SQUARE.

Returns (VALUES PARAMS-BEST VALUE-BEST N-EVALUATIONS)"

  (declare (type (function (double-float (simple-array double-float (*))) double-float)
		 func)
	   (type (member :square :abs) method)
	   ;; these are SINGLE float vectors
	   (type (simple-array single-float (*)) r-vec flux-vec flux-err-vec)
	   (type (simple-array double-float (*)) params)
	   (type (simple-array t (*)) optim-flags)
	   (optimize speed))
  
  (let* ((nparams (length params))
	 (ps (powell:build-powell-struct nparams :optim-flags optim-flags :scale 1.0d-10)))
    (flet ((powfunc (ps)
	     (declare (type powell:powell-struct ps))
	     (setf (powell:powell-struct-y ps)
		   (funcall #'%compute-goodness-of-fit-for-profile
			    func
			    (powell:powell-struct-x-vec ps) ;; params
			    r-vec flux-vec flux-err-vec
			    method))
	     nil))
			    
      (powell:init-powell-struct ps #'powfunc params)
      (powell:run-powell-on-ps #'powfunc ps ftol))
    (values
     (powell:powell-struct-x-vec ps)
     (powell:powell-struct-y ps)
     (powell:powell-struct-neval ps))))



(defun fit-1d-profile/gaussian (r-vec flux-vec flux-err-vec
				&key
				  (norm0 nil)
				  (backd0 nil)
				  (sigma0 nil) 
				  (fix-backd nil)
				  (method :square)
				  (ftol 1d-10))
  "Fit a 1d profile to a 2D Gaussian function (ie, normalized with 1/(2 pi sigma^2), 
returning a (VALUES #(BACKD NORM SIGMA) BEST-FIT-PENALTY NUMBER-EVALUATIONS)"
  ;; set initial guesses if not given
  (multiple-value-bind (backd-est norm-est hwhm-est)
      (%estimate-1d-profile-backd+norm+fwhm r-vec flux-vec)
    (setf norm0 (or norm0 norm-est))
    (if (not fix-backd)
	(setf backd0 (or backd0 backd-est))
	(when (not backd0) (error "FIX-BACKD=T but BACKD0 is not given.")))
    (setf sigma0 (or sigma0 hwhm-est)))
  
  (let ((params (make-array 3 :element-type 'double-float
			      :initial-contents (list (float backd0 1d0)
						      (float norm0 1d0)
						      (float sigma0 1d0))))
	(optim-flags (vector (not fix-backd) t t)))
    (fit-1d-profile
     (lambda (r params)
       (declare (type double-float r)
		(type (simple-array double-float (3)) params))
       (let ((backd (aref params 0))
	     (norm  (aref params 1))
	     (sigma (aref params 2)))
	  (+ backd
	     (* (/ norm (* (* 2 pi)) sigma sigma) ;; 2D gaussian, not 1D, so 1/(2pi sigma^2)
		(exp (* -0.5d0 (expt (/ r sigma) 2)))))))
     r-vec flux-vec flux-err-vec
     params optim-flags
     :method method
     :ftol ftol)))


(defun fit-1d-profile/moffat (r-vec flux-vec flux-err-vec
			      &key
				(backd0 nil)
				(norm0 nil)
				(a0 nil)
				(beta0 2.5)
				(fix-backd nil)
				(fix-beta nil)
				(method :square)
				(ftol 1d-10))
  "Fit a 1d profile to a mofatt function, returning 
(VALUES #(BACKD NORM A BETA) BEST-FIT-PENALTY NUMBER-EVALUATIONS)"
  (when (not (realp beta0)) (error "BETA0 must be given (or default used); it cannot be estimated."))
  ;; set initial guesses if not given
  (multiple-value-bind (backd-est norm-est hwhm-est)
      (%estimate-1d-profile-backd+norm+fwhm r-vec flux-vec)
    (setf norm0 (or norm0 norm-est))
    (if (not fix-backd)
	(setf backd0 (or backd0 backd-est))
	(when (not backd0) (error "FIX-BACKD=T but BACKD0 is not given.")))
    (setf a0 (or a0
		 (moffat-a-for-fwhm (* 2d0 hwhm-est) beta0))))
  ;;
  (let ((params (make-array 4 :element-type 'double-float
			      :initial-contents (list (float backd0 1d0)
						      (float norm0 1d0)
						      (float a0 1d0)
						      (float beta0  1d0))))
	(optim-flags (vector (not fix-backd) t t (not fix-beta))))
    (fit-1d-profile
     (lambda (r params)
       (declare (type double-float r)
		(type (simple-array double-float (4)) params))
       (let ((backd (aref params 0))
	     (norm (aref params 1))
	     (a (aref params 2))
	     (beta (aref params 3)))
	  (+ backd
	     (* norm
		(/ (- beta 1d0)
		   (* pi a a))
		(expt (+ 1d0 (expt (/ r a) 2)) (- beta))))))
     r-vec flux-vec flux-err-vec
     params optim-flags
     :method method
     :ftol ftol)))


(defun fit-1d-profile/moffat+1/r  (r-vec flux-vec flux-err-vec
				   &key
				     (backd0 nil)
				     (norm0 nil)
				     (a0 nil)
				     (beta 2.5)
				     (fix-backd nil)
				     (method :square)
				     (ftol 1d-10))
  "Fit a profile consisting of a fixed-BETA moffatt plus a 1/r mofatt with the same
FWHM - this is designed as comet detctor.

Ie, fits   (1-f)*Moffat(r,a;beta) + f*Coma(r;MofattFWHM(a,beta))

BETA is always fixed to the input value, which defaults to 2.5.

Returns (VALUES  #(BACKD NORM A BETA FRAC-COMA) BEST-FIT-PENALTY NUMBER-EVALUATIONS)

where FRAC-COMA is the fraction of the 1/r Moffatt term."

  (when (not (realp beta)) (error "BETA must be given (or default used); it cannot be estimated."))
  ;; set initial guesses if not given
  (multiple-value-bind (backd-est norm-est hwhm-est)
      (%estimate-1d-profile-backd+norm+fwhm r-vec flux-vec)
    (setf norm0 (or norm0 norm-est))
    (if (not fix-backd)
	(setf backd0 (or backd0 backd-est))
	(when (not backd0) (error "FIX-BACKD=T but BACKD0 is not given.")))
    (setf a0 (or a0 hwhm-est)))
  ;;
  (let ((params (make-array 5 :element-type 'double-float
			      :initial-contents (list (float backd0 1d0)
						      (float norm0 1d0)
						      (float a0 1d0)
						      (float beta  1d0)
						      0.1d0))) ;; fraction of coma
	(optim-flags (vector (not fix-backd) t t nil t))
	(rmax (* 1d0 (aref r-vec (1- (length r-vec))))))
    (fit-1d-profile
     (lambda (r params)
       (declare (type double-float r)
		(type (simple-array double-float (5)) params)
		#+nil (optimize speed))
       (let* ((backd (aref params 0))
	      (norm (aref params 1))
	      (a/n (aref params 2))    ;; a of nucleus
	      (beta/n (aref params 3)) ;; beta nucleus
	      (fwhm/n (moffat-fwhm a/n beta/n)) ;; fwhm nucleus
	      (f (aref params 4)))
	 (+ backd
	    (* norm
	       (+
		;; coma component
		(* f
		   (1/r-coma-func r fwhm/n rmax))
		;; nucleus component
		(* (- 1d0 f)
		   (/ (- beta/n 1d0)
		      (* pi a/n a/n))
		   (expt (+ 1d0 (expt (/ r a/n) 2)) (- beta/n))))))))
     r-vec flux-vec flux-err-vec
     params optim-flags
     :method method
     :ftol ftol)))




#|
(defparameter *r-test* (make-array 100 :element-type 'single-float))
(defparameter *f-test* (make-array 100 :element-type 'single-float))
(defparameter *ef-test* (make-array 100 :element-type 'single-float))
(loop ;with sigma = 10d0
      with backd = 0d0
      with norm = 3d0
      for i below 100
      for r = (float i 1d0)
      for f = #+nil (+ backd
		       (* (/ norm (*  (* 2 pi) sigma sigma)) ;; this is *2D* normalization
			  (exp (* -0.5d0 (expt (/ r sigma) 2)))))
      (moffat r 10.0 2.5 :backd backd :norm norm)
      for ef = 1d0
      do (setf (aref *r-test* i) (float r 1.0))
	 (setf (aref *f-test* i) (float f 1.0))
	 (setf (aref *ef-test* i) (float ef 1.0)))
  
|# 


