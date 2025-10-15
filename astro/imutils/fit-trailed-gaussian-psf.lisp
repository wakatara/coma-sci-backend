
#|
 

Fit a trailed Gaussians to images

The main function is:  

  (FIT-TRAILED-GAUSSIAN (IMAGE NX0 NY0 &KEY  ..)


Possible improvement: bound the trail function by a rectangle (rotated)
and perform evaluation only inside rectangle, for a speed improvement.

This could be done using a polyfill algorithm that instead of filling
pixels, fills arrays YVEC XMINVEC XMAXVEC, so that the actual
evaluator steps along YVEC[i], then for each Y steps from XMIN[i] to
YMIN[i] 


|#

(in-package imutils)

;; define floating point erf - assume that libmath is loaded already
(declaim (inline erf))
(cffi:defcfun erf :double
  (x :double))


;; trailed gaussian equation, Eq 3 Veres https://arxiv.org/pdf/1209.6106.pdf
;; but it seems there's a sign error in the erfs
;;
;; theta is position angle, counterclockwise from +Y into +X
(declaim (inline %trailed-gaussian-function-raw))
(defun %trailed-gaussian-function-raw (flux trail-length x y x0 y0 sigma
				       cos-theta sin-theta)
  (declare (type double-float flux trail-length x y x0 y0 sigma sin-theta cos-theta))
  (let* ((xx (- (* -1  sin-theta (- x x0))
		(*     cos-theta (- y y0))))
	 (yy (+ (*     cos-theta (- x x0))
		(* -1  sin-theta (- y y0))))
	 (sigma-sqrt2 (* sigma 1.4142135623730951d0))
	 (sqrt2pi 2.5066282746310002d0)
	 (norm (/ flux (* trail-length 2 sigma sqrt2pi)))
	 (trail/2 (* 0.5d0 trail-length)))

    (* norm
       (exp (/  (* -1 yy yy)
		(* 2 sigma sigma)))
       (-
	(erf (/ (+ xx trail/2) sigma-sqrt2))
	(erf (/ (- xx trail/2) sigma-sqrt2))))))


;; the efficient version doesn't waste time on regions where the exponent is nearly zero
(declaim (inline %trailed-gaussian-function-raw+efficient))
(defun %trailed-gaussian-function-raw+efficient 
    (flux trail-length x y x0 y0 sigma
     cos-theta sin-theta)
  (declare (type double-float flux trail-length x y x0 y0 sigma sin-theta cos-theta))
  (let* ((dx (- x x0))
	 (dy (- y y0))
	 (yy (+ (*     cos-theta dx)
		(* -1  sin-theta dy)))
	 (eterm (/  (* -1 yy yy)
		    (* 2 sigma sigma))))
    (if (< eterm -12d0) ;; exponential term is extremely small (1e-5) times peak
	0d0
	(let* ((xx (- (* -1  sin-theta dx) ;; deferred xx
		      (*     cos-theta dy)))
	       (sigma-sqrt2 (* sigma 1.4142135623730951d0))
	       (sqrt2pi 2.5066282746310002d0)
	       (norm (/ flux (* trail-length 2 sigma sqrt2pi)))
	       (trail/2 (* 0.5d0 trail-length)))
	  
	  (* norm
	     (exp eterm)
	     (-
	      (erf (/ (+ xx trail/2) sigma-sqrt2))
	      (erf (/ (- xx trail/2) sigma-sqrt2))))))))


;; a version of trailed gaussian for general use
(defun trailed-gaussian-function (flux trail-length x y x0 y0 sigma theta)
    "A function representing a trailed round Gaussian; theta is the position
with zero in +y, with increasing theta rotating into +x"
  (float
   (%trailed-gaussian-function-raw
    (* 1d0 flux)    (* 1d0 trail-length) (* 1d0 x) (* 1d0 y)
    (* 1d0 x0)  (* 1d0 y0) (* 1d0  sigma)
    (cos (* (/ pi 180) theta))
    (sin (* (/ pi 180) theta)))
   1.0))





;; theta will be a normal position angle of long axis
(defun %fit-trailed-gaussian/square
    (image nx0 ny0
     &key 
       (error-image nil)
       (size 21) ;; must be odd
       ;; initial guesses
       (theta0 0.0)
       (sigma0 1.0) 
       (tlength0 1.0) ;; trail length
       (freeze-theta nil)
       (freeze-tlength nil)
       (freeze-sigma nil)
       (backd nil)
       (ftol 1e-6)
       (subpix 1))
  (declare (type image image)
	   (type (or null image) error-image)
	   (type (single-float -1e10 1e10)  theta0 sigma0 tlength0)
	   (type (or null (single-float -1e10 1e10)) backd)
	   (type (unsigned-byte 15) size nx0 ny0)
	   (type (member 1 3 5) subpix))

  ;;
  ;; fitting region
  (let* ((size/2 (ash size -1))
	 (x0 (float nx0 1.0))
	 (y0 (float ny0 1.0))
	 (ix0  (max 0 (- nx0 size/2)))
	 (ix1  (min (+  nx0 size/2) (1- (array-dimension image 1))))
	 (iy0  (max 0 (- ny0 size/2)))
	 (iy1  (min (+  ny0 size/2) (1- (array-dimension image 0))))
	 ;;
	 (ps (powell:build-powell-struct 5 :name "fit-trailed-gaussian"))
	 (backd-fixed (if backd (float backd 1d0) 0d0))
	 ;; storage for backd and norm computed by fit
	 (nb-vec (make-array 2 :element-type 'double-float))
	 ;; fit just sigma,,theta - backd and norm are analytic
	 (pvec-initial  (make-array 5 :element-type 'double-float))
	 (optim-flags   (make-array 5 :initial-contents
				    (list t t
					  (not freeze-theta)
					  (not freeze-sigma)
					  (not freeze-tlength)))))


   (declare (type (unsigned-byte 28) size/2 nx0 ix1 iy0 iy1)
	    (type double-float backd-fixed)
	    (type (simple-array double-float (*)) nb-vec pvec-initial))

    
    (setf (aref pvec-initial 0) (float x0 1d0)
	  (aref pvec-initial 1) (float y0 1d0)
	  (aref pvec-initial 2) (float (* (/ pi 180) theta0 1d0)) ;; to radians
	  (aref pvec-initial 3) (float sigma0 1d0)
	  (aref pvec-initial 4) (float tlength0 1d0))

    
    (locally
	(declare (type (unsigned-byte 28) size/2 nx0 ix1 iy0 iy1)
		 (optimize speed))
      ;;
      (labels ((eval-penalty (x0 y0 theta sigma tlength)
		 (declare (type double-float x0 y0 theta sigma tlength))
		 (loop
		   with sf  of-type double-float = 0d0
		   with sff of-type double-float = 0d0
		   with sz  of-type double-float = 0d0
		   with szz of-type double-float = 0d0
		   with sfz of-type double-float = 0d0
		   with sn  of-type double-float = 0d0
		   ;; move cosine, sin out of loop
		   with ct of-type double-float = (cos theta)
		   with st of-type double-float = (sin theta)
		   ;; if distance too far away, penalize it
		   with dist-penalty of-type double-float
		     = (+ (expt (- x0 nx0) 4)
			  (expt (- y0 ny0) 4))		   
		   ;; if theta goes too far away from some multiple of
		   ;; pi, clobber it
		   with theta-penalty of-type double-float
		     = (if (> (abs theta) 10d0)
			   (* 1e4 (expt (- (abs theta) 10d0) 2))
			   0d0)
		   for ix of-type (unsigned-byte 28) from ix0 to ix1
		   for x of-type double-float = (- ix x0)
		   do
		      (loop
			for iy of-type (unsigned-byte 28) from iy0 to iy1
			for y of-type double-float = (- iy y0)
			for z of-type double-float = (float (aref image iy ix) 1d0)
			;; inverse of error at this pix (inv to multiply rather than divide)
			for zeinv of-type double-float
			  = (if error-image (/ 1d0 (float (aref error-image iy ix) 1d0)) 1d0)
			;; sigma=0 can cause a divide by zero
			for sigma-fixed of-type double-float = (sqrt (+ (* sigma sigma)
									(* 0.1d0 0.1d0)))
			for predicted-val of-type double-float
			  = (subpixel-quadrature
			     x y xx yy subpix ;;x,y are center and xx,yy wander over pix
			     ;; expression that is integrated over a pixel
			     (%trailed-gaussian-function-raw+efficient
			      1d0 (abs tlength) xx yy 0d0 0d0 sigma-fixed
			      ct st)
			     :float-type double-float)
			do 
			   (incf sf (* zeinv predicted-val))
			   (incf sff (expt (* zeinv predicted-val) 2))
			   (incf sz  (* zeinv z))
			   (incf szz (expt (* zeinv z) 2))
			   (incf sfz  (* zeinv zeinv z predicted-val))
			   (incf sn (* zeinv zeinv))) ;; coeff of const term, or 1/sigmaz^2
		   finally
		      (let ((a 0d0)
			    (c 0d0)
			    (penalty 0d0))
			(declare (type double-float a c penalty))
			(cond ((not backd) ;; not using frozen user input backd
			       (multiple-value-setq (a c)
				 (2x2-lin-solve-macro sf sn sff sf sz sfz)))
			      (t
			       (setf c backd-fixed)
			       (setf a (/ (- sfz (* c sf)) sff))))
			(setf penalty
			      (+ szz (* c c sn) (* a a sff)
				 (* 2 
				    (+ (* -1 c sz) (* a c sf) (* -1 a sfz)))))
			(setf (aref nb-vec 0) a)
			(setf (aref nb-vec 1) c)
			(return (+ theta-penalty dist-penalty penalty)))))
	       ;;
	       (eval-penalty-ps (ps)
		 (declare (type powell:powell-struct ps))
		 (let ((pvec (powell:powell-struct-x-vec ps)))
		   (declare (type (simple-array double-float *) pvec))
		   
		   (setf (powell:powell-struct-y ps)
			   (eval-penalty (aref pvec 0) (aref pvec 1) (aref pvec 2)
					 (aref pvec 3) (aref pvec 4)))
		   t)))

	(declare (inline eval-penalty))

	;; dimensions are x0 y0 theta a b norm backd
	(powell:init-powell-struct ps #'eval-penalty-ps pvec-initial :optim-flags optim-flags)
	;; freeze backd if requested
	(powell:run-powell-on-ps #'eval-penalty-ps ps ftol)
	(eval-penalty-ps ps)))
    
    ;; now convert the results back to single precision because
    ;; that's what imutils generally operates in
    (let ((x-vec (make-array 7 :element-type 'single-float))
	  (ps-x-vec (powell:powell-struct-x-vec ps))
	  (y (float (powell:powell-struct-y ps) 1.0)))
      (loop for i below 5 do (setf (aref x-vec i)
				   (float (aref ps-x-vec i) 1.0)))
      ;; convert theta to deg
      (setf (aref x-vec 2) (float (* (aref ps-x-vec 2) (/ 180 pi)) 1.0))

      ;; set sigma, trail length to their abs values
      (setf (aref x-vec 3) (abs  (aref x-vec 3)))
      (setf (aref x-vec 4) (abs  (aref x-vec 4)))
      ;;
      ;; put theta into range -90 to 90
      (let ((theta (aref x-vec 2)))
	;; first into -180 to 180
	(setf theta (nth-value 1 (round theta 360)))
	(when (> theta 180) (decf theta 360))
	;; then bump by +/- 180 if below -90 or above  +90
	(cond ((< theta -90) (incf theta 180))
	      ((> theta 90)  (decf theta 180)))
	(setf (aref x-vec 2) theta))
      
      (setf (aref x-vec 5) (float (aref nb-vec 0) 1.0))
      (setf (aref x-vec 6) (float (aref nb-vec 1) 1.0))

      (values x-vec
	      y
	      (powell:powell-struct-neval ps)))))
    





    
;; theta will be a normal position angle of long axis
(defun %fit-trailed-gaussian/abs
    (image nx0 ny0
     &key 
       (error-image nil)
       (size 21) ;; must be odd
       ;; initial guesses
       (theta0 0.0)
       (sigma0 1.0) 
       (tlength0 1.0)
       (freeze-theta nil)
       (freeze-tlength nil)
       (freeze-sigma nil)
       (backd nil)
       (ftol 1e-6)
       (subpix 1))

  (declare (type image image)
	   (type (or null image) error-image)
	   (type (single-float -1e6 1e6)  theta0 sigma0 tlength0)
	   (type (or null (single-float -1e6 1e6)) backd)
	   (type (unsigned-byte 15) size nx0 ny0)
	   (type (member 1 3 5) subpix))
  ;;
  ;; fitting region
  (let* ((size/2 (ash size -1))
	 (x0 (float nx0 1.0))
	 (y0 (float ny0 1.0))
	 (ix0  (max 0 (- nx0 size/2)))
	 (ix1  (min (+  nx0 size/2) (1- (array-dimension image 1))))
	 (iy0  (max 0 (- ny0 size/2)))
	 (iy1  (min (+  ny0 size/2) (1- (array-dimension image 0))))
	 ;;
	 (ps (powell:build-powell-struct 7 :name "fit-trailed-gaussian"))
	 (backd0 0d0)
	 (norm0 0d0) ;; put in better guess below
	 (pvec-initial (make-array 7 :element-type 'double-float))
	 (optim-flags   (make-array 7 :initial-contents
				    (list t t
					  (not freeze-theta)
					  (not freeze-sigma)
					  (not freeze-tlength)
					  t
					  t))))



    ;; estimate initial values for norm and backd 
    (setf backd0
	  (* 1d0
	     (imutils:image-median  image :ix0 ix0 :ix1 ix1 
					  :iy0 iy0 :iy1 iy1)))
    (loop
      for ix of-type (unsigned-byte 28) from ix0 to ix1
      do
	 (loop for iy of-type (unsigned-byte 28) from iy0 to iy1
	       do
		  (incf norm0 (* 1.0 (- (aref image iy ix) backd0)))))

    
    (setf (aref pvec-initial 0) (float x0 1d0)
	  (aref pvec-initial 1) (float y0 1d0)
	  (aref pvec-initial 2) (float (* (/ pi 180) theta0 1d0)) ;; to radians
	  (aref pvec-initial 3) (float sigma0 1d0)
	  (aref pvec-initial 4) (float tlength0 1d0)
	  (aref pvec-initial 5) (float norm0 1d0)
	  (aref pvec-initial 6) (float backd0 1d0))
     
    (locally
	(declare (type (unsigned-byte 28) size/2 nx0 ix1 iy0 iy1)
		 (type double-float backd0 norm0)
		 (optimize speed))
      ;;
      (labels ((eval-penalty (x0 y0 theta sigma tlength norm backd)
		 (loop 
		    ;; move LT (leading term of gaussian) out of loop
		    with ct of-type double-float = (cos theta)
		    with st of-type double-float = (sin theta)
		    with penalty of-type double-float = 0d0
		    ;; if distance too far away, penalize it
		    with dist-penalty of-type double-float
		      = (+ (expt (- x0 nx0) 4)
			   (expt (- y0 ny0) 4))	
		    ;; if theta goes too far away from some multiple of
		    ;; pi, clobber it
		    with theta-penalty of-type double-float = 
		      (if (> (abs theta) 10d0)
			  (* 1e4 (expt (- (abs theta) 10d0) 2))
			  0d0)

		    for ix of-type (unsigned-byte 28) from ix0 to ix1
		    for x of-type double-float = (- ix x0)
		    do
		    (loop
		       for iy of-type (unsigned-byte 28) from iy0 to iy1
		       for y of-type double-float = (- iy y0)
		       ;; inverse of error at this pix (inv to multiply rather than divide)
		       for zeinv of-type double-float 
			 = (if error-image (/ 1d0 (float (aref error-image iy ix) 1d0)) 1d0)
		       ;; sigma=0 can cause a divide by zero
		       for sigma-fixed of-type double-float = (sqrt (+ (* sigma sigma)
								       (* 0.1d0 0.1d0)))			      
		       for predicted-val of-type double-float = 
			 (subpixel-quadrature
			  x y xx yy subpix ;;x,y are center and xx,yy wander over pix
			  ;; expression that is integrated over a pixel
			  (- (%trailed-gaussian-function-raw+efficient
			      (* 1d0 norm) (abs tlength) xx yy 0d0 0d0 sigma-fixed
			      ct st)
			     backd)
			  :float-type double-float)
		       for dif of-type double-float 
			 = (- predicted-val (aref image iy ix))
		       do 
			 (incf penalty (* zeinv (abs dif))))
		    finally
		    (return (+ theta-penalty dist-penalty penalty))))
	       ;;
	       (eval-penalty-ps (ps)
		 (declare (type powell:powell-struct ps))
		 (let ((pvec (powell:powell-struct-x-vec ps)))
		   (declare (type (simple-array double-float *) pvec))
		   (setf (powell:powell-struct-y ps)
			 (eval-penalty (aref pvec 0) (aref pvec 1) (aref pvec 2)
				       (aref pvec 3) (aref pvec 4) (aref pvec 5)
				       (aref pvec 6)))
		   t)))
	


	;; dimensions are x0 y0 theta a b norm backd
	(powell:init-powell-struct ps #'eval-penalty-ps pvec-initial :optim-flags optim-flags)
	;; freeze backd if requested
	(when backd (setf (aref (powell:powell-struct-optim-flags ps) 6) nil))
	(powell:run-powell-on-ps #'eval-penalty-ps ps ftol)))
    
    ;; now convert the results back to single precision because
    ;; that's what imutils generally operates in
    (let ((x-vec (make-array 7 :element-type 'single-float))
	  (ps-x-vec (powell:powell-struct-x-vec ps))
	  (y (float (powell:powell-struct-y ps) 1.0)))
      (loop for i below 7 do (setf (aref x-vec i)
				   (float (aref ps-x-vec i) 1.0)))
      ;; convert theta to deg
      (setf (aref x-vec 2) (float (* (aref ps-x-vec 2) (/ 180 pi)) 1.0))

      ;; set sigma, trail-length to their abs values
      (setf (aref x-vec 3) (abs  (aref x-vec 3)))
      (setf (aref x-vec 4) (abs  (aref x-vec 4)))
      ;;
      ;; put theta into range -90 to 90
      (let ((theta (aref x-vec 2)))
	;; first into -180 to 180
	(setf theta (nth-value 1 (round theta 360)))
	(when (> theta 180) (decf theta 360))
	;; then bump by +/- 180 if below -90 or above  +90
	(cond ((< theta -90) (incf theta 180))
	      ((> theta 90)  (decf theta 180)))
	(setf (aref x-vec 2) theta))
	

      (values x-vec
	      y
	      (powell:powell-struct-neval ps)))))
    

;; theta will be a normal position angle of long axis
(defun fit-trailed-gaussian (image nx0 ny0
			     &key 
			       (error-image nil)
			       (size 21) ;; must be odd
			       ;; initial guesses
			       (theta0 0.0)
			       (sigma0 1.0) 
			       (tlength0 1.0)
			       (freeze-theta nil)
			       (freeze-tlength nil)
			       (freeze-sigma nil)
			       (backd nil)
			       (ftol 1e-6)
			       (subpix 1)
			       (metric :square))
"Fit a trailed Gaussian to an image starting at NX0,NY0 in a box of
SIZE pixels around it.

Returns (VALUES #(X0 Y0 THETA SIGMA TLENGTH NORM BACKD)
                PENALTY-VALUE
                NUMBER-OF-EVALUATIONS)

The outputs are

   X0,Y0   - central fractional pixel index
   THETA   - angle in deg, shifted to [-90,90], where THETA=0
             points +Y, and rotates into +X
   SIGMA   - gaussian sigma
   TLENGTH - trail length
   BACKD   - additive background value
   NORM    - the overall normalization term, in addition 
             to the 1/SQRT(2*PI*A*B) term already assumed 
             for the Gaussian.  NORM is the area under the Gaussian.

ERROR-IMAGE is an optional error image. For a square fit, it is the
one-sigma error for each pixel.  For an ABS absolute value fit, each
pixels penalty term is divided by this value.

THETA0, SIGMA0, TLENGTH0 are the starting guesses for the angle, sigma, 
and trail length.  FTOL is a fractional numerical tolerance that doesn't seem
to matter much.  

FREEZE-THETA, FREEZE-SIGMA, FREEZE-TLENGTH will freeze the values
at the THETA0, etc given. This is useful for fitting many trails,
when the X0,Y0 is to be determined for known THETA,SIGMA,TLENGTH

If BACKD is given, the background is frozen at this value, and background
is not fit.

SUBPIX is 1,3,5, and tells how many sub-intervals to use in the quadrature
rule.  1 means use the pixel center.  3 is an inside rule, 5 is outside.

METRIC is :SQUARE for a square-deviation penalty, and :ABS for
absolute value.
"
  (declare (type image image)
	   (type (or null image) error-image)
	   (type (single-float -1e6 1e6)  theta0 sigma0 tlength0)
	   (type (or null (single-float -1e6 1e6)) backd)
	   (type (unsigned-byte 15) size nx0 ny0)
	   (type (member 1 3 5) subpix)
	   (type (member :square :abs) metric))
  ;;
  (cond ((eq metric :square)
	 (%fit-trailed-gaussian/square
	  image nx0 ny0  
	  :error-image error-image
	  :size size :sigma0 sigma0
	  :tlength0 tlength0
	  :theta0 theta0
	  :freeze-theta freeze-theta
	  :freeze-sigma freeze-sigma
	  :freeze-tlength freeze-tlength
	  :backd backd :ftol ftol 
	  :subpix subpix))
	((eq metric :abs)
	 (%fit-trailed-gaussian/abs
	  image nx0 ny0  
	  :error-image error-image
	  :size size :sigma0 sigma0
	  :tlength0 tlength0
	  :theta0 theta0
	  :freeze-theta freeze-theta
	  :freeze-sigma freeze-sigma
	  :freeze-tlength freeze-tlength
	  :backd backd :ftol ftol 
	  :subpix subpix))))

    
    
    
