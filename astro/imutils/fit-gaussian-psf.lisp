  
#|


Fit round and elliptical Gaussians to images.

Functions are

 (FIT-ROUND-GAUSSIAN IMAGE NX0 NY0 &KEY ...)
 
 (FIT-2AXIS-GAUSSIAN IMAGE NX0 NY0 &KEY ...)

--- notes ---

Interior quadrature is not as efficient as it could be, because
values of fit function at pixel boundaries are needlessly recomputed.

Fits the normalization and the background analytically for the :SQUARE
penalty type, which reduces the number of fit parameters by 2,
so the 2axis gaussian has just 5 parameters.    This means that for
each fit type, there are two functions, one ABS, and one SQUARE

Error image functionality is not tested.

|#

(in-package imutils)




(defun %fit-round-gaussian/square
    (image nx0 ny0 &key 
     (error-image nil)
     (size 21) ;; must be odd
     (sigma0 1.0) 
     (backd nil) ;; 
     (ftol 1e-6)
     (subpix 1))
  (declare (type image image)
	   (type (or null image) error-image)
	   (type (single-float -1e6 1e6)  sigma0)
	   (type (or null (single-float -1e6 1e6)) backd)
	   (type (unsigned-byte 15) size nx0 ny0)
	   (type (member 1 3 5) subpix))
  ;;
  ;; fitting region
  (let* ((size/2 (ash size -1))
	 (ix0  (max 0 (- nx0 size/2)))
	 (ix1  (min (+  nx0 size/2) (1- (array-dimension image 1))))
	 (iy0  (max 0 (- ny0 size/2)))
	 (iy1  (min (+  ny0 size/2) (1- (array-dimension image 0))))
	 ;;
	 (ps (powell:build-powell-struct 3 :name "fit-gaussian"))
	 (backd-fixed (if backd (float backd 1d0) 0d0))
	 ;; storage for backd and norm computed by fit
	 ;; the could be doubles, but it conses for some reason
	 (nb-vec (make-array 2 :element-type 'double-float))
	 ;; fit just a,b,theta - backd and norm are analytic
	 (pvec-initial (make-array 3 :element-type 'double-float)))
    (declare (type (unsigned-byte 28) size/2 nx0 ix1 iy0 iy1)
	     (type double-float backd-fixed)
	     (type (simple-array double-float (*)) nb-vec pvec-initial))


    (setf (aref pvec-initial 0) (float nx0 1d0)
	  (aref pvec-initial 1) (float ny0 1d0)
	  (aref pvec-initial 2) (float sigma0 1d0))
    
    (locally
	(declare (optimize speed))
      ;;
      (labels ((eval-penalty (x0 y0 sigma)
		 (declare (optimize debug))
		 (loop 
		    ;; various sum terms
		    with sf  of-type double-float = 0d0
		    with sff of-type double-float = 1d-100 ;; prevent some rare 1/0 error
		    with sz  of-type double-float = 0d0
		    with szz of-type double-float = 1d-100
		    with sfz of-type double-float = 0d0
		    with sn  of-type double-float = 0d0
		    ;;
		    with sigma2safe of-type (double-float 1d-8) = (+ 1d-8 (* sigma sigma))
		    with x0 = (float x0 1d0)
		    with y0 = (float y0 1d0)
		    with 1/s2 of-type double-float = 
		      (/ 1.0 sigma2safe)
		    ;; move LT (leading term of gaussian) out of loop
		    with lt of-type double-float =  
		      (/ 1d0   (* sigma2safe 2 pi))
		    for ix of-type (unsigned-byte 28) from ix0 to ix1
		    for x of-type double-float =  (- ix x0)
		    do
		    (loop
		       for iy of-type (unsigned-byte 28) from iy0 to iy1
		       for y of-type double-float = (- iy y0)
		       for z of-type double-float = (float (aref image iy ix) 1d0)
		       ;; inverse of error at this pix (inv to multiply rather than divide)
		       for zeinv of-type double-float =
			 (if error-image (/ 1d0 (float (aref error-image iy ix) 1d0)) 1d0)
		       ;;
		       for predicted-val of-type double-float = 
			 (subpixel-quadrature
			  x y xx yy subpix ;;x,y are center and xx,yy wander over pix
			  ;; expression that is integrated over a pixel
			  (let ((r2 (+ (expt xx 2)
				       (expt yy 2))))
			    (* lt 
			       (exp (* -0.5d0 (* r2 1/s2)))))
			  :float-type double-float)
		       ;;
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
		      ;;(setf backd-fit c)
		      ;;(setf norm-fit a)
		      (setf (aref nb-vec 0) a)
		      (setf (aref nb-vec 1) c)
		      (return penalty))))
	       ;;
	       (eval-penalty-ps (ps)
		 (declare (type powell:powell-struct ps))
		 (let ((pvec (powell:powell-struct-x-vec ps)))
		   (declare (type (simple-array double-float *) pvec))
		   (setf (powell:powell-struct-y ps)
			 (eval-penalty (aref pvec 0) (aref pvec 1) (aref pvec 2)))
		   t)))
	;;
	;; dimensions are x0 y0 sigma norm backd
	(powell:init-powell-struct ps #'eval-penalty-ps pvec-initial)
	(powell:run-powell-on-ps #'eval-penalty-ps ps ftol)
	(eval-penalty-ps ps)))
      
    
    
    ;; now convert the results back to single precision because
    ;; that's what imutils generally operates in
    (let ((x-vec (make-array 5 :element-type 'single-float))
	  (ps-x-vec (powell:powell-struct-x-vec ps))
	  (y (float (powell:powell-struct-y ps) 1.0)))
      (loop for i below 3 do (setf (aref x-vec i)
				   (float (aref ps-x-vec i) 1.0)))
      (setf (aref x-vec 3) (float (aref nb-vec 0) 1.0))
      (setf (aref x-vec 4) (float (aref nb-vec 1) 1.0))

      ;; set sigma to its abs value
      (setf (aref x-vec 2) (abs  (aref x-vec 2)))

      (values x-vec
	      y
	      (powell:powell-struct-neval ps)))))
    

(defun %fit-round-gaussian/abs 
    (image nx0 ny0 &key 
     (error-image nil)
     (size 21) ;; must be odd
     (sigma0 1.0) 
     (backd nil) ;; 
     (ftol 1e-6)
     (subpix 1))
  (declare (type image image)
	   (type (or null image) error-image)
	   (type (single-float -1e6 1e6)  sigma0)
	   (type (or null (single-float -1e6 1e6)) backd)
	   (type (unsigned-byte 15) size nx0 ny0)
	   (type (member 1 3 5) subpix))
  ;;
  ;; fitting region
  (let* ((size/2 (ash size -1))
	 (ix0  (max 0 (- nx0 size/2)))
	 (ix1  (min (+  nx0 size/2) (1- (array-dimension image 1))))
	 (iy0  (max 0 (- ny0 size/2)))
	 (iy1  (min (+  ny0 size/2) (1- (array-dimension image 0))))
	 ;;
	 (ps (powell:build-powell-struct 5 :name "fit-gaussian"))
	 (backd0 (* 1d0
		    (or backd
			(imutils:image-median  image :ix0 ix0 :ix1 ix1 
					       :iy0 iy0 :iy1 iy1)))) ;; first guess
	 (norm0 0d0) ;; put in better guess below
	 (pvec-initial (make-array 5 :element-type 'double-float)))

    ;; take a guess at the norm by summing pixels over box and subtracting
    ;; the backd computed above
    (loop
       for ix of-type (unsigned-byte 28) from ix0 to ix1
       do
	 (loop for iy of-type (unsigned-byte 28) from iy0 to iy1
	      do
	      (incf norm0 (* 1.0 (- (aref image iy ix) backd0)))))
    
    (setf (aref pvec-initial 0) (float nx0 1d0)
	  (aref pvec-initial 1) (float ny0 1d0)
	  (aref pvec-initial 2) (float sigma0 1d0)
	  (aref pvec-initial 3) (float norm0 1d0)
	  (aref pvec-initial 4) (float backd0 1d0))
    
    (locally
	(declare (type (unsigned-byte 28) size/2 nx0 ix1 iy0 iy1)
		 (type double-float backd0 norm0)
		 (optimize speed))
      ;;
      (labels ((eval-penalty (x0 y0 sigma norm backd)
		 (loop 
		    with x0 = (float x0 1d0)
		    with y0 = (float y0 1d0)
		    with sigma2safe of-type (double-float 1d-8) = (+ 1d-8 (* sigma sigma))
		    with 1/s2 of-type double-float = 
		      (/ 1.0 sigma2safe)
		    ;; move LT (leading term of gaussian) out of loop
		    with lt of-type double-float =  
		      (/ norm  (* sigma2safe 2 pi))
		    with penalty of-type double-float = 0d0
		    for ix of-type (unsigned-byte 28) from ix0 to ix1
		    for x of-type double-float =  (- ix x0)
		    do
		    (loop
		       for iy of-type (unsigned-byte 28) from iy0 to iy1
		       for y of-type double-float = (- iy y0)
		       ;; inverse of error at this pix (inv to multiply rather than divide)
		       for zeinv of-type double-float =
			 (if error-image (/ 1d0 (float (aref error-image iy ix) 1d0)) 1d0)
		       ;;
		       for predicted-val of-type double-float = 
			 (subpixel-quadrature
			  x y xx yy subpix ;;x,y are center and xx,yy wander over pix
			  ;; expression that is integrated over a pixel
			  (let ((r2 (+ (expt xx 2)
				       (expt yy 2))))
			    (+ backd
			       (* lt 
				  (exp (* -0.5d0 (* r2 1/s2))))))
			  :float-type double-float)
		       ;;
		       for dif of-type double-float = 
			 (- predicted-val (aref image iy ix))
		       do 
			 (incf penalty (* zeinv (abs dif))))
		    finally
		      (return penalty)))
	       ;;
	       (eval-penalty-ps (ps)
		 (declare (type powell:powell-struct ps))
		 (let ((pvec (powell:powell-struct-x-vec ps)))
		   (declare (type (simple-array double-float *) pvec))
		   (setf (powell:powell-struct-y ps)
			 (eval-penalty (aref pvec 0) (aref pvec 1) (aref pvec 2)
				       (aref pvec 3) (aref pvec 4)))
		   t)))
	;;
	;; dimensions are x0 y0 sigma norm backd
	(powell:init-powell-struct ps #'eval-penalty-ps pvec-initial)
	;; freeze backd if requested
	(when backd (setf (aref (powell:powell-struct-optim-flags ps) 4) nil))	
	(powell:run-powell-on-ps #'eval-penalty-ps ps ftol)))
    
    ;; now convert the results back to single precision because
    ;; that's what imutils generally operates in
    (let ((x-vec (make-array 5 :element-type 'single-float))
	  (ps-x-vec (powell:powell-struct-x-vec ps))
	  (y (float (powell:powell-struct-y ps) 1.0)))
      (loop for i below 5 do (setf (aref x-vec i)
				   (float (aref ps-x-vec i) 1.0)))
      ;; set sigma to its abs value
      (setf (aref x-vec 2) (abs  (aref x-vec 2)))

      (values x-vec
	      y
	      (powell:powell-struct-neval ps)))))
    




;; FIXME - the gaussian function leads to floating point errors.
;; it should be softened to prevent extreme values
(defun fit-round-gaussian (image nx0 ny0 &key 
			   (error-image nil)
			   (size 21) ;; must be odd
			   (sigma0 1.0) 
			   (backd nil) ;; 
			   (ftol 1e-6)
			   (subpix 1)
			   (metric :square))
"Fit a 2d Gaussian to an image starting at NX0,NY0 in a box of SIZE pixels around
it. 

Returns (VALUES #(X0 Y0 SIGMA NORM BACKD)
                PENALTY-VALUE
                NUMBER-OF-EVALUATIONS)

The outputs are

   X0,Y0   - central fractional pixel index
   SIGMA   - sigma of round Gassian
   BACKD   - additive background value
   NORM    - the overall normalization term, in addition 
             to the 1/SQRT(2*PI*A*B) term already assumed 
             for the Gaussian.  NORM is the area under the Gaussian.

ERROR-IMAGE is an optional error image. For a square fit, it is the
one-sigma error for each pixel.  For an ABS absolute value fit, each
pixels penalty term is divided by this value.

SIGMA0 is the initial guess for the size.

If BACKD is given, freeze the background at this value.

SUBPIX is 1,3,5, and tells how many sub-intervals to use in the quadrature
rule.  1 means use the pixel center.  3 is an inside rule, 5 is outside.

METRIC is :SQUARE for a square-deviation penalty, and :ABS for
absolute value.
"
  (declare (type image image)
	   (type (or null image) error-image)
	   (type (single-float -1e6 1e6)  sigma0)
	   (type (or null (single-float -1e6 1e6)) backd)
	   (type (unsigned-byte 15) size nx0 ny0)
	   (type (member 1 3 5) subpix)
	   (type (member :square :abs) metric))
  ;;
  (cond ((eq metric :square)
	 (%fit-round-gaussian/square image nx0 ny0  
				     :error-image error-image
				     :size size :sigma0 sigma0 
				     :backd backd :ftol ftol 
				     :subpix subpix))
	((eq metric :abs)
	 (%fit-round-gaussian/abs image nx0 ny0  
				  :error-image error-image
				  :size size :sigma0 sigma0 
				  :backd backd :ftol ftol 
				  :subpix subpix))))






;; theta will be a normal position angle of long axis
(defun %fit-2axis-gaussian/square
    (image nx0 ny0 &key 
     (error-image nil)
     (size 21) ;; must be odd
     ;; initial guesses
     (theta0 0.0)
     (a0 1.0) 
     (b0 1.0)
     (backd nil)
     (first-fit-round t)
     (ftol 1e-6)
     (subpix 1))
  (declare (type image image)
	   (type (or null image) error-image)
	   (type (single-float -1e10 1e10)  theta0 a0 b0)
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
	 (ps (powell:build-powell-struct 5 :name "fit-gaussian"))
	 (backd-fixed (if backd (float backd 1d0) 0d0))
	  ;; storage for backd and norm computed by fit
	 ;; the could be doubles, but it conses for some reason
	 (nb-vec (make-array 2 :element-type 'double-float))
	 ;; fit just a,b,theta - backd and norm are analytic
	 (pvec-initial (make-array 5 :element-type 'double-float)))

   (declare (type (unsigned-byte 28) size/2 nx0 ix1 iy0 iy1)
	    (type double-float backd-fixed)
	    (type (simple-array double-float (*)) nb-vec pvec-initial))

    (when first-fit-round
      (let ((rxvec (%fit-round-gaussian/square
		    image nx0 ny0 
		    :error-image error-image
		    :size size 
		    :sigma0 (sqrt (* a0 b0))  
		    :ftol ftol 
		    :backd backd)))
	(setf x0 (aref rxvec 0))
	(setf y0 (aref rxvec 1))
	(setf a0 (aref rxvec 2)) ;; sigma
	(setf b0 a0)))


    
    (setf (aref pvec-initial 0) (float x0 1d0)
	  (aref pvec-initial 1) (float y0 1d0)
	  (aref pvec-initial 2) (float (* (/ pi 180) theta0 1d0)) ;; to radians
	  (aref pvec-initial 3) (float a0 1d0)
	  (aref pvec-initial 4) (float b0 1d0))

    
    (locally
	(declare (type (unsigned-byte 28) size/2 nx0 ix1 iy0 iy1)
		 (optimize speed))
      ;;
      (labels ((gaussian-exp-part (x y ct st a b) 
		 ;; does NOT contain leading term or norm
		 (declare (type (double-float -1d8 1d8) x y a b ct st))
		 (let* ((aa (+ (abs a) 1d-6))
			(bb (+ (abs b) 1d-6))
			(u (+ (* st (+ x))  (* ct y)))  ;; long axis
			(v (+ (* ct x)  (* st (- y))))) ;; short axis
		   (declare (type double-float u v))
		   (exp (* -0.5d0 (+ (expt (/ u aa) 2)
				     (expt (/ v bb) 2))))))
	       ;;
	       (eval-penalty (x0 y0 theta a b)
		 (loop 
		    with sf  of-type double-float = 0d0
		    with sff of-type double-float = 1d-100 ;; prevent rare 1/0 error
		    with sz  of-type double-float = 0d0
		    with szz of-type double-float = 1d-100
		    with sfz of-type double-float = 0d0
		    with sn  of-type double-float = 0d0
		    ;; move LT (leading term of gaussian) out of loop
		    with lt of-type double-float =  
		      (/  (* a b 2 pi))
		    with ct of-type double-float = (cos theta)
		    with st of-type double-float = (sin theta)
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
		       for z of-type double-float = (float (aref image iy ix) 1d0)
		       ;; inverse of error at this pix (inv to multiply rather than divide)
		       for zeinv of-type double-float =
			 (if error-image (/ 1d0 (float (aref error-image iy ix) 1d0)) 1d0)
		       for predicted-val of-type double-float = 
			 (subpixel-quadrature
			  x y xx yy subpix ;;x,y are center and xx,yy wander over pix
			  ;; expression that is integrated over a pixel
			  (* lt  
			     (gaussian-exp-part xx yy
						ct st a b))
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
			(return (+ theta-penalty penalty)))))
	       ;;
	       (eval-penalty-ps (ps)
		 (declare (type powell:powell-struct ps))
		 (let ((pvec (powell:powell-struct-x-vec ps)))
		   (declare (type (simple-array double-float *) pvec))
		   (setf (powell:powell-struct-y ps)
			 (eval-penalty (aref pvec 0) (aref pvec 1) (aref pvec 2)
				       (aref pvec 3) (aref pvec 4)))
		   t)))
	

	;; dimensions are x0 y0 theta a b norm backd
	(powell:init-powell-struct ps #'eval-penalty-ps pvec-initial)
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

      ;; set a,b to their abs values
      (setf (aref x-vec 3) (abs  (aref x-vec 3)))
      (setf (aref x-vec 4) (abs  (aref x-vec 4)))
      ;; swap a and b if necessary, and rotate theta by 90 deg
      (when (< (aref x-vec 3) (aref x-vec 4)) ;; a<b
	(rotatef (aref x-vec 3) (aref x-vec 4))
	(incf (aref x-vec 2) 90.0))
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
(defun %fit-2axis-gaussian/abs
    (image nx0 ny0 &key 
     (error-image nil)
     (size 21) ;; must be odd
     ;; initial guesses
     (theta0 0.0)
     (a0 1.0) 
     (b0 1.0)
     (backd nil)
     (first-fit-round t)
     (ftol 1e-6)
     (subpix 1))

  (declare (type image image)
	   (type (or null image) error-image)
	   (type (single-float -1e6 1e6)  theta0 a0 b0)
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
	 (ps (powell:build-powell-struct 7 :name "fit-gaussian"))
	 (backd0 0d0)
	 (norm0 0d0) ;; put in better guess below
	 (pvec-initial (make-array 7 :element-type 'double-float)))



    ;; when we're not first doing a round fit, estimate initial
    ;; values for norm and backd 
    (when (not first-fit-round)
      (setf backd0
	    (* 1d0
	       (imutils:image-median  image :ix0 ix0 :ix1 ix1 
				      :iy0 iy0 :iy1 iy1)))
      (loop
	 for ix of-type (unsigned-byte 28) from ix0 to ix1
	 do
	   (loop for iy of-type (unsigned-byte 28) from iy0 to iy1
	      do
		(incf norm0 (* 1.0 (- (aref image iy ix) backd0))))))

	 

    (when first-fit-round
      (let ((rxvec (%fit-round-gaussian/abs
		    image nx0 ny0 
		    :error-image error-image
		    :size size 
		    :sigma0 (sqrt (* a0 b0))  
		    :ftol ftol 
		    :backd backd)))
	(setf x0 (aref rxvec 0))
	(setf y0 (aref rxvec 1))
	(setf a0 (aref rxvec 2))
	(setf b0 a0)
	(setf norm0 (aref rxvec 3))
	(setf backd0  (aref rxvec 4)))) ;; this will be the freeze value
                                        ;; if backd was given
    


    
    (setf (aref pvec-initial 0) (float x0 1d0)
	  (aref pvec-initial 1) (float y0 1d0)
	  (aref pvec-initial 2) (float (* (/ pi 180) theta0 1d0)) ;; to radians
	  (aref pvec-initial 3) (float a0 1d0)
	  (aref pvec-initial 4) (float b0 1d0)
	  (aref pvec-initial 5) (float norm0 1d0)
	  (aref pvec-initial 6) (float backd0 1d0))
     
    (locally
	(declare (type (unsigned-byte 28) size/2 nx0 ix1 iy0 iy1)
		 (type double-float backd0 norm0)
		 (optimize speed))
      ;;
      (labels ((gaussian-exp-part (x y ct st a b) 
		 ;; does NOT contain leading term or norm
		 (declare (type (double-float -1d8 1d8) x y a b ct st))
		 (let* ((aa (+ (abs a) 1d-6))
			(bb (+ (abs b) 1d-6))
			(u (+ (* st (+ x))  (* ct y)))   ;; long axis
			(v (+ (* ct x)  (* st (- y))))) ;; short axis
		   (declare (type double-float u v))
		   (exp (* -0.5 (+ (expt (/ u aa) 2)
				   (expt (/ v bb) 2))))))
	       ;;
	       (eval-penalty (x0 y0 theta a b norm backd)
		 (loop 
		    ;; move LT (leading term of gaussian) out of loop
		    with lt of-type double-float =  
		      (/ norm  (* a b 2 pi))
		    with ct of-type double-float = (cos theta)
		    with st of-type double-float = (sin theta)
		    with penalty of-type double-float = 0d0
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
		       for zeinv of-type double-float =
			 (if error-image (/ 1d0 (float (aref error-image iy ix) 1d0)) 1d0)
		       for predicted-val of-type double-float = 
			 (subpixel-quadrature
			  x y xx yy subpix ;;x,y are center and xx,yy wander over pix
			  ;; expression that is integrated over a pixel
			  (+ backd
			     (* lt  
				(gaussian-exp-part xx yy
						   ct st a b)))
			  :float-type double-float)
		       for dif of-type double-float = 
			 (- predicted-val (aref image iy ix))
		       do 
			 (incf penalty (* zeinv (abs dif))))
		    finally
		    (return (+ theta-penalty penalty))))
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
	(powell:init-powell-struct ps #'eval-penalty-ps pvec-initial)
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

      ;; set a,b to their abs values
      (setf (aref x-vec 3) (abs  (aref x-vec 3)))
      (setf (aref x-vec 4) (abs  (aref x-vec 4)))
      ;; swap a and b if necessary, and rotate theta by 90 deg
      (when (< (aref x-vec 3) (aref x-vec 4)) ;; a<b
	(rotatef (aref x-vec 3) (aref x-vec 4))
	(incf (aref x-vec 2) 90.0))
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
(defun fit-2axis-gaussian (image nx0 ny0 &key 
			   (error-image nil)
			   (size 21) ;; must be odd
			   ;; initial guesses
			   (theta0 0.0)
			   (a0 1.0) 
			   (b0 1.0)
			   (backd nil)
			   (first-fit-round t)
			   (ftol 1e-6)
			   (subpix 1)
			   (metric :square))
"Fit a 2 axis Gaussian to an image starting at NX0,NY0 in a box of
SIZE pixels around it.

Returns (VALUES #(X0 Y0 THETA A B NORM BACKD)
                PENALTY-VALUE
                NUMBER-OF-EVALUATIONS)

The outputs are

   X0,Y0   - central fractional pixel index
   THETA   - angle in deg, shifted to [-90,90], where THETA=0
             points +Y, and rotates into +X
   A,B     - major and minor axes
   BACKD   - additive background value
   NORM    - the overall normalization term, in addition 
             to the 1/SQRT(2*PI*A*B) term already assumed 
             for the Gaussian.  NORM is the area under the Gaussian.

ERROR-IMAGE is an optional error image. For a square fit, it is the
one-sigma error for each pixel.  For an ABS absolute value fit, each
pixels penalty term is divided by this value.

THETA0,A0,B0 are the starting guesses for the angle and major/minor
axes.  FTOL is a fractional numerical tolerance that doesn't seem
to matter much.  

If BACKD is given, the background is frozen at this value, and background
is not fit.

FIRST-FIT-ROUND - Means to fit a round Gaussian first to get starting
points for the parameters, setting A0=B0=sigma.  The round Gaussian
fit should be more robust.  It uses SQRT(A*B) as the intial guess for
the round sigma.

SUBPIX is 1,3,5, and tells how many sub-intervals to use in the quadrature
rule.  1 means use the pixel center.  3 is an inside rule, 5 is outside.

METRIC is :SQUARE for a square-deviation penalty, and :ABS for
absolute value.
"
  (declare (type image image)
	   (type (or null image) error-image)
	   (type (single-float -1e6 1e6)  theta0 a0 b0)
	   (type (or null (single-float -1e6 1e6)) backd)
	   (type (unsigned-byte 15) size nx0 ny0)
	   (type (member 1 3 5) subpix)
	   (type (member :square :abs) metric))
  ;;
  (cond ((eq metric :square)
	 (%fit-2axis-gaussian/square image nx0 ny0  
				     :error-image error-image
				     :size size :a0 a0 :b0 b0
				     :theta0 theta0
				     :backd backd :ftol ftol 
				     :first-fit-round first-fit-round
				     :subpix subpix))
	((eq metric :abs)
	 (%fit-2axis-gaussian/abs image nx0 ny0  
				  :error-image error-image
				  :size size :a0 a0 :b0 b0
				  :theta0 theta0
				  :backd backd :ftol ftol 
				  :first-fit-round first-fit-round
				  :subpix subpix))))

    
    
    

(defun test-round-gauss-fit (&key (ftol 1e-6) (metric :square)
			     (sigma 6.0) (backd 14.0)
			     (x0 55) (y0 66) (norm 18.0)
			     (backd-freeze nil)
			     (subpix 1)
			     (size 25))
  (let ((im (make-gaussian-image :nx 100 :ny 100  :x0 x0 :y0 y0  
				 :a sigma :b sigma  :backd-val backd
				 :norm norm)))
    ;(plot:plot-image im)
    ;; recover Gaussian with messed up initial guesses
    (fit-round-gaussian im  (round (+ 3.0 x0)) (round (+ 2.0 y0)) :size size
			:sigma0 (* sigma 3)  :ftol ftol 
			:backd backd-freeze ;; do we freeze backd at this val?
			:metric metric :subpix subpix)))

(defun test-2axis-gauss-fit (&key (ftol 1e-6) (metric :square)
			     (a 6.0) (b 3.0) (theta 45.0) (backd 14.0)
			     (backd-freeze nil)
			     (x0 55) (y0 66) (norm 18.0)
			     (subpix 1)
			     (size 25))
  (let ((im (make-gaussian-image :nx 100 :ny 100  :x0 x0 :y0 y0  
				 :a a :b b :theta theta :backd-val backd
				 :norm norm)))
    ;(plot:plot-image im)
    ;; recover Gaussian with messed up initial guesses
    (fit-2axis-gaussian im  (round (+ 3.0 x0)) (round (+ 2.0 y0)) :size size
		     :a0 (* a 2)  :b0 (* b 2) :ftol ftol 
		     :theta0 (+ 90 (float theta  1.0))
		     :backd backd-freeze ;; do we freeze backd at this val?
		     :subpix subpix
		     :metric metric)))
