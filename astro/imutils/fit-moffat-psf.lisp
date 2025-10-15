
#|


Fit round and elliptical Moffat functinos to images, least squares metric only.

Functions are

 (FIT-ROUND-MOFFAT IMAGE NX0 NY0 &KEY ...)
 
 (FIT-2AXIS-MOFFAT IMAGE NX0 NY0 &KEY ...)

--- notes ---

Interior quadrature is not as efficient as it could be, because
values of fit function at pixel boundaries are needlessly recomputed.

Error image functionality is not tested.


|#

(in-package imutils)




(defun fit-round-moffat
    (image nx0 ny0 &key 
     (error-image nil)
     (size 21) ;; must be odd
     (a0 1.0) 
     (beta0 2.5)
     (backd nil) 
     (beta nil) 
     (ftol 1e-6)
     (subpix 1))

"Fit a round MOFFAT function at NX0,NY0 in IMAGE with least squares

   NORM*(beta-1)/(pi a^2) (1+(r/a)^2)^-beta

ERROR-IMAGE is an optional image with one sigma error estimates.

Freeze BACKD and/or BETA at supplied values if requested.
BETA0 is an initial guess, but BETA is a value to freeze at.

Returns (VALUES #(X0 Y0 A BETA NORM BACKD) 
                CHISQR 
                N-EVALUATIONS)"

  (declare (type image image)
	   (type (or null image) error-image)
	   (type (single-float -1e6 1e6)  a0)
	   (type (or null (single-float -1e6 1e6)) beta backd)
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
	 (ps (powell:build-powell-struct 4 :name "fit-moffat"))
	 (backd-fixed (if backd (float backd 1d0) 0d0))
	 ;; storage for backd and norm computed by fit
	 ;; the could be doubles, but it conses for some reason
	 (nb-vec (make-array 2 :element-type 'double-float))
	 ;; fit just a,b,theta - backd and norm are analytic
	 (pvec-initial (make-array 4 :element-type 'double-float)))
    (declare (type (unsigned-byte 28) size/2 nx0 ix1 iy0 iy1)
	     (type double-float backd-fixed)
	     (type (simple-array double-float (*)) nb-vec pvec-initial))


    (setf (aref pvec-initial 0) (float nx0 1d0)
	  (aref pvec-initial 1) (float ny0 1d0)
	  (aref pvec-initial 2) (float a0 1d0)
	  (aref pvec-initial 3) (float (or beta beta0) 1d0))
    
    (locally
	(declare (optimize speed))
      ;;
      (labels ((eval-penalty (x0 y0 a beta)
		 (loop 
		    ;; various sum terms
		    with sf  of-type double-float = 0d0
		    with sff of-type double-float = 1d-100 ;; avoid rare 1/0 error
		    with sz  of-type double-float = 0d0
		    with szz of-type double-float = 1d-100 
		    with sfz of-type double-float = 0d0
		    with sn  of-type double-float = 0d0
		    ;;
		    with x0 = (float x0 1d0)
		    with y0 = (float y0 1d0)
		    with lt of-type double-float =  
		      (/ (- beta 1.0)  (* pi a a))
		    with aa of-type double-float = (* a a)
		    with extra-penalty of-type double-float = 0d0
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
			       (expt (the (double-float 0d0) (+ 1d0 (/ r2 aa)))  
				     (- beta))))
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
		      (when (> beta 5d0)
			(incf extra-penalty (* 1d5 (expt (- beta 5d0) 2))))
		      (when (< beta 1d0)
			(incf extra-penalty (* 1d5 (expt (- beta 1d0) 2))))
		      (when (> a (* 0.3 size))
			(incf extra-penalty (* 1d5 (expt (- a (* 0.3d0 size)) 2))))
		      (when (< a 0)
			(incf extra-penalty (* 1d5 (expt a 2))))


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
		      (return (+ extra-penalty penalty)))))
	       ;;
	       (eval-penalty-ps (ps)
		 (declare (type powell:powell-struct ps))
		 (let ((pvec (powell:powell-struct-x-vec ps)))
		   (declare (type (simple-array double-float *) pvec))
		   (setf (powell:powell-struct-y ps)
			 (eval-penalty (aref pvec 0) (aref pvec 1) (aref pvec 2)
				       (aref pvec 3)))
		   t)))
	;;
	(powell:init-powell-struct ps #'eval-penalty-ps pvec-initial  
				   :optim-flags (vector t t t (not beta)))
	(powell:run-powell-on-ps #'eval-penalty-ps ps ftol)
	(eval-penalty-ps ps)))
      
    
    
    ;; now convert the results back to single precision because
    ;; that's what imutils generally operates in
    (let ((x-vec (make-array 6 :element-type 'single-float))
	  (ps-x-vec (powell:powell-struct-x-vec ps))
	  (y (float (powell:powell-struct-y ps) 1.0)))
      (loop for i below 4 do (setf (aref x-vec i)
				   (float (aref ps-x-vec i) 1.0)))
      (setf (aref x-vec 4) (float (aref nb-vec 0) 1.0))
      (setf (aref x-vec 5) (float (aref nb-vec 1) 1.0))

      ;; set a to its abs value
      (setf (aref x-vec 2) (abs  (aref x-vec 2)))

      (values x-vec
	      y
	      (powell:powell-struct-neval ps)))))
    
;; theta will be a normal position angle of long axis
(defun fit-2axis-moffat
    (image nx0 ny0 &key 
     (error-image nil)
     (size 21) ;; must be odd
     ;; initial guesses
     (theta0 0.0)
     (beta0 2.5) ;; initial value
     (beta nil)  ;; value to freeze at
     (a0 1.0) 
     (b0 1.0)
     (backd nil)
     (first-fit-round t)
     (ftol 1e-6)
     (subpix 1))
"Least-squares fit a 2 axis Moffat to an image starting at NX0,NY0 in
a box of SIZE pixels around it.

   NORM*(beta-1)/(pi a^2) (1+(r/a)^2)^-beta

where r is is the A,B,THETA ellipticization of the coordinates.

Returns (VALUES #(X0 Y0 A BETA NORM BACKD) 
                PENALTY-VALUE
                NUMBER-OF-EVALUATIONS)

The outputs are

   X0,Y0   - central fractional pixel index
   THETA   - angle in deg, shifted to [-90,90], where THETA=0
             points +Y, and rotates into +X
   THETA   - angle in deg, shifted to [-90,90]
   A,B     - major and minor axes
   BACKD   - additive background value
   NORM    - the overall normalization termm

ERROR-IMAGE is an optional error image.

THETA0,A0,B0 are the starting guesses for the angle and major/minor
axes.  FTOL is a fractional numerical tolerance that doesn't seem to
matter much.

If BACKD is given, the background is frozen at this value, and background
is not fit.

FIRST-FIT-ROUND - Means to fit a round Moffat first to get starting
points for the parameters.

SUBPIX is 1,3,5, and tells how many sub-intervals to use in the quadrature
rule.  1 means use the pixel center.  3 is an inside rule, 5 is outside."


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
	 (ps (powell:build-powell-struct 6 :name "fit-moffat"))
	 (backd-fixed (if backd (float backd 1d0) 0d0))
	  ;; storage for backd and norm computed by fit
	 ;; the could be doubles, but it conses for some reason
	 (nb-vec (make-array 2 :element-type 'double-float))
	 ;; fit just x0,y0,a,b,theta,beta - backd and norm are analytic
	 (pvec-initial (make-array 6 :element-type 'double-float)))

   (declare (type (unsigned-byte 28) size/2 nx0 ix1 iy0 iy1)
	    (type double-float backd-fixed)
	    (type (simple-array double-float (*)) nb-vec pvec-initial))

    (when first-fit-round
      (let ((rxvec (fit-round-moffat
		    image nx0 ny0 
		    :error-image error-image
		    :size size 
		    :a0 (sqrt (* a0 b0))  
		    :ftol ftol
		    :beta0 beta0
		    :beta beta
		    :backd backd)))
	(setf x0 (aref rxvec 0))
	(setf y0 (aref rxvec 1))
	(setf a0 (aref rxvec 2)) 
	(setf b0 a0)
	(setf beta0 (aref rxvec 3)))) ;; will be beta if it is set

    
    
    (setf (aref pvec-initial 0) (float x0 1d0)
	  (aref pvec-initial 1) (float y0 1d0)
	  (aref pvec-initial 2) (float (* (/ pi 180) theta0 1d0)) ;; to radians
	  (aref pvec-initial 3) (float a0 1d0)
	  (aref pvec-initial 4) (float b0 1d0)
	  (aref pvec-initial 5) (float beta0 1d0))

    
    (locally
	(declare (type (unsigned-byte 28) size/2 nx0 ix1 iy0 iy1)
		 (optimize speed))
      ;;
      (labels ((moffat-expt-part (x y ct st a b beta) 
		 ;; does NOT contain leading term or norm
		 (declare (type (double-float -1d8 1d8) x y a b ct st beta))
		 (let* ((aa (+ (abs a) 1d-10))
			(bb (+ (abs b) 1d-10))
			(u (+ (* st (+ x))  (* ct y)))   ;; long axis
			(v (+ (* ct x)  (* st (- y))))) ;; short axis
		   (declare (type double-float ct st aa bb u v))
		   (expt (the (double-float 0d0) 
			   (+ 1d0 
			      (+ (expt (/ u aa) 2)
				 (expt (/ v bb) 2))))
			 (- beta))))
	       ;;
	       (eval-penalty (x0 y0 theta a b beta)
		 (loop 
		    with sf  of-type double-float = 0d0
		    with sff of-type double-float = 1d-100 ;; avoid rare 1/0 error
		    with sz  of-type double-float = 0d0
		    with szz of-type double-float = 0d0
		    with sfz of-type double-float = 1d-100
		    with sn  of-type double-float = 0d0
		    ;; move LT (leading term of moffat) out of loop
		    with lt of-type double-float =  
		      (/ (- beta 1)  (* pi a b))
		    with ct of-type double-float = (cos theta)
		    with st of-type double-float = (sin theta)
		    with extra-penalty of-type double-float = 0d0
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
			     (moffat-expt-part xx yy ct st a b beta))
			  :float-type double-float)
		       do 
			 (incf sf (* zeinv predicted-val))
			 (incf sff (expt (* zeinv predicted-val) 2))
			 (incf sz  (* zeinv z))
			 (incf szz (expt (* zeinv z) 2))
			 (incf sfz  (* zeinv zeinv z predicted-val))
			 (incf sn (* zeinv zeinv))) ;; coeff of const term, or 1/sigmaz^2
		    finally
		      (when (> (abs theta) 10d0)
			(incf extra-penalty
			      (* 1e4 (expt (- (abs theta) 10d0) 2))))
		      (when (> beta 5d0)
			(incf extra-penalty (* 1d5 (expt (- beta 5d0) 2))))
		      (when (< beta 1d0)
			(incf extra-penalty (* 1d5 (expt (- beta 1d0) 2))))
		      (when (> a (* 0.3 size))
			(incf extra-penalty (* 1d5 (expt (- a (* 0.3d0 size)) 2))))
		       (when (< a 0)
			(incf extra-penalty (* 1d5 (expt a 2))))
		      (when (> b (* 0.3 size))
			(incf extra-penalty (* 1d5 (expt (- b (* 0.3d0 size)) 2))))
		      (when (< b 0)
			(incf extra-penalty (* 1d5 (expt b 2))))

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
			(return (+ extra-penalty penalty)))))
	       ;;
	       (eval-penalty-ps (ps)
		 (declare (type powell:powell-struct ps))
		 (let ((pvec (powell:powell-struct-x-vec ps)))
		   (declare (type (simple-array double-float *) pvec))
		   (setf (powell:powell-struct-y ps)
			 (eval-penalty (aref pvec 0) (aref pvec 1) (aref pvec 2)
				       (aref pvec 3) (aref pvec 4) (aref pvec 5)))
		   t)))
	

	;; dimensions are x0 y0 theta a b beta norm backd
	(powell:init-powell-struct ps #'eval-penalty-ps pvec-initial
				   :optim-flags (vector t t t t t (not beta)))


	;; freeze backd if requested
	(powell:run-powell-on-ps #'eval-penalty-ps ps ftol)
	(eval-penalty-ps ps)))
    
    ;; now convert the results back to single precision because
    ;; that's what imutils generally operates in
    (let ((x-vec (make-array 8 :element-type 'single-float))
	  (ps-x-vec (powell:powell-struct-x-vec ps))
	  (y (float (powell:powell-struct-y ps) 1.0)))
      (loop for i below 6 do (setf (aref x-vec i)
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
      
      (setf (aref x-vec 6) (float (aref nb-vec 0) 1.0))
      (setf (aref x-vec 7) (float (aref nb-vec 1) 1.0))

      (values x-vec
	      y
	      (powell:powell-struct-neval ps)))))
    


    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %make-moffat-image (&key (nx 100) (ny 100) 
			  (theta 0.0) (backd-val 0.0)
			  (x0 50.0) (y0 50.0) (norm 1.0)
			  (a 5.0) (b 5.0) (beta 2.5))
  (let ((im (make-image ny nx)))
    (fill-image im backd-val)
    (add-moffat-to-image im x0 y0 a b beta theta norm)
    im))

(defun test-round-mofatt-fit (&key (ftol 1e-6) 
			      (a 6.0) (backd 14.0)
			      (x0 55) (y0 66) (norm 18.0)
			      (backd-freeze nil)
			      (beta0 2.5) ;; initial guess
			      (beta nil) ;; freeze value
			      (subpix 1)
			      (size 25))
  (let ((im (%make-moffat-image :nx 100 :ny 100  
			       :x0 (float x0 1.0) :y0 (float y0 1.0) 
			       :beta beta0
			       :a a :b a :backd-val backd
			       :norm norm)))
    ;(plot:plot-image im)
    ;; recover Moffat with messed up initial guesses
    (fit-round-moffat im  (round (+ 1.0 x0)) (round (+ 1.0 y0)) :size size
			:a0  (* 2 a)  :ftol ftol 
			:beta beta
			:backd backd-freeze ;; do we freeze backd at this val?
			:subpix subpix)))

(defun test-2axis-mofatt-fit (&key (ftol 1e-6) 
			      (a 6.0) (b 3.0) (theta 45.0) (backd 14.0)
			      (beta0 2.50)
			      (beta nil)
			      (backd-freeze nil)
			      (x0 55) (y0 66) (norm 18.0)
			      (subpix 1)
			      (first-fit-round t)
			      (size 25))
  (let ((im (%make-moffat-image :nx 100 :ny 100 
			       :x0 (float x0 1.0) :y0 (float y0 1.0)
			       :beta beta0
			       :a a :b b :theta theta :backd-val backd
			       :norm norm)))
    ;; recover Moffat with messed up initial guesses
    (fit-2axis-moffat im  (round (+ 1.0 x0)) (round (+ 2.0 y0)) :size size
		     :a0 (* a 2)  :b0 (* b 3) :ftol ftol 
		     :theta0 theta ; (+ 90 (float theta  1.0))
		     :backd backd-freeze ;; do we freeze backd at this val?
		     :first-fit-round first-fit-round
		     :beta beta
		     :subpix subpix)))
