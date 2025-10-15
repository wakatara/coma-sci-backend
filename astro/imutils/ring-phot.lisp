#|

RING-PHOT performs central aperture photometry added to a integral of a 
median-computed profile, to provide background and contamination
rejection.


All errors are monte-carlo, so there is no need to know the gain.

|#



(in-package imutils)


;; ramanujan's 2nd formula, good to almost 6th order
#+nil
(declaim (inline %circumference-of-ellipse))
#+nil
(defun %circumference-of-ellipse (a b)
  (declare (type (single-float (0.0)) a b)
	   (optimize speed))
  (let* ((x (/ (- a b) (+ a b)))
	 (3x2 (* 3 x x)))  ;; three x squared
    (float
     (* pi
	(+ a b)
	(+ 1 (/ 3x2
		(+ 10.0 (sqrt (the (float 0.0) (- 4 3x2)))))))
     1.0)))


;; interpolate y in uniformly spaced x with no bounds checking
(declaim (inline %uniform-interpolate))
(defun %uniform-interpolate (xvec yvec x)
  (declare  (type (simple-array single-float (*)) xvec yvec)
	    (type (single-float 0.0 1e6) x))
  (let* ((x0 (aref xvec 0))
	 (dx (- (aref xvec 1) x0))
	 (n  (1- (length xvec)))) ;; max index
    (declare (type (single-float 0.0 1e6) x0)
	     (type (single-float 0.01 10.0) dx))
    (multiple-value-bind (i rem)
	(truncate (- x x0) dx)
      (let ((f (/ rem dx))) ;; fraction of way to next point
	(+ 
	 (* (- 1.0 f) (aref yvec i))
	 (if (< i n) ;; special case of acessing last value in xvec,yvec
	     (* f (aref yvec (1+ i)))
	     0.0))))))



(defstruct ring-phot-result
  flux           flux-err
  backd-flux     backd-sigma
  mag            mag-err      ;; -2.5 log10(flux)
  x0             y0
  r-ap-phot      ap-flux        ap-flux-err   npix-ap
  profile-flux   profile-flux-err
  profile-flux-r
  profile-r-vec
  profile-flux-vec
  profile-flux-err-vec
  profile-angle-max profile-angle-min)  

(defun ring-phot
    (im x y
     &key
       ;; quadratic centering parameters
       (center-quadratically t)
       (nquad-center 11)
       ;; general background parameters
       (r-background-1 40.0) (r-background-2 50.0)
       ;; aperture photometry parameters
       (r-ap-phot 3.0) ;; the radius in which we do aperture phtometry
       (nsubpix 10) ;; subpixels for aperture photometry
       ;; radial profile parameters
       (average-type :median)
       (r-profile 20.0)  ;; radius to integrate profile to
       (nsamp-per-pixel 1)
       (dr 0.5)
       (angle-min 0.0)
       (angle-max 360.0)
       ;;
       (gain 1.0) ;; used only for FLUX to MAG conversion
       (edge-value most-negative-single-float))
 "Perform photometry using
   1. a central aperture of R-AP-PHOT radius
       PLUS
   2. photomery by a radial profile from R-AP-PHOT to R-PROFILE

Optionally, the angle (ccw from +X) subtended by the profile generation
is limited to ANGLE-MIN to ANGLE-MAX.

All errors are Monte-Carlo computed, so gain is not used.

GAIN is used only for flux to mag conversion."

  (declare (type image im)
	   (type (single-float -1e6 1e6) x y)
	   (type (integer 0 10000) nquad-center nsubpix)
	   (type single-float r-ap-phot dr angle-min angle-max 
		 r-background-1 r-background-2 r-profile gain)
	   (optimize speed))

  (when (< r-background-2 r-background-1) (error "Outer background radius is inside inner."))
  (when (< r-background-1 r-profile) (error "R-PROFILE enters backgrond annulus at R-BACKGROUND-1."))
  (when (> r-ap-phot r-profile) (error "R-AP-PHOT > R-PROFILE."))
  
  (let ((x0 x) (y0 y)) ;; object coords
    (declare (type single-float x0 y0))
    ;; re-center if asked
    (if center-quadratically
	(multiple-value-bind (xx yy sign)
	    (fit-quadratic im (round x) (round y) nquad-center)
	  (declare (type double-float xx yy)
		   (type fixnum sign))
	  (when (not (minusp sign))
	    (error "ring-phot: Quadratic fit to find center is not negative definite."))
	  (setf x0 (float xx 1.0))
	  (setf y0 (float yy 1.0))))

    (let* ((v-backd	
	     (gather-pixels-in-annulus im x0 y0 r-background-1 r-background-2))
	   (median-backd
	     (fastmedian:fast-single-float-1d-array-median v-backd))
	   ;; convert quartiles to sigma
	   (sigma-backd
	     (let ((q25 (fastmedian:fast-single-float-1d-array-fraction
				v-backd 0.25))
		   (q75 (fastmedian:fast-single-float-1d-array-fraction
			 v-backd 0.75)))
	       (* (- q75 q25) #.(/ 1.0 1.3489795003921632e0)))))
      
      
      ;; compute the core photometry using apertures
      (multiple-value-bind (sum-center npix-center)
	  (image-sum-in-annulus im x0 y0 0.0 r-ap-phot :nsubpix nsubpix)
	(declare (type single-float sum-center npix-center))
	;;
	(multiple-value-bind (flux-vec r-vec flux-err-vec)
	  (compute-radial-profile im x0 y0 
				  :n-points (round
					     (the (float 0.0 1e5)
						  (/ r-profile dr)))
				  :axis-ratio 1.0 :axis-angle 0.0 ;; round for now
				  :angle-min angle-min :angle-max angle-max
				  :dr dr
				  :nsamp-per-pixel nsamp-per-pixel
				  :average-type average-type
				  :compute-error t
				  :edge-value edge-value)
	  (let ((sum-outside 0.0)
		(err-outside 0.0)
		(area-of-photometry 0.0))
	    (declare (type single-float sum-outside err-outside area-of-photometry))
	    
	    (loop with dri of-type single-float = 0.1
		  with ru of-type single-float = 0.0 ;; the last r we actually use
		  with c of-type single-float = (float (* 2 dri pi) 1.0) ;; constant of integ
		  for r of-type single-float from r-ap-phot below r-profile by dri
		  do (incf sum-outside (* c r (%uniform-interpolate r-vec flux-vec r)))
		     ;; sum errors of each annulus' flux in quadrature
		     (incf err-outside (* c r (expt (%uniform-interpolate r-vec flux-err-vec r) 2)))
		     (setf ru r) ;; note the last r we really used
		  finally
		     ;; the error in the profiled region is monte-carloed by
		     ;; the radial profile routine
		     (setf err-outside (sqrt (the (single-float 0.0) err-outside)))
		     (setf area-of-photometry (float (* pi ru ru) 1.0)))
	    ;;
	    (let* ((sum-total-with-backd (+ sum-center sum-outside))
		   (backd-total (* median-backd area-of-photometry))
		   (sum-total (- sum-total-with-backd backd-total))
		   (err-center (* (sqrt (the (single-float 0.0) npix-center))
				  sigma-backd))
		   (err-total
		     (sqrt (+ (expt err-center 2)
			      (expt err-outside 2)))))
	      (multiple-value-bind (mag mag-err)
		  (compute-mag+dmag-from-flux+dflux sum-total err-total
						    :gain (float gain 1.0))
		
		(make-ring-phot-result
		 :flux sum-total
		 :flux-err err-total
		 :backd-flux median-backd
		 ;; mag in ADU units
		 :mag mag
		 :mag-err mag-err
		 :x0 x0 :y0 y0
		 :backd-sigma sigma-backd
		 :r-ap-phot r-ap-phot
		 :ap-flux sum-center
		 :ap-flux-err err-center
		 :npix-ap npix-center
		 :profile-flux sum-outside
		 :profile-flux-err err-outside
		 :profile-flux-r r-profile
		 :profile-r-vec r-vec
		 :profile-flux-vec flux-vec 
		 :profile-flux-err-vec flux-err-vec
		 :profile-angle-min angle-min
		 :profile-angle-max angle-max)))))))))
	      
	      
  
  
	   
