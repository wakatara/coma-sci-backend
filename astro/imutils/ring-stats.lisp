
;; compute image stats over rings


(in-package imutils)


;; compute average (mean/median) and possibly monte carlo error as 2nd value
;; MAX-RESAMPLE is the maximum number of resampling operations
(defun %compute-average-of-vec-helper (vec n-inside compute-error average-type
				       &key (max-resample 200))
  (declare (type (simple-array single-float (*)) vec)
	   (type (unsigned-byte 28) n-inside max-resample)
	   (type (member :mean :median) average-type)
	   (optimize speed))
  (cond ((eq average-type :median)
	 (cond
	   ((not compute-error)
	    (fastmedian:fast-single-float-1d-array-median vec n-inside))
	   (t ;; returns 2 values
	    (bootstrap:resample-one-sided-median-dev/single-float
		(subseq vec 0 n-inside) 
		(min max-resample (* 2 n-inside))
		:frac 0.68))))
	;;
	((eq average-type :mean) 
	 (cond
	   ((not compute-error)
	    (/ (loop for i below n-inside
		     sum (aref vec i) of-type single-float)
	       n-inside))
	   (t ;; returns 2 values
	    (bootstrap:resample-one-sided-mean-dev/single-float
	     (subseq vec 0 n-inside) 
	     (min max-resample (* 2 n-inside))
	     :frac 0.68))))))


;; rotate points by phi, which is angle rotating from +x into +y
(defmacro %2d-rotate (x y cosphi sinphi)
  `(let ((%x ,x)
	 (%y, y)
	 (%cosphi ,cosphi)
	 (%sinphi ,sinphi))
     (values
      (- (* %x %cosphi) (* %y %sinphi))
      (+ (* %y %cosphi) (* %x %sinphi)))))
  

#+ignore  ;; old version that has built-in linear interpolation
(defun compute-average-over-ring-old 
    (data x0 y0 r
     &key
       (axis-ratio 1.0)
       (axis-angle 0.0)
       (angle-min 0.0)
       (angle-max 360.0)
       (nsamp-per-pixel 1)
       (average-type :median) ;; or mean
       (scratch-vec nil)
       (compute-error nil) ;; use Monte-Carlo to compute error
       (max-resample 200) ;; maximum number of resamplings
       (edge-value most-negative-single-float))
  "compute mean or median in 1 pixel wide possibly elliptical ring of
radius R centered on X0,Y0 - interpolates pixel values, and returns
0.0 at or beyond edges. Scratch vec is an optional storage vector of
length > r*2pi, to avoid local allocation and consing.


AXIS-RATIO (default 1) is the ratio of minor axis /  major axis.
 By default, the ring is circular.

AXIX-ANGLE (default 0) is the direction that the major axis points,
 with 0 being +X and 90 being +Y.

ANGLE-MIN and ANGLE-MAX constrain the angle range in which the average
 is computed.  These are RELATIVE TO AXIS-ANGLE, IN THE COORDINATES OF
 THE ELLIPSE, so that sweeping from ANGLE-MIN=0 to ANGLE-MAX=90 sweeps
 counterclockwise from the major axis to the minor axis.

NSAMP-PER-PIXEL is the approximate number of points per pixel sampled
 as the arc is traced. By default it is 1, and must be <100.

EDGE-VALUE is the value for out of bounds r. By default, it is
 MOST-NEGATIVE-SINGLE-FLOAT.

COMPUTE-ERROR uses Monte Carlo resampling to compute an error on the 
 profile.  However, this is very expensive, so MAX-RESAMPLE limits the
 number of resampling operations per pixel."
  (declare (type image data)
	   (type (single-float 0.0 1e5) r angle-min angle-max axis-angle axis-ratio)
	   (type (integer 1 100) nsamp-per-pixel)
	   (type single-float edge-value)
	   (type (single-float -1e6 1e6) x0 y0)
	   (type (or null (simple-array single-float (*))) scratch-vec)
	   (type (member :median :mean) average-type)
	   (optimize speed))
  (locally (declare (optimize (speed 0))) ;; shush compilation notes
    (when (or (> (- angle-max angle-min) 360.0)
	      (< angle-max angle-min))
      (error "ANGLE-MIN=~A > ANGLE-MAX=~A or ANGLE-MAX - ANGLE-MIN = ~A > 360"
	     angle-min angle-max (- angle-max angle-min))))
  ;;
  (let* ((nx (array-dimension data 1))
	 (ny (array-dimension data 0))
	 ;; number of pixels in circle -- ensure it is at least 1
	 (angle-range (* (- angle-max angle-min) #.(/ 1.0 360.0)))
	 (narc (max 1
		    (* nsamp-per-pixel (ceiling (* angle-range 2 pi r)))))
	 ;; number actually falling into frame
	 (n-inside 0)
	 (v (cond ((not scratch-vec) 
		   (make-array narc :element-type 'single-float))
		  ((< (length scratch-vec) narc)
		   (error "Scratch vec too short: ~A< narc=~A" (length scratch-vec) narc))
		  (t scratch-vec))))
    (declare (type (unsigned-byte 28) nx ny narc n-inside)
	     (type (float -1.0 1.0) angle-range))
    ;;
    (flet ((get-val-at-xy (x y)
	     (declare (type (single-float -1e6 1e6) x y))
	     (let* ((ix (floor x))
		    (iy (floor y))
		    (fx (- x ix))
		    (fy (- y iy)))
	       ;; avoid extreme edges - just return 0.0
	       (when (or (< ix 0) (< iy 0)
		       (>= ix (- nx 2)) (>= iy (- ny 2)))
		 ;; 2nd value NIL means outside the frame
		 (return-from get-val-at-xy (values 0.0 nil)))
	       ;;
	       (values
		(+
		 (* (aref data iy ix)             (- 1.0 fy) (- 1.0 fx))
		 (* (aref data (1+ iy) ix)        fy (- 1.0 fx))
		 (* (aref data iy (1+ ix))        (- 1.0 fy) fx)
		 (* (aref data (1+ iy) (1+ ix))   fy fx))
		t)))) ;; T means inside the frame
      (loop
	with cosphi = (cos (float (* axis-angle (/ pi 180)) 1.0))
	with sinphi = (sin (float (* axis-angle (/ pi 180)) 1.0))
	with t0 = (float (* angle-min (/ pi 180)) 1.0)
	with t1 = (float (* angle-max (/ pi 180)) 1.0)
	with dtheta of-type single-float = (/ (- t1 t0) narc)
	for theta of-type single-float from t0 by dtheta
	for i of-type (unsigned-byte 28) below narc 
	do
	   ;; xe, ye are the x,y values in a system where ellipse is oriented
	   ;; along x axis and centered
	   (let ((xe (* r (cos theta)))
		 (ye (* r axis-ratio (sin theta))))
	     ;; x,y rotate these into the global cartesian system
	     (multiple-value-bind (x y)
		 (%2d-rotate xe ye cosphi sinphi)
	       (incf x x0)
	       (incf y y0)
	       (multiple-value-bind (val inside?)
		   (get-val-at-xy x y)
		 (when inside?
		   (setf (aref v n-inside) val)
		   (incf n-inside))))))
      ;;
      ;; test to see we really have the right number of points at end - we do!
      ;;(format t "r=~A n=~A~%" r n-inside)
      ;;
      (if (plusp n-inside)
	  (%compute-average-of-vec-helper v n-inside compute-error
					  average-type
					  :max-resample max-resample)
	  ;; n-inside is zero, so return edge-value
	  (values edge-value (when compute-error nil))))))




(defun compute-average-over-ring 
    (data x0 y0 r
     &key
       (axis-ratio 1.0)
       (axis-angle 0.0)
       (angle-min 0.0)
       (angle-max 360.0)
       (nsamp-per-pixel 1)
       (average-type :median) ;; or mean
       (scratch-vec nil)
       (compute-error nil) ;; use Monte-Carlo to compute error
       (max-resample 200) ;; maximum number of resamplings
       (interp-method :linear)
       (edge-value most-negative-single-float))
  "compute mean or median in 1 pixel wide possibly elliptical ring of
radius R centered on X0,Y0 - interpolates pixel values, and returns
0.0 at or beyond edges. Scratch vec is an optional storage vector of
length > r*2pi, to avoid local allocation and consing.


AXIS-RATIO (default 1) is the ratio of minor axis /  major axis.
 By default, the ring is circular.

AXIX-ANGLE (default 0) is the direction that the major axis points,
 with 0 being +X and 90 being +Y.

ANGLE-MIN and ANGLE-MAX constrain the angle range in which the average
 is computed.  These are RELATIVE TO AXIS-ANGLE, IN THE COORDINATES OF
 THE ELLIPSE, so that sweeping from ANGLE-MIN=0 to ANGLE-MAX=90 sweeps
 counterclockwise from the major axis to the minor axis.

NSAMP-PER-PIXEL is the approximate number of points per pixel sampled
 as the arc is traced. By default it is 1, and must be <100.

INTERP-METHOD determines how pixel values are interpolated, and may be 
  :NEAREST, :LINEAR, :LANCZOS2,3,4

EDGE-VALUE is the value for out of bounds r. By default, it is
 MOST-NEGATIVE-SINGLE-FLOAT.

COMPUTE-ERROR uses Monte Carlo resampling to compute an error on the 
 profile.  However, this is very expensive, so MAX-RESAMPLE limits the
 number of resampling operations per pixel.

NaN or infinite values are ignored.

"
  (declare (type image data)
	   (type (single-float 0.0 1e5) r angle-min angle-max axis-angle axis-ratio)
	   (type (integer 1 100) nsamp-per-pixel)
	   (type single-float edge-value)
	   (type (single-float -1e6 1e6) x0 y0)
	   (type (or null (simple-array single-float (*))) scratch-vec)
	   (type (member :median :mean) average-type)
	   (optimize speed))
  (locally (declare (optimize (speed 0))) ;; shush compilation notes
    (when (or (> (- angle-max angle-min) 360.0)
	      (< angle-max angle-min))
      (error "ANGLE-MIN=~A > ANGLE-MAX=~A or ANGLE-MAX - ANGLE-MIN = ~A > 360"
	     angle-min angle-max (- angle-max angle-min))))
  ;;
  (let* ((angle-range (* (- angle-max angle-min) #.(/ 1.0 360.0)))
	 (narc (max 1 ;; number of samples per circle
		    (* nsamp-per-pixel (ceiling (* angle-range 2 pi r)))))
	 ;; number actually falling into frame
	 (n-inside 0)
	 (v (cond ((not scratch-vec) 
		   (make-array narc :element-type 'single-float))
		  ((< (length scratch-vec) narc)
		   (error "Scratch vec too short: length=~A < narc=~A"
			  (length scratch-vec) narc))
		  (t scratch-vec))))
    (declare (type (unsigned-byte 28)  narc n-inside)
	     (type (float -1.0 1.0) angle-range))
    ;;
    (flet ((get-val-at-xy (x y)
	     (declare (type (single-float -1e6 1e6) x y))
	     ;; returns (values interp-val T) if inside or
	     ;; (values 0.0  NIL) if outside
	     (general-interpolate-image data x y interp-method)))
      ;;
      (loop
	with cosphi = (cos (float (* axis-angle (/ pi 180)) 1.0))
	with sinphi = (sin (float (* axis-angle (/ pi 180)) 1.0))
	with t0 = (float (* angle-min (/ pi 180)) 1.0)
	with t1 = (float (* angle-max (/ pi 180)) 1.0)
	with dtheta of-type single-float = (/ (- t1 t0) narc)
	for theta of-type single-float from t0 by dtheta
	for i of-type (unsigned-byte 28) below narc 
	do
	   ;; xe, ye are the x,y values in a system where ellipse is oriented
	   ;; along x axis and centered
	   (let ((xe (* r (cos theta)))
		 (ye (* r axis-ratio (sin theta))))
	     ;; x,y rotate these into the global cartesian system
	     (multiple-value-bind (x y)
		 (%2d-rotate xe ye cosphi sinphi)
	       (incf x x0)
	       (incf y y0)
	       (multiple-value-bind (val inside?)
		   (get-val-at-xy x y)
		 (when (and inside?
			    (not (float-utils:single-float-nan-or-infinity-p val)))
		   (setf (aref v n-inside) val)
		   (incf n-inside))))))
      ;;
      ;; test to see we really have the right number of points at end - we do!
      ;;(format t "r=~A n=~A~%" r n-inside)
      ;;
      (if (plusp n-inside)
	  (%compute-average-of-vec-helper v n-inside compute-error
					  average-type
					  :max-resample max-resample)
	  ;; n-inside is zero, so return edge-value
	  (values edge-value (when compute-error nil))))))



(defun compute-radial-profile (data x0 y0
			       &key
				 (axis-ratio 1.0)
				 (axis-angle 0.0)
				 (angle-min 0.0) 
				 (angle-max 360.0)
				 (n-points nil)
				 (dr 1.0)
				 (nsamp-per-pixel 1)
				 (average-type :median)
				 (interp-method :linear)
				 (compute-error t)
				 (max-resample 200)
				 (begin-at-one nil)
				 (subtract-background nil)
				 (edge-value most-negative-single-float))
    "For single float 2D array DATA, compute a vector of medians or
means (determined by AVERAGE-TYPE = :MEDIAN or :MEAN) in elliptical
rings centered on X0,Y0 from angle ANGLE-MIN to ANGLE-MAX. Returned
vector of medians are plotted at intervals DR, by default 1 pix.  The
number of points returned is N-POINTS, by default to the edge of the
image.  EDGE-VALUE, by default MOST-NEGATIVE-SINGLE-FLOAT, is placed
at indices beyond the edge of the image.

ANGLE-MIN and ANGLE-MAX are defined as counterclockwise from
AXIS-ANGLE, which is the ccw angle of the long axis of the ellipse from
the x axis.  By default, rings are circular, so the ANGLE-MIN, ANGLE-MAX
are just angles from X axis into +Y.

+X corresponds to 0 deg, and +Y is 90 deg.

NSAMP-PER-PIXEL is the approximate number of points per pixel sampled
 as the arc is traced. By default it is 1.

INTERP-METHOD (:NEAREST, :LINEAR, :LANCZOS2,3,4) is the way uesed

If COMPUTE-ERROR is set, use Monte-Carlo resampling to compute
errors.  For this to be OK, we have to hit each pixel once, or
use NSAMP-PER-PIXEL=1.    

BEGIN-AT-ONE means to normalize so that flux(0)=1.0
SUBTRACT-BACKGROUND means to use FLUX(RMAX) as the background;
 if it is an integer 'm', then the last 'm' bins are averaged.

MAX-RESAMPLE limits the number of resampling operations if
COMPUTE-ERROR is set because resampling is very expensive.

Returns (VALUES FLUX-VEC R-VEC ERR-VEC)"
  
  (declare (type image data)
	   (type real x0 y0 angle-min angle-max dr)
	   (type (or null (unsigned-byte 28)) n-points)
	   (type single-float edge-value axis-ratio axis-angle)
	   (type (member :median :mean) average-type))
  (when (and compute-error (not (= nsamp-per-pixel 1)))
    (error "Reasonable errors will not be computed with COMPUTE-ERROR=T if NSAMP-PER-PIXEL is not 1 because values will be correlated."))
  
  (let* ((nx (array-dimension data 0))
	 (ny (array-dimension data 1))
	 (dxmax (max (- x0 0) (- nx x0)))
	 (dymax (max (- y0 0) (- ny y0)))
	 (rmax (sqrt (+ (expt dxmax 2) (expt dymax 2))))
	 (dr (float dr 1.0))
	 (irmax (or n-points (round (/ rmax dr))))
	 (ncirc (+ 1 ;; safety buffer for roundoff
		   (ceiling 
		    (* 2 pi irmax))))
	 (scratch-vec (make-array ncirc  :element-type 'single-float))
	 (fvec (make-array irmax :element-type 'single-float
			   :initial-element edge-value))
	 (rvec (make-array irmax :element-type 'single-float))
	 (errvec (when compute-error
		   (make-array irmax :element-type 'single-float))))

 
    
    (loop for i below irmax
	  for r of-type single-float = 0.0 then (+ r dr)
	  do 
	     (multiple-value-bind (fval ferr)
		 (compute-average-over-ring data (float x0 1.0) (float y0 1.0)
					    (float r 1.0)
					    :axis-ratio axis-ratio
					    :axis-angle axis-angle
					    :compute-error compute-error
					    :max-resample max-resample
					    :angle-min (float angle-min 1.0)
					    :angle-max (float angle-max 1.0)
					    :average-type average-type
					    :edge-value edge-value
					    :nsamp-per-pixel nsamp-per-pixel
					    :interp-method interp-method
					    :scratch-vec scratch-vec)
	       (setf (aref fvec i) fval)
	       (when compute-error
		 (setf (aref errvec i) (or ferr ;; if FERR=NIL (no data) make error infinite
					   most-positive-single-float)))
	       (setf (aref rvec i) r)))

    (when subtract-background
      ;; subtract background using last SUBTRACT-BACKGROUND pixels if it's
      ;; an integer
      (loop with nsb = (if (integerp subtract-background) subtract-background 1)
	    with f0 = (/ (loop with k0 = (length fvec)
			       for k from 1 to nsb
			       sum (aref fvec (- k0 k)))
			 nsb)
	    for i below (length fvec)
	    do (decf (aref fvec i) f0)))
    
    (when begin-at-one
      (loop with f0 = (aref fvec 0)
	    for i below (length fvec)
	    do (setf (aref fvec i) (/ (aref fvec i) f0))
	       (when errvec 
		 (setf (aref errvec i) (/ (aref errvec i) f0)))))
    (values fvec rvec errvec)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; these are legacies

(defun compute-median-over-ring 
    (data x0 y0 r &key (angle-min 0.0) (angle-max 360.0)
     (nsamp-per-pixel 1)
     (scratch-vec nil) (edge-value most-negative-single-float))
  "Legacy wrapper for compute-radial-profile"
  (compute-average-over-ring data x0 y0 r   
			  :angle-min angle-min
			  :angle-max angle-max 
			  :nsamp-per-pixel nsamp-per-pixel
			  :scratch-vec scratch-vec :edge-value edge-value))

(defun make-ring-median-vector (data x0 y0 &key 
				(angle-min 0.0) 
				(angle-max 360.0)
				(n-points nil)
				(nsamp-per-pixel 1)
				(dr 1.0)
				(edge-value most-negative-single-float))
  "Legacy wrapper for compute-radial-profile"
  (compute-radial-profile data x0 y0 
			  :angle-min angle-min
			  :angle-max angle-max :n-points n-points
			  :nsamp-per-pixel nsamp-per-pixel
			  :dr dr :edge-value edge-value
			  :average-type :median)) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fwhm-using-radial-profile (img x y rmax
				  &key
				    (fit-quadratic t)
				    (n-quadfit 7)
				    (background :auto))
  "Compute the FWHM of an image using a radial profile.   By default,
FIT-QUADRTIC forces the position to be tuned.

By default BACKGROUND=:AUTO and the RMAX pixel is the background; otherwise
BACKGROUND should be a real value for the background to use (typically 0
if background subtracted).



Returns (VALUES FWHM MONOTONIC-P XOUT YOUT) where MONONOTONIC-P is true if the
profile is monotinically decreasing.  Otherwise, the value is suspect.

XOUT,YOUT are the corrected X,Y."
  (declare (type image img)
	   (type real x y rmax)
	   (type (or (member :auto) real) background))
  (let ((x (float x 1.0))
	(y (float y 1.0)))
    ;; tune position
    (when fit-quadratic
      (multiple-value-bind (x0 y0 sign)
	  (imutils:fit-quadratic img (round x) (round y) n-quadfit)
	(when (not (minusp sign))
	  (error "Not a peak (negative definite quadratic form) in FIT-QUADRATIC at x=~A,y=~A"
		 x y))
	(setf x x0
	      y y0)))

    (multiple-value-bind (flux-vec r-vec)
	(compute-radial-profile img x y
				:n-points (ceiling (* 2 rmax))
				;; compute at 0.2 pix intervals
				:dr 0.2
				:compute-error nil
				:interp-method :linear)
      (let* ((backd (if (eq background :auto)
			(aref flux-vec (1- (length flux-vec)))
			(float background 0.0)))
	     (peak (aref flux-vec 0))
	     (half-peak (+ backd (* 0.5 (- peak backd))))
	     (monotonic-p t))
	(loop for i below (1- (length flux-vec)) ;; almost to end
	      for f0 = (aref flux-vec i)
	      for f1 = (aref flux-vec (1+ i))
	      for r0 = (aref r-vec i)
	      for r1 = (aref r-vec (1+ i))
	      when (> f1 f0)
		do (setf monotonic-p nil)
	      ;; when we straddle the half-max
	      when (<= f1 half-peak f0)
		do (let ((frac-pix (if (= f0 f1)
				       0.5  ;; weird case of f0=f1
				       (/ (- f0 half-peak) (- f0 f1)))))
		     (return
		       (values
			(* 2 (+ r0 frac-pix)) ;; HWHM to FWHM
			monotonic-p x y)))
	      finally (error "Could not find FWHM as the half-value between peak and background."))))))
		
    
