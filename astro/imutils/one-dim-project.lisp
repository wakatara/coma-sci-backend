
#|

one dimensional projection of image - given a line, project onto its
orthogonal.

NOT TESTED


|#



(in-package imutils)



(defun one-dim-project (image x0 y0 x1 y1 zmin zmax nsample
			&key (interp-method :linear)
			(combine-method :mean)
			(sampling-step 0.5)
			(null-value 0.0))
  "Given an IMAGE, and a line extending from X0,Y0 to X1,Y1
compute an NSAMPLE length vector along the Z direction, where Z
is a direction orthogonal to the line, and the direction of +Z
is  in the direction of -(Y1-Y0),+(X1-X0) 

Ie, if the line extends in the +X direction, then Z is
in the +Y direction.

The number of samples along Z is NSAMPLE, and the sampling scale
along the line is SAMPLING-STEP, by default 0.5 pixels.

The image is interpolated using :INTERP-METHOD as in
image-interp.lisp, and the points along a particular sweep along
the line are combined using :COMBINE-METHOD, which may be :MEAN
or :MEDIAN or :SUM

NULL-VALUE is the value returned when there is no data, indicating
an attempt to sample out of bounds.

Returns
 (VALUES FLUX-VEC Z-VEC)
where FLUX-VEC is the fluxes, an Z-VEC is their Z-values."


  (declare (type image image)
	   (type (single-float -1e6 1e6) x0 y0 x1 y1 zmin zmax sampling-step)
	   (type (member :nearest :linear :lanczos2 :lanczos3 :lanczos4) interp-method)
	   (type (member :median :mean :sum) combine-method)
	   (type (integer 1 #.(expt 2 25)) nsample)
	   (optimize speed))


  (let* ((dxline (- x1 x0))
	 (dyline (- y1 y0))
	 (line-length  (sqrt (+ (expt dxline 2) (expt dyline 2))))
	 ;; components of ZHAT, the direction orthog to line
	 (zhat0 (- (/ dyline line-length)))
	 (zhat1 (+ (/ dxline line-length)))
	 ;; components of WHAT, the direction along the line
	 (what0 (+ (/ dxline line-length)))
	 (what1 (+ (/ dyline line-length)))
	 ;;
	 ;; output z array and flux value
	 (zvec (make-array nsample :element-type 'single-float))
	 (fvec (make-array nsample :element-type 'single-float))
	 ;; accumulation array to gather samples for mean/median
	 (nacc (floor (the (float 0.0 1e6) (/ line-length sampling-step))))
	 (acc  (make-array nacc :element-type 'single-float)))
    ;;
    (declare (type single-float dxline dyline line-length 
		   zhat0 zhat1 what0 what1)
	     (type (simple-array single-float (*)) zvec acc)
	     (type (unsigned-byte 28) nacc))
    ;;
    (flet ((do-resampling-pass-along-line (z)
	     (declare (type single-float z)
		      (optimize (speed 3) (safety 1)))
	     ;;
	     (loop 
		with j of-type (unsigned-byte 28) = 0
		for i of-type (unsigned-byte 28) below nacc
		for w of-type single-float = (* i sampling-step)
		for x of-type single-float = (+ x0 (* z zhat0) (* w what0))
		for y of-type single-float = (+ y0 (* z zhat1) (* w what1))
		do
		  (multiple-value-bind (f inside?)
		      (cond ((eq interp-method :nearest)
			     (nearest-pixel-interpolate-image image x y))
			    ((eq interp-method :linear)
			     (linear-interpolate-image image x y))
			    ((eq interp-method :lanczos2)
			     (lanczos2-interpolate-image image x y))
			    ((eq interp-method :lanczos3)
			     (lanczos3-interpolate-image image x y))
			    ((eq interp-method :lanczos4)
			     (lanczos4-interpolate-image image x y)))
		    (when inside?
		      (setf (aref acc j) f)
		      (incf j)))
		finally
		  (return
		    (cond ((= j 0) null-value) ;; no data, so return null-value
			  ;;
			  ((eq combine-method :mean)
			   (/ 
			    (loop for jj below j
			       sum  (aref acc jj) of-type single-float)
			    j))
			  ;;
			  ((eq combine-method :sum)
			   (loop for jj below j
				 sum (* sampling-step (aref acc jj)) of-type single-float))
			  ;;
			  ((eq combine-method :median)
			   (fastmedian:fast-single-float-1d-array-median 
			    acc j)))))))
      ;;
      ;; for each Z value, make a pass down the line in the W direction
      ;; and resample and get the mean/median
      (loop
	 for nz of-type (unsigned-byte 28) below nsample
	 for z of-type single-float = (+ zmin (/ (* nz (- zmax zmin)) (1- nsample)))
	 do
	   (setf (aref zvec nz) z)
	   (setf (aref fvec nz) (do-resampling-pass-along-line z))))
    ;;
    (values fvec zvec)))
    
	

  
