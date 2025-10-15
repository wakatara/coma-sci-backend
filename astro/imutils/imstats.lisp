

(in-package imutils)

	     


(defun image-sum (im &key ix0 ix1 iy0 iy1 (normalize nil) (ignore-nan t))
  "returns the sum of the pixels in the inclusive boundaries  IX0,IX1,IY0,IY1
as a DOUBLE float. If NORMALIZE is T, then return average of pixels. Return the
number of pixels summed as the second value, and the number of bad (NaN) pixels
as third value, if ignore-nan is set."
  (declare (type image im)
	   (type (or null (unsigned-byte 20)) ix0 ix1 iy0 iy1))
  (let* ((nx (1- (array-dimension im 1)))
	 (ny (1- (array-dimension im 0)))
	 (ix0 (or ix0 0))
	 (ix1 (or ix1 nx))
	 (iy0 (or iy0 0))
	 (iy1 (or iy1 ny))
	 (npix 0)
	 (nbad 0)
	 (the-result 0d0))
    (declare (type (unsigned-byte 28) nx ny npix nbad)
	     (type double-float the-result))
    (locally  ;; put optimization here to suppress boxing message
	(declare (optimize speed))
      (when (< ix1 ix0) (rotatef ix0 ix1))
      (when (< iy1 iy0) (rotatef iy0 iy1))
      (when (or (< ix0 0) (> ix1 nx) (< iy0 0) (> iy1 ny))
	(error "bounds IX0=~A IX1=~A IY0=~A IY1=~A outside valid array indices IX=~A...~A IY=~A...~A"
	       ix0 ix1 iy0 iy1 0 nx 0 ny))
      (loop
       with norm = (if (not normalize) 1d0
		       (* (float (- ix1 ix0 -1) 1d0)
			  (float (- iy1 iy0 -1) 1d0)))
       with sum of-type double-float = 0d0
       for ix of-type (signed-byte 20) from ix0 to ix1
       do
       (loop 
	for iy of-type (signed-byte 20) from iy0 to iy1
	for val of-type single-float = (aref im iy ix)
	do
	   (if (or (not ignore-nan)
		   (not (float-utils:single-float-nan-or-infinity-p val)))
	       (progn (incf sum val)
		      (incf npix))
	       (incf nbad))		      
       finally
       (setf the-result (/ sum norm))))
    ;;
    (values the-result npix nbad)))) 


(defun image-mean  (im &key ix0 ix1 iy0 iy1 (ignore-nan t))
  "returns the mean of the pixels in the inclusive boundaries  IX0,IX1,IY0,IY1
as a double float.  Alias for (image-sum im .... :normalize t)"
  (image-sum im :ix0 ix0 :ix1 ix1 :iy0 iy0 :iy1 iy1 :normalize t :ignore-nan ignore-nan))


;; a whole pixel version of image-sum-in-annulus
(defun image-sum-in-annulus/wholepix (im x0 y0 r0 r1 &key (normalize nil) (ignore-nan t))
  "Returns the sum as double float of pixels at R such that r0 <= r <=
r2 If NORMALIZE is T, then return average of pixels instead of
sum. Return the number of pixels (as a float) in the annulus as the second value.

The third value is the number of NaN or Inf pixels, if IGNORE-NAN is set."
  (declare (type image im)
	   (type (single-float 0.0 1e6)  x0 y0)
	   (type (single-float 0.0 1e6)  r0 r1)
	   (optimize speed))
  ;;
  (let* ((nx (1- (array-dimension im 1)))
	 (ny (1- (array-dimension im 0)))
	 (ix0 (max 0  (floor   (- x0 r1))))
	 (ix1 (min nx (ceiling (+ x0 r1))))
	 (iy0 (max 0  (floor   (- y0 r1))))
	 (iy1 (min ny (ceiling (+ y0 r1))))
	 (npix 0) ;; count of pix inside annulus
	 (nbad 0) ;; number of bad (NaN or Inf) pix
	 (sum 0d0))
    (declare (type (unsigned-byte 28) nx ny npix nbad ix0 ix1 iy0 iy1)
	     (type double-float sum))

    (loop 
     with r02 of-type single-float = (expt r0 2)
     with r12 of-type single-float = (expt r1 2)
     for ix of-type (unsigned-byte 28) from ix0 to ix1
     for x of-type single-float  = (- ix x0)
     do
     (loop 
      for iy of-type (unsigned-byte 28) from iy0 to iy1
      for y of-type single-float = (- iy y0)
      for r2 of-type single-float = (+ (expt x 2) (expt y 2))
      when (and (>= r2 r02) (<= r2 r12))
	do
	   (let ((val (aref im iy ix)))
	      (if (or (not ignore-nan)
		   (not (float-utils:single-float-nan-or-infinity-p val)))
	       (progn (incf sum val)
		      (incf npix))
	       (incf nbad)))))
    ;;
    (when (and normalize (zerop npix))
      (error "No pixels inside annulus"))
    (when normalize (setf sum (/ sum npix)))
    (values (float sum 1.0) (float npix 1.0) (float nbad 1.0))))


  
(defun image-sum-in-annulus (im x0 y0 r0 r1 &key (normalize nil)
						(nsubpix 10)
						(ignore-nan t))
  "returns the sum as double float of pixels at R such that r0 <= r <=
r2 If NORMALIZE is T, then return average of pixels instead of
sum. 

Pixels straddling the circles are computed as sub-pixels in a 
NXUBPIX x NSUBPIX grid.

Return the FLOATING POINT number of pixels in the annulus as the
second value (includes fractional pixels).

The third value is the number of bad (NaN or Inf) pixels, if
IGNORE-NAN is set."
  (declare (type image im)
	   (type (single-float 0.0 1e6)  x0 y0)
	   (type (single-float 0.0 1e6)  r0 r1)
	   (type (integer 1 10000) nsubpix)
	   (optimize (speed 3) (safety 3)))
  ;;
  (assert (> r1 r0))
  (let* ((nx (1- (array-dimension im 1)))
	 (ny (1- (array-dimension im 0)))
	 (ix0 (max 0  (floor   (- x0 r1 2))))
	 (ix1 (min nx (ceiling (+ x0 r1 2))))
	 (iy0 (max 0  (floor   (- y0 r1 2))))
	 (iy1 (min ny (ceiling (+ y0 r1 2))))
	 (npix 0d0) ;; count of pix inside annulus, a FLOAT
	 (nbad 0d0)   ;; count of bad pix we ignore, a FLOAT
	 (sum 0d0))
    (declare (type (unsigned-byte 28) nx ny ix0 ix1 iy0 iy1)
	     (type double-float sum npix nbad))

    (loop
      with r02 of-type single-float = (expt r0 2)
      with r12 of-type single-float = (expt r1 2)
      for ix of-type (unsigned-byte 28) from ix0 to ix1
      for x of-type single-float  = (- ix x0)
      do
	 (loop 
	   for iy of-type (unsigned-byte 28) from iy0 to iy1
	   for y of-type single-float = (- iy y0)
	   ;; compute radii of all 4 corners of this pixel
	   for r2a of-type single-float = (+ (expt (+ x 0.5) 2) (expt (+ y 0.5) 2))
	   for r2b of-type single-float = (+ (expt (+ x 0.5) 2) (expt (- y 0.5) 2))
	   for r2c of-type single-float = (+ (expt (- x 0.5) 2) (expt (+ y 0.5) 2))
	   for r2d of-type single-float = (+ (expt (- x 0.5) 2) (expt (- y 0.5) 2))
	   do
	      (cond
		;; when all 4 corners fall between annulus radii, sum whole pixel
		((and (>= r2a r02) (<= r2a r12)
		      (>= r2b r02) (<= r2b r12)
		      (>= r2c r02) (<= r2c r12)
		      (>= r2d r02) (<= r2d r12))
		  (let ((val (aref im iy ix)))
		    (if (or (not ignore-nan)
			    (not (float-utils:single-float-nan-or-infinity-p val)))
			(progn (incf sum val)
			       (incf npix 1d0))
			(incf nbad 1d0))))
		;; otherwise if a single corner falls in annulus sum subpix
		((or (and (>= r2a r02) (<= r2a r12))
		     (and (>= r2b r02) (<= r2b r12))
		     (and (>= r2c r02) (<= r2c r12))
		     (and (>= r2d r02) (<= r2d r12)))
		 (loop
		   with pixarea = (/ 1.0 nsubpix nsubpix) ;; area of one subpix
		   with area = pixarea  ;; total area of subpix counted
		   with pval = (* pixarea (aref im iy ix)) ;; flux val of subpix
							;; p0 and dp are initial subpix pos and step
		   with p0 of-type single-float = (/ (- 1.0 nsubpix) (* 2 nsubpix))
		   with dp of-type single-float = (/ 1.0 nsubpix)
		   for iix of-type (unsigned-byte 28) below nsubpix
		   for xs = (+ x p0 (* iix dp))
		   do
		      (loop for iiy of-type (unsigned-byte 28) below nsubpix
			    for ys = (+ y p0 (* iiy dp))
			    for r2s of-type single-float = (+ (expt xs 2) (expt ys 2))
			    when (and (>= r2s r02) (<= r2s r12))
			      do
				 (if (or (not ignore-nan)
					 (not (float-utils:single-float-nan-or-infinity-p pval)))
				     (progn (incf sum pval)
					    (incf npix pixarea))
				     (incf nbad pixarea))))))))
    ;;
    (when (and normalize (zerop npix))
      (error "No pixels inside annulus"))
    (when normalize (setf sum (/ sum npix)))
    (values (float sum 1.0) (float npix 1.0) (float nbad 1.0))))


(defun image-sum-in-annulus/linterp (im x0 y0 r0 r1 &key (normalize nil)
					      (nsubpix 10) (ignore-nan t))
  "returns the sum as double float of pixels at R such that r0 <= r <=
r2 If NORMALIZE is T, then return average of pixels instead of
sum. 

Pixels straddling the circles are computed as sub-pixels in a 
NXUBPIX x NSUBPIX grid.

Return the FLOATING POINT number of pixels in the annulus as the
second value (includes fractional pixels).

In this case, we linearly interpolate over pixels.  This is not necessarily
better and can produce spurious results.

The third value is the number of bad (NaN or Inf) pixels, if
IGNORE-NAN is set."
  (declare (type image im)
	   (type (single-float 0.0 1e6)  x0 y0)
	   (type (single-float 0.0 1e6)  r0 r1)
	   (type (integer 1 10000) nsubpix)
	   (optimize (speed 3) (safety 3)))
  ;;
  (let* ((nx (1- (array-dimension im 1)))
	 (ny (1- (array-dimension im 0)))
	 (ix0 (max 0  (floor   (- x0 r1 2))))
	 (ix1 (min nx (ceiling (+ x0 r1 2))))
	 (iy0 (max 0  (floor   (- y0 r1 2))))
	 (iy1 (min ny (ceiling (+ y0 r1 2))))
	 (npix 0d0) ;; count of pix inside annulus, a FLOAT
	 (nbad 0d0)
	 (sum 0d0))
    (declare (type (unsigned-byte 28) nx ny ix0 ix1 iy0 iy1)
	     (type double-float sum npix))

    (loop
      with r02 of-type single-float = (expt r0 2)
      with r12 of-type single-float = (expt r1 2)
      for ix of-type (unsigned-byte 28) from ix0 to ix1
      for x of-type single-float  = (- ix x0)
      do
	 (loop 
	   for iy of-type (unsigned-byte 28) from iy0 to iy1
	   for y of-type single-float = (- iy y0)
	   ;; compute radii of all 4 corners of this pixel
	   for r2a of-type single-float = (+ (expt (+ x 0.5) 2) (expt (+ y 0.5) 2))
	   for r2b of-type single-float = (+ (expt (+ x 0.5) 2) (expt (- y 0.5) 2))
	   for r2c of-type single-float = (+ (expt (- x 0.5) 2) (expt (+ y 0.5) 2))
	   for r2d of-type single-float = (+ (expt (- x 0.5) 2) (expt (- y 0.5) 2))
	   do
		(cond
		  ;; when all 4 corners fall between annulus radii, sum whole pixel
		  ((and (>= r2a r02) (<= r2a r12)
			(>= r2b r02) (<= r2b r12)
			(>= r2c r02) (<= r2c r12)
			(>= r2d r02) (<= r2d r12))
		   (let ((val (linear-interpolate-image im (float ix 1.0) (float iy 1.0))))
		      (if (or (not ignore-nan)
			    (not (float-utils:single-float-nan-or-infinity-p val)))
			(progn (incf sum val)
			       (incf npix 1d0))
			(incf nbad 1d0))))
		  ;; otherwise if a single corner falls in annulus sum subpix
		  ((or (and (>= r2a r02) (<= r2a r12))
		       (and (>= r2b r02) (<= r2b r12))
		       (and (>= r2c r02) (<= r2c r12))
		       (and (>= r2d r02) (<= r2d r12)))
		   (loop
		     with pixarea = (/ 1.0 nsubpix nsubpix) ;; area of subpix
		     with area = pixarea ;; total area summed
		     ;; p0 and dp are initial subpix pos and step
		     with p0 of-type single-float = (/ (- 1.0 nsubpix) (* 2 nsubpix))
		     with dp of-type single-float = (/ 1.0 nsubpix)
		     for iix of-type (unsigned-byte 28) below nsubpix
		     for xs = (+ x p0 (* iix dp))
		     do
			(loop for iiy of-type (unsigned-byte 28) below nsubpix
			      for ys = (+ y p0 (* iiy dp))
			      for r2s of-type single-float = (+ (expt xs 2) (expt ys 2))
			      when (and (>= r2s r02) (<= r2s r12))
				do
				   (let ((val (* pixarea (linear-interpolate-image im (+ x0 xs) (+ y0 ys)))))
				      (if (or (not ignore-nan)
					      (not (float-utils:single-float-nan-or-infinity-p val)))
					  (progn (incf sum  val)
						 (incf npix area))
					  (incf nbad area)))))))))
    ;;
    (when (and normalize (zerop npix))
      (error "No pixels inside annulus"))
    (when normalize (setf sum (/ sum npix)))
    (values (float sum 1.0) (float npix 1.0) (float nbad 1.0))))



(defun image-fraction (im frac &key 
		       ix0 ix1 iy0 iy1 (scratch-vec nil))
  "returns the value C such that fraction FRAC of the pixels in the
inclusive boundaries IX0,IX1,IY0,IY1 are smaller than C, with C being
a single float.  SCRATCH-VEC is an optional single-float array for
computing the median."
  (declare (type image im)
	   (type (or null (simple-array single-float (*))) scratch-vec)
	   (type single-float frac)
	   (type (or null (unsigned-byte 20)) ix0 ix1 iy0 iy1)
	   (optimize speed))
  (let* ((nx (1- (array-dimension im 1)))
	 (ny (1- (array-dimension im 0)))
	 (ix0 (or ix0 0))
	 (ix1 (or ix1 nx))
	 (iy0 (or iy0 0))
	 (iy1 (or iy1 ny))
	 (npix 0))
    (declare (type (unsigned-byte 28) npix))
    (when (< ix1 ix0) (rotatef ix0 ix1))
    (when (< iy1 iy0) (rotatef iy0 iy1))
    (when (or (< ix0 0) (> ix1 nx) (< iy0 0) (> iy1 ny))
      (error "bounds IX0=~A IX1=~A IY0=~A IY1=~A outside valid array indices IX=~A...~A IY=~A...~A"
	     ix0 ix1 iy0 iy1 0 nx 0 ny))

    (setf npix (the (unsigned-byte 28) 
		 (* (the (unsigned-byte 14)  (1+ (- ix1 ix0)))
		    (the (unsigned-byte 14)  (1+ (- iy1 iy0))))))
    ;;
    (when (and scratch-vec (< (length scratch-vec) npix))
      (error "SCRATCH-VEC provided is too short"))
    
    (locally (declare (optimize speed))
      (loop
	with scratch-vec = (or scratch-vec
			       (make-array npix :element-type 'single-float))
	with i of-type (unsigned-byte 28) 
	for ix of-type (signed-byte 20) from ix0 to ix1
	do
	   (loop 
	     for iy of-type (signed-byte 20) from iy0 to iy1
	     for val of-type single-float = (aref im iy ix)
	     when (not (float-utils:single-float-nan-or-infinity-p val))
	       do 
		  (setf (aref scratch-vec i) (aref im iy ix))
		  (incf i))	
	finally
	   (when (zerop i) (error "No valid pixels to compute image fraction"))
	   (return (fastmedian:fast-single-float-1d-array-fraction scratch-vec
								   frac i))))))
  
(defun image-median  (im &key ix0 ix1 iy0 iy1 (scratch-vec nil))
   "returns the value C such that 1/2 of the pixels in the inclusive
boundaries IX0,IX1,IY0,IY1 are smaller than C, with C being a single
float.  SCRATCH-VEC is an optional single-float array for computing
the median.   An alias for (image-fraction im 0.5 .....)"
   (image-fraction im 0.5 :ix0 ix0 :ix1 ix1 :iy0 iy0 :iy1 iy1 :scratch-vec scratch-vec))





(defun gather-pixels-in-annulus (im x0 y0 r0 r1
				 &key (scratch-vec nil)
				 (ignore-nan t))
  "Create a single-float vector of the pixels within an annulus
centered on X0,Y0 betweeen radii R0,R1 inclusive.   If IGNORE-NAN
is set, then ignore any NaN or Inf pixels."
  (declare (type image im)
	   (type (or null (simple-array single-float (*))) scratch-vec)
	   (type (single-float 0.0 1e6)  x0 y0)
	   (type (single-float 0.0 1e6)  r0 r1)
	   (optimize speed))
  ;;
  (let* ((nx (1- (array-dimension im 1)))
	 (ny (1- (array-dimension im 0)))
	 (ix0 (max 0  (floor   (- x0 r1))))
	 (ix1 (min nx (ceiling (+ x0 r1))))
	 (iy0 (max 0  (floor   (- y0 r1))))
	 (iy1 (min ny (ceiling (+ y0 r1))))
	 (npix 0)) ;; count of pix inside annulus
    (declare (type (unsigned-byte 28) nx ny npix ix0 ix1 iy0 iy1))

    ;; first count the pixels inside the annulus
    (loop 
     with r02 of-type single-float = (expt r0 2)
     with r12 of-type single-float = (expt r1 2)
     for ix of-type (unsigned-byte 28) from ix0 to ix1
     for x of-type single-float  = (- ix x0)
     do
	(loop 
	  for iy of-type (unsigned-byte 28) from iy0 to iy1
	  for y of-type single-float = (- iy y0)
	  for r2 of-type single-float = (+ (expt x 2) (expt y 2))
	  when (and (>= r2 r02) (<= r2 r12) ;; pixel is in annulus
		    (or (not ignore-nan) ;; and is not a NaN if avoiding NaNs
			   (not (float-utils:single-float-nan-or-infinity-p
				 (aref im iy ix)))))
	    do 
	       (incf npix 1)))
    
    ;; now either make scratch-vec, or throw an error
    (if scratch-vec
	(when (> npix (length scratch-vec))
	  (error "SCRATCH-VEC has ~D elements, but ~D are needed"
		 (length scratch-vec) npix))
	(setf scratch-vec (make-array npix :element-type 'single-float)))
    
    ;; now make an array of the right size, and fill a vector with them
    (loop 
       with v of-type (simple-array single-float (*)) = scratch-vec
       with k of-type imindex = 0
       with r02 of-type single-float = (expt r0 2)
       with r12 of-type single-float = (expt r1 2)
       for ix of-type (unsigned-byte 28) from ix0 to ix1
       for x of-type single-float  = (- ix x0)
       do
	 (loop 
	    for iy of-type (unsigned-byte 28) from iy0 to iy1
	    for y of-type single-float = (- iy y0)
	    for r2 of-type single-float = (+ (expt x 2) (expt y 2))
	    when (and (>= r2 r02) (<= r2 r12) ;; pixel is in annulus
		    (or (not ignore-nan) ;; and is not a NaN if avoiding NaNs
			   (not (float-utils:single-float-nan-or-infinity-p
				 (aref im iy ix)))))
	      do
		 (setf (aref v k) (aref im iy ix))
		 (incf k))
       finally 
	 (return (values v npix)))))



(defun image-fraction-in-annulus (im frac x0 y0 r0 r1
				  &key
				    (scratch-vec nil)
				    (ignore-nan t))
  "Returns the value C such that fraction FRAC of the pixels in the
annulus centered on X0,Y0 with radii from R0 to R1 are smaller than C,
with C being a single float.  SCRATCH-VEC is an optional single-float
array for computing the median. 

Returns 

 (VALUES FRACTIONAL-PIX-VALUE NUMBER-OF-PIXELS-EXAMINED).  

The annulus is allowed to go over the edges, in which case out of
bounds pixels are ignored.

If no pixels are found then (VALUES 0.0 0) is returned, so be sure to check
second value."
  (multiple-value-bind (vec npix)
      (gather-pixels-in-annulus im x0 y0 r0 r1
				:scratch-vec scratch-vec
				:ignore-nan ignore-nan)
    (values
     (if (plusp npix)
	 (fastmedian:fast-single-float-1d-array-fraction vec frac npix)
	 0.0)
     npix)))

(defun image-median-and-sigma-in-annulus (im x0 y0 r0 r1
					  &key
					    (scratch-vec nil)
					    (no-median nil)
					    (ignore-nan t))
  "Returns the median value of the pixels in the annulus centered on
X0,Y0 with radii from R0 to R1 are smaller than C, with C being a
single float.  As second value, return the sigma computed using
quartiles.  SCRATCH-VEC is an optional single-float array for computing
the median.

Returns:
 
  (VALUES MEDIAN SIGMA NPIX)

The annulus is allowed to go over the edges, in which case out of
bounds pixels are ignored.

If no pixels are found then (VALUES 0.0 0.0 0) is returned, so be sure to check
final value NPIX.

If NO-MEDIAN is true, then return just the sigma.
"
    (multiple-value-bind (vec npix)
      (gather-pixels-in-annulus im x0 y0 r0 r1
				:scratch-vec scratch-vec
				:ignore-nan ignore-nan)
      (if (plusp npix)
	  (let* ((med (if (not no-median)
			  (fastmedian:fast-single-float-1d-array-fraction vec 0.5 npix)
			  0.0))
		 (q1 (fastmedian:fast-single-float-1d-array-fraction vec 0.25 npix))
		 (q3 (fastmedian:fast-single-float-1d-array-fraction vec 0.75 npix))
		 (sigma (/ (- q3 q1) 1.3489795003921)))
	    (values med sigma npix))
	  (values 0.0 0.0 npix))))





    


(defun get-random-pixel-sample (image nsamp
				&key 
				  (flag-image nil)
				  (allow-nan nil)
				  ix0 ix1 iy0 iy1 (scratch-vec nil))
  "Returns NSAMP randomly sampled pixels from a image in
[IX0:IX1,IY0:IY1], placing them into SCRATCH-VEC if provided.
The returned vector may be shorter than NSAMP if there are flagged pixels
Returns (VALUES VECTOR-OF-PIXELS NSAMPLED  COMPLETE) where NSAMPLED<=NSAMP
and COMPLETE is T if all NSAMP points were obtained. 
ALLOW-NAN, if true (the default), allows infinity and NaN to be returned in sample."
  (declare (type image image)
	   (type (or null flag-image) flag-image)
	   (type (or null (simple-array single-float (*))) scratch-vec)
	   (type (or null (unsigned-byte 20)) ix0 ix1 iy0 iy1)
	   (type (unsigned-byte 24) nsamp)
	   (optimize speed))
  (let* ((nx (1- (array-dimension image 1)))
	 (ny (1- (array-dimension image 0)))
	 (ix0 (or ix0 0))
	 (ix1 (or ix1 nx))
	 (iy0 (or iy0 0))
	 (iy1 (or iy1 ny))
	 (nsampled 0))

    (declare (type (unsigned-byte 28) nsampled))
    (when (< ix1 ix0) (rotatef ix0 ix1))
    (when (< iy1 iy0) (rotatef iy0 iy1))
    (when (or (< ix0 0) (> ix1 nx) (< iy0 0) (> iy1 ny))
      (error "bounds IX0=~A IX1=~A IY0=~A IY1=~A outside valid array indices IX=~A...~A IY=~A...~A"
	     ix0 ix1 iy0 iy1 0 nx 0 ny))

    ;;
    (when (and scratch-vec (< (length scratch-vec) nsamp))
      (error "SCRATCH-VEC provided is too short"))
    ;;
    (setf scratch-vec (or scratch-vec (make-array nsamp :element-type 'single-float)))
    ;;
    (loop 
       with kxmax of-type imindex =  (1+ (- ix1 ix0))
       with kymax of-type imindex =  (1+ (- iy1 iy0))
       ;; try up to 4x nsamp (to allow for flagging)
       for i of-type imindex below (* 4 nsamp)
       for ix = (+ ix0 (random kxmax))
       for iy = (+ iy0 (random kymax))
       for imval = (aref image iy ix)
       when (and
	     ;; no flag
	     (or (not flag-image) 
		 (zerop (aref flag-image iy ix)))
	     ;; NaN
	     (or allow-nan
		 (not (float-utils:single-float-nan-or-infinity-p imval))))
		 
       do
	 (setf (aref scratch-vec nsampled) imval)
	 (incf nsampled)
	 (when (= nsampled nsamp)
	   (return)))
    ;;
    (values (if (= nsampled nsamp)
		scratch-vec 
		(subseq scratch-vec 0 nsampled))
	    nsampled 
	    (= nsampled nsamp))))
	 


(defun compute-sampled-image-median-and-sigma
    (image nsamp
     &key 
       (flag-image nil)
       ix0 ix1 iy0 iy1
       (scratch-vec nil)
       (allow-nan nil))
  "Randomly sample an image using  GET-RANDOM-PIXEL-SAMPLE, 
and return (VALUES MEDIAN SIGMA OK) where OK is NIL on failure."
  (let* ((v (get-random-pixel-sample image nsamp 
				     :flag-image  flag-image
				     :ix0 ix0 :ix1 ix1
				     :iy0 iy0 :iy1 iy1
				     :scratch-vec scratch-vec
				     :allow-nan allow-nan))
	 (med 0.0)
	 (sigma 0.0)
	 (ok nil))
    (declare (type (simple-array single-float (*)) v)
	     (type single-float med sigma))
    ;;
    (when (plusp (length v))
      (setf ok t)
      (setf med (fastmedian:fast-single-float-1d-array-median v))
      (dotimes (i (length v))
	(setf (aref v i)
	      (abs (- (aref v i) med))))
      (setf sigma (fastmedian:fast-single-float-1d-array-fraction 
		   v 0.68268949213)))

    ;;
    (values med sigma nil)))

	


(defun estimate-image-gain (im  &key ix0 ix1 iy0 iy1 (bias 0.0))
  "Estimate the gain of an image, assuming given bias offset.  Return
two values, from the top and bottom sigmas"
  (declare (type image im))
  (let* ((median-raw (image-median im :ix0 ix0 :ix1 ix1 :iy0 iy0 :iy1 iy1))
	 (median (- median-raw bias))
	 (m1sig  (- median-raw  
		    (image-fraction im 0.1586553
				    :ix0 ix0 :ix1 ix1 :iy0 iy0 :iy1 iy1)))
	 (p1sig  (- (image-fraction im 0.8413447
				    :ix0 ix0 :ix1 ix1 :iy0 iy0 :iy1 iy1)
		    median-raw)))
    (values
     (/ median (expt m1sig 2))
     (/ median (expt p1sig 2)))))

#|
(asdf:load-system "random")
(defun generate-noise-image-with-gain (gain &key (nx 500) (ny 500)
					      (counts 5000.0)
					      (bias 0.0))
  "Generate an image with level GAIN and BIAS to test gain estimation.
COUNTS is the mean level, on top of BIAS level."
  (loop
    with im = (make-image nx ny)
    with 1/gain = (/ 1.0 gain) ;; turn photons to ADU
    for i below (array-total-size im)
    for x = (+ bias
	       (* 1/gain counts)
	       (* 1/gain (sqrt counts) (random:gaussian)))
    do (setf (row-major-aref im i) (float x 1.0))
       finally (return im)))

cl-user> (imutils:estimate-image-gain 
	  (imutils::generate-noise-image-with-gain 4.0 :bias 1000) 
	  :bias 1000.0)
3.9959192 ;; correct gain of 4 comes out
3.9950368

|#
