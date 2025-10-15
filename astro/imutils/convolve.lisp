
;; convolutions, so far just brute force




(in-package imutils)


(defun convolve-image (data kernel &key (flag-image nil) (null-value 0.0))
"produce a copy of an image DATA that is convolved by kernel KERNEL.
Both are single-float 2 dimensinal arrays. The dimensions of kernel
must be odd.

If FLAG-IMAGE is provided, then use only points that have a zero flag value. 
If all points are flagged, the convolved image has NULL-VALUE."
  (declare (type (simple-array single-float (* *)) data kernel)
	   (type (or null flag-image) flag-image)
	   (type single-float null-value))
  (let* ((nkx (array-dimension kernel 1))
	 (nky (array-dimension kernel 0))
	 (nkx/2 (ash nkx -1))
	 (nky/2 (ash nky -1))
	 (nx (1- (array-dimension data 1)))
	 (ny (1- (array-dimension data 0)))
	 (data-out (make-array (array-dimensions data) :element-type 'single-float))
	 (ksum (loop for i of-type (unsigned-byte 28) below (array-total-size kernel)
		     sum (row-major-aref kernel i) of-type single-float)))
    ;;
    (when (not (and (oddp nkx) (oddp nky)))
      (error "Array dimensions of KERNEL ~A are not both odd" (array-dimensions kernel)))
    ;;
    (flet ((convolve-at-pix (mx my)
	     (declare (optimize (speed 3)  (safety 1)))
	     (loop 
		with nc of-type (unsigned-byte 28) = 0 ;; number 
		with sum of-type single-float = 0.0
		with norm of-type single-float = 0.0 
		for ix of-type (signed-byte 20) from (- mx nkx/2) to (+ mx nkx/2)
		for kx of-type (unsigned-byte 20) from 0 
		when (and (>= ix 0) (<= ix nx))
		do
		;;
		(loop 
		   for iy of-type (signed-byte 20) from (- my nky/2) to (+ my nky/2)
		   for ky of-type (unsigned-byte 20) from 0
		   when (and (>= iy 0) (<= iy ny)
			     (or (not flag-image)
				 (zerop (aref flag-image iy ix))))
		   do
		     (incf nc)
		     (let ((kval (aref kernel ky kx)))
		       (incf sum (* (aref data iy ix) kval))
		       (incf norm kval)))
		;;
		finally
		  (return (if (and (plusp nc) (not (zerop norm)))
			      (/ sum (/ norm ksum))
			      null-value)))))
      ;;
      (declare (inline convolve-at-pix)
	       (optimize speed))
      ;;
      (loop
       for ix of-type (signed-byte 28) to nx
       do
       (loop 
	for iy of-type (signed-byte 28) to ny
	do (setf (aref data-out iy ix) (convolve-at-pix ix iy))))
      data-out)))



(defun make-gaussian-kernel (nkernel sigma &key (norm 1.0) (circular t))
  "Make a Gaussian kernel of NKERNEL x NKERNEL, of Gaussian width SIGMA and 
total normalization within the array of NORM. If CIRCULAR is T (the default) 
then the outer boundary of the kernel is made circular, so the corners of the
square kernel array are clipped and kernel[ix,iy]=0 at radii larger 
than (nkernel-1)/2"
  (declare (type (unsigned-byte 20) nkernel)
	   (type real sigma norm))
  (let ((norm (float norm 1.0))
	(kern (make-array (list nkernel nkernel) :element-type 'single-float)) 
	(sigma (float sigma 1.0)))
    ;;
    (declare (type (simple-array single-float (* *)) kern)
	     (type single-float norm sigma))
    ;;
    (locally (declare (optimize speed))
      ;;    
      (loop
       with sum of-type single-float = 0.0
       with s2 of-type single-float = (expt sigma 2)
       with nk/2 = (ash nkernel -1)
       ;; max radius - either fill whole square, or limit to inscribed circle
       with r2max = (if circular 
			(expt (float nk/2 1.0) 2) 
		       most-positive-single-float)
       for ix below nkernel
       do
       (loop
	for iy below nkernel
	for r2 of-type single-float 
	=   (+ (expt (float (- nk/2 iy) 1.0) 2)
	       (expt (float (- nk/2 ix) 1.0) 2))
	for expterm of-type single-float = (* -0.5 (/ r2 s2))
        ;;
	for val of-type single-float = (if (and (> expterm -103) ;; exp underflow 
						(<= r2 r2max))  ;; outside limiting (circularizing) radius
					   (exp expterm)
					   0.0) ;; 0 to avoid underflow or circularize
	do 
	(setf (aref kern iy ix) val)
	(incf sum val))
       finally
       (loop 
	with c of-type single-float = (/ norm sum)
	for i of-type (unsigned-byte 28) below (array-total-size kern)
	do (setf (row-major-aref kern i) (* c (row-major-aref kern i))))
       (return kern)))))



(defun make-2gaussian-kernel (nkernel sigma1 sigma2 norm1 norm2 &key (circular t))
  "Make a double Gaussian Kernal of NKERNEL x NKERNEL, of Gaussian width SIGMA1 and 
SIGMA2 and NormalizationS NORM 1 and NORM2.  The normalizations are for that part of each
Gaussian actually contained within the array."
  (let ((kern1 (make-gaussian-kernel nkernel sigma1 :norm norm1 :circular circular))
	(kern2 (make-gaussian-kernel nkernel sigma2 :norm norm2 :circular circular)))
    (im+ kern1 kern2 :image-out kern1)
    kern1))
    

   