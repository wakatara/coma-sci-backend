

(in-package imutils)

(declaim (inline complex-fft-index-to-frequency))
(defun complex-fft-index-to-frequency (i n &optional (timespan 1.0))
  "Given an array of size N (0...N-1), and given index I, return the frequency
corresponding to I.  For an even 0..N array with time spacing D this produces 
frequencies
    [0 1/ND 2/ND .. 1/2D .. -2/ND 1/ND]
The center 1/2D is actually a mix of -1/2D and 1/2D.  We GUESS that for an 
odd-length FFT transform, the center 1/2D frequency is omitted.

If the TIMESPAN is given (default 1.0) representing the interval
between the first and last time points, then the result is a true
frequency, using D=TIMESPAN/(N-1)"
  (declare (type fixnum i n)
	   (type single-float timespan))
  (let ((n/2 (ash n -1))
	(1/d (/ (1- n) timespan))) ;; inverse of time interval
    (* 1/d
       (cond ((<= i n/2)
	      (/ (float i) n))
	     (t
	      (- (/ (float (- n i)) n)))))))


  
	   

(defun fftf-image-to-complex-image (im  &key (fft-im-out nil)
					(double-precision nil))
  "Perform a foward ('f') FFT of 2d floating point array IM,
turning it into a COMPLEX FLOAT array.  This involves doubling its
size.  The Fourier representation is normalized by 1/SQRT(N) so that a
transform followed by a reverse transform should leave the image
unaffected.

The transforms are normalized; ie, divided 1/sqrt(NX*NY).

If FFT-IM-OUT is given, it should be a COMPLEX FLOAT array of the
same dimensions as IM; is is filled with the FFT.

If DOUBLE-PRECISION is true, then the output array is complex double.

IM is not altered."
  
  (declare (type (or image double-image) im)
	   (type (or null complex-image double-complex-image) fft-im-out))

  (when (and fft-im-out
	     (not (equalp (array-dimensions im) (array-dimensions fft-im-out))))
    (error "Dimensions of IM and FFT-IM-OUT do not match"))

  (when (and fft-im-out
	     (or (and double-precision (eq (array-element-type fft-im-out)
					   '(complex single-float)))
		 (and (not double-precision) (eq (array-element-type fft-im-out)
						 '(complex double-float)))))
    (error "IM-OUT is of type ~A but it does not match DOUBLE-PRECISION=~A"
	   (array-element-type fft-im-out) double-precision))

  (let ((plan-pair (if double-precision
		       (fftw3lib:build-fftw3-plan-pair-double-c2c
			(coerce (array-dimensions im) 'vector)
			:normalize t)
		       (fftw3lib:build-fftw3-plan-pair-float-c2c
			(coerce (array-dimensions im) 'vector)
			:normalize t)))
	(im-in (if (and double-precision
			(eq (array-element-type im) 'single-float))
		   (image-to-double-image im)
		   im))
	(im-out
	  (or fft-im-out 
	      (make-array (array-dimensions im)
			  :element-type
			  (if double-precision
			      '(complex double-float)
			      '(complex single-float))))))

    (fftw3lib:execute-fftw3-plan-pair im-in im-out plan-pair :forward)
    (fftw3lib:destroy-fftw3-plan-pair plan-pair)
    im-out))




(defun fftb-complex-image-to-image (imfft &key (im-out nil)
					  (double-precision nil))
  "Given a complex single or double float array IMFFT, compute the
inverse fourier transform and place into a REAL float image.  Also
return the largest imaginary value that was discarded.

The transforms are normalized; ie, divided 1/sqrt(NX*NY).

If DOUBLE-PRECISION is true, then the output is made double precision.
This case might be useful when IMFFT is a double float, when the
forward transform was used in double precision mode.

IMFFT is overwritten with its fourier transform.
"
  (declare (type (or double-complex-image complex-image) imfft)
	   (type (or null image double-image) im-out))

  (when (and im-out
	     (not (equalp (array-dimensions imfft) (array-dimensions im-out))))
    (error "Dimensions of IMFFT and IM-OUT do not match"))
  (let* ((double-precision-fft (typep imfft 'double-complex-image))
	 (plan-pair (if double-precision-fft
			(fftw3lib:build-fftw3-plan-pair-double-c2c 
			 (coerce (array-dimensions imfft) 'vector)
			 :normalize t)
			(fftw3lib:build-fftw3-plan-pair-float-c2c 
			 (coerce (array-dimensions imfft) 'vector)
			 :normalize t)))
	(im-out
	  (or im-out
	      (if double-precision
		  (make-array (array-dimensions imfft) :element-type 'double-float)
		  (make-array (array-dimensions imfft) :element-type 'single-float))))
	(max-imag-val most-negative-double-float)) ;; use dbl
    (declare (type (or double-float single-float) max-imag-val)
	     (type (or image double-image) im-out))

    ;; imfft how holds the image back in spatial space
    (fftw3lib:execute-fftw3-plan-pair imfft imfft plan-pair :backward)
    (fftw3lib:destroy-fftw3-plan-pair plan-pair)

    ;; cases are float->float, float->double, double->float, double->double
    (if double-precision
	;; double output
	(locally (declare (type double-image im-out))
	  (if double-precision-fft
	      ;; double input
	      (loop for i below (array-total-size imfft)
		    for xc of-type (complex double-float)  = (row-major-aref imfft i)
		    do (setf max-imag-val (max (imagpart xc) max-imag-val))
		       (setf (row-major-aref im-out i)  (realpart xc)))
	      ;; float input
	      (loop for i below (array-total-size imfft)
		    for xc of-type (complex single-float)  = (row-major-aref imfft i)
		    do (setf max-imag-val (max (imagpart xc) (float max-imag-val 1d0)))
		       (setf (row-major-aref im-out i) (float (realpart xc) 1d0)))))
	;; float output
	(locally (declare (type image im-out))
	  (if double-precision-fft
	      ;; double input
	      (loop for i below (array-total-size imfft)
		    for xc of-type (complex double-float)  = (row-major-aref imfft i)
		    do (setf max-imag-val (float (max (imagpart xc) max-imag-val) 1e0))
		       (setf (row-major-aref im-out i) (float (realpart xc) 1e0)))
	      ;; float input
	      (loop for i below (array-total-size imfft)
		    for xc of-type (complex single-float)  = (row-major-aref imfft i)
		    do (setf max-imag-val (max (imagpart xc) max-imag-val))
		       (setf (row-major-aref im-out i) (realpart xc))))))

    
    (values    
     im-out max-imag-val)))


#+nil
(defun fft-convolve-images
    (im1 im2 &key (imfft1 nil) (imfft2 nil) (im-out nil) (action :convolve)
	 (eps 1e-10))
  "Convolve two images - ie, multiply their fourier transforms, using
a complex fft.  IMFFT1,2 are an optional single-float scratch arrays,
and IM-OUT is an optional single float output target array. All array
dimensions must match.

If ACTION is DECONVOLVE, perform a deconvolution instead, dividing
FFT(IM1) by FFT(IM2) instead of multiplying.

Note that the kernel is in wrap-around order.  The indices 0,0
correspond to x=0, y=0.  Positive indices 1,2,3 are forward spatial
offsets,and indices N-1,N-2, etc are negative spatial offsets.

Also note that deconvolution is unusable because of instability.  A 
true deconvolution algorithm, like Wiener deconvolution, uses a noise
estimate to construct an optimal deconvolution kernel. 

The value of EPS used seems to stabilize some of the worst problems.

The current algorithm works in forward/reverse on perfect data, but
any attempt to apply to real data produces a meaningless image."

  (declare (type image im1 im2)
	   (type (or null image) im-out)
	   (type (or null complex-image) imfft1 imfft2)
	   (type (member :convolve :deconvolve) action)
	   (type single-float eps))
  
  (when (not (equalp (array-dimensions im1)
		     (array-dimensions im2)))
    (error "IM1 and IM2 have different dimensions"))
  (when (and imfft1  (not (equalp (array-dimensions im1)
				  (array-dimensions imfft1))))
    (error "IMFFT1 has wrong dimensions"))
  (when (and imfft2  (not (equalp (array-dimensions im1)
				  (array-dimensions imfft2))))
    (error "IMFFT2 has wrong dimensions"))
  (when (and im-out  (not (equalp (array-dimensions im1)
				  (array-dimensions im-out))))
    (error "IM-OUT has wrong dimensions"))

  ;;
  (let ((imfft1
	  (or
	   imfft1
	   (make-array (array-dimensions im1)
		       :element-type '(complex single-float))))
	(imfft2
	  (or
	   imfft2
	   (make-array (array-dimensions im1)
		       :element-type '(complex single-float))))
	(im-out
	  (or
	   im-out
	   (make-array (array-dimensions im1) :element-type 'single-float))))
    
    (declare (type complex-image imfft1 imfft2)
	     (type image im-out))
    ;;
    (fftf-image-to-complex-image im1 :fft-im-out imfft1)
    (fftf-image-to-complex-image im2 :fft-im-out imfft2)
    ;;
    ;; either multiply or divide in fourier space
    (locally (declare (optimize speed))
      (cond ((eq action :convolve)
	     (loop 
		for i below (array-total-size im1)
		do (setf (row-major-aref imfft1 i)
			 (* (row-major-aref imfft1 i)
			    (row-major-aref imfft2 i)))))
	    ;;
	    ((eq action :deconvolve)
	     (loop 
		for i below (array-total-size im1)
		for ival  of-type (complex single-float)
		  = (row-major-aref imfft1 i)
		for ival-stabilized of-type (complex single-float)
		  = (if (< (abs ival) eps)
			#C(0.0 0.0)
			ival)
		for kval of-type (complex single-float)
		  = (row-major-aref imfft2 i)
		for kval-stabilized  of-type (complex single-float)
		  = (if (< (abs kval) eps)
			(complex eps)
			kval)
		do
		   (setf (row-major-aref imfft1 i)
			 (/ ival-stabilized kval-stabilized))))))
    ;; and convert back
    (fftb-complex-image-to-image imfft1 :im-out im-out)))



(defun fft-convolve-images
    (im1 im2 &key (imfft1 nil) (imfft2 nil) (im-out nil) (action :convolve)
	 (double-precision nil)
	 (eps 1e-10))
  "Convolve two images - ie, multiply their fourier transforms, using
a complex fft.  IMFFT1,2 are an optional single-float scratch arrays,
and IM-OUT is an optional single float output target array. All array
dimensions must match.

If ACTION is DECONVOLVE, perform a deconvolution instead, dividing
FFT(IM1) by FFT(IM2) instead of multiplying.

Note that the kernel is in wrap-around order.  The indices 0,0
correspond to x=0, y=0.  Positive indices 1,2,3 are forward spatial
offsets,and indices N-1,N-2, etc are negative spatial offsets.

Also note that deconvolution is unusable because of instability.  A 
true deconvolution algorithm, like Wiener deconvolution, uses a noise
estimate to construct an optimal deconvolution kernel. 

The input arrays IM1 and IM2 can be single or double precision real arrays.

The internal math is done with double precison, so IMFFT1 and IMFFT2 must
be double precision complex, if given.  If DOUBLE-PRECISION is true,
then the output array is double precision as well.

The value of EPS used seems to stabilize some of the worst problems.

The current algorithm works in forward/reverse on perfect data, but
any attempt to apply to real data produces a meaningless image."

  (declare (type (or image double-image) im1 im2)
	   (type (or null image double-image) im-out)
	   (type (or null double-complex-image) imfft1 imfft2)
	   (type (member :convolve :deconvolve) action)
	   (type real eps))
  
  (when (not (equalp (array-dimensions im1)
		     (array-dimensions im2)))
    (error "IM1 and IM2 have different dimensions"))
  (when (and imfft1  (not (equalp (array-dimensions im1)
				  (array-dimensions imfft1))))
    (error "IMFFT1 has wrong dimensions"))
  (when (and imfft2  (not (equalp (array-dimensions im1)
				  (array-dimensions imfft2))))
    (error "IMFFT2 has wrong dimensions"))
  (when (and im-out  (not (equalp (array-dimensions im1)
				  (array-dimensions im-out))))
    (error "IM-OUT has wrong dimensions"))
  ;;
  ;; is IM-OUT of the correct type, if given?
  (when (and im-out
	     (or (and double-precision (eq (array-element-type im-out) 'single-float))
		 (and (not double-precision) (eq (array-element-type im-out) 'double-float))))
    (error "IM-OUT is of type ~A but it does not match DOUBLE-PRECISION=~A"
	   (array-element-type im-out) double-precision))
	

  ;;
  (let ((eps (float eps 1d0))
	(imfft1
	  (or
	   imfft1
	   (make-array (array-dimensions im1)
		       :element-type '(complex double-float))))
	(imfft2
	  (or
	   imfft2
	   (make-array (array-dimensions im1)
		       :element-type '(complex double-float))))
	(im-out
	  (or
	   im-out
	   (make-array (array-dimensions im1)
		       :element-type (if double-precision
					 'double-float
					 'single-float)))))
    
    (declare (type double-complex-image imfft1 imfft2)
	     (type (or image double-image) im-out))
    ;;
    (fftf-image-to-complex-image im1 :fft-im-out imfft1 :double-precision t)
    (fftf-image-to-complex-image im2 :fft-im-out imfft2 :double-precision t)
	
    ;;
    ;; either multiply or divide in fourier space, putting result into IMFFT1
    (locally (declare (optimize speed))
      (cond ((eq action :convolve)
	     (loop 
		for i below (array-total-size im1)
		do (setf (row-major-aref imfft1 i)
			 (* (row-major-aref imfft1 i)
			    (row-major-aref imfft2 i)))))
	    ;;
	    ((eq action :deconvolve)
	     (loop 
		for i below (array-total-size im1)
		for ival  of-type (complex double-float)
		  = (row-major-aref imfft1 i)
		for ival-stabilized of-type (complex double-float)
		  =  (if (< (abs ival) eps)
			#C(0d0 0d0)
			ival)
		for kval of-type (complex double-float)
		  = (row-major-aref imfft2 i)
		for kval-stabilized  of-type (complex double-float)
		  = (if (< (abs kval) eps)
			(complex eps)
			kval)
		do
		   (setf (row-major-aref imfft1 i)
			 (/ ival-stabilized kval-stabilized))))))
    ;; and convert back, returning single or double precision array as requested
    (fftb-complex-image-to-image imfft1 :im-out im-out :double-precision double-precision)))


(defun fft-filter-image (image filter-function)
  "Convert image to Fourier space, and filter it with
multiplicative FILTER-FUNCTION, and convert it back.  

The function is called as (FILTER Y-FREQUENCY X-FREQUENCY)
and returns a floating poing rescaling factor.

The frequecy is computed using the total pixel span 
(ie, N-1 for an N image), so that a 100x100 image has frequencies [0,
0.01, 0.02 .. 0.49, 0.50, -0.49, ... -0.01]"

  (declare (type image image)
	   (type (function (single-float single-float) single-float)
		 filter-function))
  (let ((im-copy (copy-image image))
	(fft-im (fftf-image-to-complex-image image)))
    (declare (type image im-copy)
	     (type complex-image fft-im))
    (loop
      with ny = (array-dimension image 0)
      with nx = (array-dimension image 1)
      with yspan = (float (1- ny) 1.0) ;; image spans t=0...NY-1, or NY-1
      with xspan = (float (1- nx) 1.0)
      for iy of-type fixnum below ny
      for fy of-type single-float = (complex-fft-index-to-frequency iy ny yspan)
      do (loop for ix of-type fixnum below nx
	       for fx of-type single-float = (complex-fft-index-to-frequency ix nx xspan)
	       for scale of-type single-float = (funcall filter-function fy fx)
	       do (setf (aref fft-im iy ix)
			(* (aref fft-im iy ix) scale))))
    (fftb-complex-image-to-image fft-im :im-out im-copy)))
    
(defun compute-image-power-spectrum (im)
  (declare (type image im))
  (let ((ps (make-same-size-image im))
	(im-fft (fftf-image-to-complex-image im)))
    (declare (type image ps)
	     (type complex-image im-fft))
    (loop for i below (array-total-size im)
	  for x of-type (complex single-float) = (row-major-aref im-fft i)
	  do (setf (row-major-aref ps i)
		   (+ (expt (realpart x) 2)
		      (expt (imagpart x) 2))))
    ps))
    


(defun fft-wiener-deconvolve-image
    (im im-kernel
     noise-power-spectrum
     &key
       image-power-spectrum
       (eps 1e-10))
  "Return the Wiener deconvolution of image IM with response function
(filter) IM-KERNEL, assuming a particular NOISE-POWER-SPECTRUM
and taking an optional IMAGE-POWER-SPECTRUM.

Inputs are:
  IM           - the input image
  IM-KERNEL    - the response function to remove by deconvolvolution
  NOISE-POWER-SPECTRUM  - the noise power spectrum, or a constant for
                          Gaussian white noise (the variance of the noise, or
                          sigma^2)
  IMAGE-POWER-SPECTRUM  - the optional power spectrum of the noise-free 
                          ideal verion of IM.
  EPS          - a tiny number used for both the minimum allowed
                 value of the image power spectrum, and for the mininum
                 allowed value of 1/FFT(IM-KERNEL)

If IMAGE-POWER-SPECTRUM is not given, it is computed as
  IMAGE-POWER-SPECTRUM = MAX(EPS, NOISE-POWER-SPECTRUM - IMAGE-POWER-SPECTRUM)

The deconvolution formula is

  IM-OUT = InverseFFT(FFT(IM) x W / FFT(IM-KERNEL)
where the Wiener filter is
  W = IM-POWER-SPECTRUM / (IM-POWER-SPECTRUM + NOISE-POWER-SPECTRUM)"

  (declare (type image im im-kernel)
	   (type (or real image) noise-power-spectrum)
	   (type (or null image) image-power-spectrum)
	   (type single-float eps))
  
  (when (not (equalp (array-dimensions im)
		     (array-dimensions im-kernel)))
    (error "IM and IM-KERNEL have different dimensions"))

  (when (and (typep noise-power-spectrum 'image)
	     (not  (equalp (array-dimensions im)
			   (array-dimensions noise-power-spectrum))))
    (error "IM and NOISE-POWER-SPECTRUM have different dimensions."))
  
  (when (and (typep image-power-spectrum 'image)
	     (not  (equalp (array-dimensions im)
			   (array-dimensions image-power-spectrum))))
     (error "IM and IMAGE-POWER-SPECTRUM have different dimensions."))
  
  ;;
  (let ((im-fft
	  (make-complex-image (array-dimension im 0)
			      (array-dimension im 1)))
	(kernel-fft
	   (make-complex-image (array-dimension im 0)
			       (array-dimension im 1)))
	(im-ps
	  (or image-power-spectrum
	      (make-image (array-dimension im 0)
			  (array-dimension im 1))))
	(noise-ps
	  (if (typep noise-power-spectrum 'image)
	      noise-power-spectrum
	      (make-image (array-dimension im 0)
			  (array-dimension im 1)
			  :initial-value
			  (float noise-power-spectrum 1.0))))
	(out-fft ;; output image
	   (make-complex-image (array-dimension im 0)
			       (array-dimension im 1))))
    ;;
    (declare (type complex-image im-fft kernel-fft out-fft)
	     (type image im-ps noise-ps))
    ;;
    (fftf-image-to-complex-image im :fft-im-out im-fft)
    (fftf-image-to-complex-image im-kernel :fft-im-out kernel-fft)
    ;;

    (locally
	(declare (optimize debug))
      ;;
      ;; Compute power spectrum of image if it was not given.
      ;; it is taken to be ImagePS=MAX(eps,TotalSignalPS-NoisePS).
      ;; Ie, the part of the total power spectrum that is above the
      ;; presumed noise, but requiring a minimum power eps.
      (when (not image-power-spectrum)
	(loop for i below (array-total-size im)
	      for s of-type (complex single-float)
		= (row-major-aref im-fft i)
	      for s2 of-type single-float
		= (+ (expt (realpart s) 2)
		     (expt (imagpart s) 2))
	      for n2 of-type single-float
		= (row-major-aref noise-ps i)
	      do (setf (row-major-aref im-ps i)
		       (max eps (- s2 n2)))))
      ;;
      ;; apply the Wiener filter
       (loop
	 for i below (array-total-size im)
	 for h of-type (complex single-float)
	   = (row-major-aref kernel-fft i)
	 ;; stabilize 1/h against 1/0
	 for h2 of-type single-float
	   = (max (+ (expt (realpart h) 2) (expt (imagpart h) 2)) eps)
	 for 1/h = (/ (conjugate h) h2)
	 ;;
	 for s of-type (complex single-float)
	   = (row-major-aref im-fft i)
	 ;; stabilize input image against small power terms
	 for sstab = (if (< (abs s) eps)
			 #C(1.0 0.0)
			 s)
	 for sp of-type single-float ;; signal power
	   = (row-major-aref im-ps i)
	 for np of-type single-float ;; noise power
	   = (row-major-aref noise-ps i)
	 ;; the Wiener filter that modifies the usual deconvolution
	 for filtval of-type single-float
	   = (/ sp (max eps (+ sp np)))
	 do
	    (setf (row-major-aref out-fft i)
		  (* 1/h sstab filtval)))
	 
    ;; and convert back
    (fftb-complex-image-to-image out-fft))))



	   

(defun wrap-image-into-fft-kernel (im ny nx)
  "Turn an image (with an odd length in both dimensions) into a
wrap-around kernel of dimensions NX,NY suitable for fourier
operations.

The center pixel is remapped to 0,0, and pixels below it are remapped
to the other side of the image.  Eg, a centered Gaussian ends up in
the four corners of the output kernel, so that it induces no shift
when its transform is used in a Fourier convolution.

Works in either IMAGE, COMPLEX-IMAGE, DOUBLE-IMAGE, or  DOUBLE-COMPLEX-IMAGE."

  (declare (type (or image complex-image double-image double-complex-image) im)
	   (type (unsigned-byte 28) ny nx))

  (let* ((my (array-dimension im 0))
	 (mx (array-dimension im 1))
	 ;; center pix of im
	 (my0 (ash my -1))
	 (mx0 (ash mx -1)))
    
    (when (not (and (oddp my) (oddp mx)))
      (error "Input image IM does not have odd dimensions."))
    (when (or (> my ny)  (> mx nx))
      (error "Image of dimensions MY=~A, MX=~A will not fit into kernal of dimen  NY=~A, NX=~A"  (array-dimension im 0) (array-dimension im 1)
	     ny nx))

    ;; local inline function to do the copying
    (flet ((wrap-loops (kern)
	     (declare (optimize speed)
		      (type (simple-array * (* *)) kern))
	     (loop
	       for jy  of-type fixnum below my
	       ;; index in new array wrapping around
	       for iy of-type fixnum
		 = (if (>= jy my0)
		       (- jy my0)
		       (- ny (- my0 jy)))
	       do (loop
		    for jx  of-type fixnum below mx
		    ;; index in new array wrapping around
		    for ix  of-type fixnum
		      = (if (>= jx mx0)
			    (- jx mx0)
			    (- nx (- mx0 jx)))
		    do (setf (aref kern iy ix)
			     (aref im jy jx))))
	     ;; 
	     kern))
      ;;
      (declare (inline wrap-loops)) ;;  inline so compiler specializes it
      ;;
      (cond ((typep im 'image)
	     (let ((kern (make-image ny nx)))
	       (declare (type image kern))
	       (wrap-loops kern)))
	    ((typep im 'complex-image)
	     (let ((kern (make-complex-image ny nx)))
	       (declare (type complex-image kern))
	       (wrap-loops kern)))
	    ((typep im 'double-image)
	     (let ((kern (make-double-image ny nx)))
	       (declare (type double-image kern))
	       (wrap-loops kern)))
	    ((typep im 'double-complex-image)
	     (let ((kern (make-double-complex-image ny nx))) 
	       (declare (type double-complex-image kern))
	       (wrap-loops kern)))))))

    
