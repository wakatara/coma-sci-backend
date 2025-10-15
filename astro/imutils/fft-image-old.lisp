

(in-package imutils)

(declaim (inline complex-fft-index-to-frequency))
(defun complex-fft-index-to-frequency (i n)
  "Given an array of size N (0...N-1), and given index I, return the frequency
corresponding to I - WARNING - not absolutely sure this is right."
  (declare (type fixnum i n))
  (let ((n/2 (ash n -1)))
    (cond ((< i n/2)
	   (/ (float i) n))
	  (t
	   (- (/ (float (- n i)) n))))))


(defun fftf-image-to-complex-double-array (im &key (fft-im-out nil))
  "Perform a foward ('f') FFT of 2d floating point array IM,
turning it into a COMPLEX DOUBLE array.  This involves
quadrupling its size, but complex double representation is done
for expediency.  The Fourier representation is normalized by
1/SQRT(N) so that a transform followed by a reverse transform
should leave the image unaffected.

If FFT-IM-OUT is given, it should be a COMPLEX DOUBLE array of the same dimensions
as IM;  is is filled with the FFT.

The frequency at index K is just K

because the output (of one dimensional input V[] ) is
 
  FFT[j] =  \sum_{i=0}^(N-1) V[k] exp(-2 pi i j (i/n))

IM is not altered.
"
  (declare (type image im)
	   (type (or null (simple-array (complex double-float) (* *))) fft-im-out))

  (when (and fft-im-out
	     (not (equalp (array-dimensions im) (array-dimensions fft-im-out))))
    (error "Dimensions of IM and FFT-IM-OUT do not match"))

  (let* ((nx (array-dimension im 1))
	 (ny (array-dimension im 0))
	 (norm (/ 1d0 (sqrt nx) (sqrt ny)))
	 ;; the wsaves for the 2 dims
	 (wx (make-array (+ 15 (* 4 nx)) :element-type 'double-float))
	 (wy (make-array (+ 15 (* 4 ny)) :element-type 'double-float))
	 ;; the scratch vectors
	 (vx (make-array nx :element-type '(complex double-float)))
	 (vy (make-array ny :element-type '(complex double-float)))
	 ;; output float array
	 (im-out (or fft-im-out 
		     (make-array (list ny nx) :element-type '(complex double-float)))))
    (declare 
     (type (unsigned-byte 25) nx ny)
     (type double-float norm)
     (type (simple-array double-float (*)) wx wy)
     (type (simple-array (complex double-float) (*)) vy vy)
     (type (simple-array (complex double-float) (* *)) im-out))
    ;;
    ;; prepare the wsaves
    (dfftpack:zffti nx wx)
    (dfftpack:zffti ny wy)
    ;;
    ;; fft rows
    (locally (declare (optimize speed))
      (loop 
	 for iy below ny 
	 do 
	 ;; copy to scratch
	   (loop 
	      for jx below nx 
	      for r of-type double-float = (float (aref im iy jx) 1d0)
	      do
		(setf (aref vx jx) (complex r 0d0)))
	 ;; do fft on vx
	   (dfftpack:zfftf nx vx wx)
	 ;; copy to output
	   (loop for jx below nx 
		do
		(setf (aref im-out iy jx) (aref vx jx)))))
    
    ;; and fft columns
    (locally (declare (optimize speed))
      (loop 
	 for ix below nx
	 do
	 ;; copy col into scratch
	   (loop 
		for jy below ny
		do
		(setf (aref vy jy) (aref im-out jy ix)))
	   ;; do fft on vy
	   (dfftpack:zfftf ny vy wy)
	   ;; copy back to output
	    (loop 
		for jy below ny
		do
		 (setf (aref im-out jy ix) (aref vy jy)))))

    ;; normalize
    (locally (declare (optimize speed))
      (loop for i below (array-total-size im-out)
	   do (setf (row-major-aref im-out i)
		    (* norm (row-major-aref im-out i)))))

    im-out))



(defun fftb-complex-double-array-to-image (imfft &key (im-out nil))
  "Given a complex double float array IMFFT, compute the inverse
fourier transform and place into a REAL float image.  Also return
the largest imaginary element that was discarded.

IMFFT is overwritten with its fourier transform.
"
  (declare (type (simple-array (complex double-float) (* *)) imfft)
	   (type (or null image) im-out))
  
  (when (and im-out
	     (not (equalp (array-dimensions imfft) (array-dimensions im-out))))
    (error "Dimensions of IMFFT and IM-OUT do not match"))

  (let* ((nx (array-dimension imfft 1))
	 (ny (array-dimension imfft 0))
	 (norm (/ 1d0 (sqrt nx) (sqrt ny)))
	 (biggest-imag 0d0)
	 ;; the wsaves for the 2 dims
	 (wx (make-array (+ 15 (* 4 nx)) :element-type 'double-float))
	 (wy (make-array (+ 15 (* 4 ny)) :element-type 'double-float))
	 ;; the scratch vectors
	 (vx (make-array nx :element-type '(complex double-float)))
	 (vy (make-array ny :element-type '(complex double-float)))
	 ;; output float array
	 (im-out (make-array (list ny nx) :element-type 'single-float)))
    (declare 
     (type (unsigned-byte 25) nx ny)
     (type (simple-array double-float (*)) wx wy)
     (type (simple-array (complex double-float) (*)) vy vy)
     (type double-float norm biggest-imag))
    ;;
    ;; prepare the wsaves
    (dfftpack:zffti nx wx)
    (dfftpack:zffti ny wy)
    ;;
    ;; fft rows
    (locally (declare (optimize speed))
      (loop 
	 for iy below ny 
	 do 
	 ;; copy to scratch
	   (loop 
	      for jx below nx 
	      do
		(setf (aref vx jx) (aref imfft iy jx)))
	 ;; do fft on vx
	   (dfftpack:zfftb nx vx wx)
	 ;; copy to output
	   (loop for jx below nx 
	      do
		(setf (aref imfft iy jx) (aref vx jx)))))
    ;;
    ;; fft columns
    (locally (declare (optimize speed))
      (loop 
	 for ix below nx
	 do
	 ;; copy col into scratch
	   (loop 
	      for jy below ny
	      do
		(setf (aref vy jy) (aref imfft jy ix)))
	   ;; do fft on vy
	   (dfftpack:zfftb ny vy wy)
	   ;; copy back to output
	    (loop 
		for jy below ny
		do
		 (setf (aref imfft jy ix) (aref vy jy)))))
    
    ;; normalize
    (locally (declare (optimize speed))
      (loop for i below (array-total-size imfft)
	 do (setf (row-major-aref imfft i)
		  (* norm (row-major-aref imfft i)))))

    ;; and copy over to the single-float array
    (locally (declare (optimize speed))
      (loop 
	 for i below (array-total-size imfft)
	 for z of-type (complex double-float) =  (row-major-aref imfft i)
	 for zr of-type double-float = (realpart z)
	 for zi of-type double-float = (imagpart z)
	 do 
	   (setf (row-major-aref im-out i) (float zr 1.0))
	   (setf biggest-imag (max biggest-imag (float zi)))))

    (values im-out biggest-imag)))




(defun fft-convolve-images (im1 im2 &key (imfft1 nil) (imfft2 nil) (im-out nil) (action :convolve))
  "Convolve two images - ie, multiply their fourier transforms, using a complex fft.
IMFFT1,2 are an optional double-float scratch arrays, and IM-OUT
is an optional single float output target array. All array
dimensions must match.

If ACTION is DECONVOLVE, perform a deconvolution instead,
dividing FFT(IM1) by FFT(IM2) instead of multiplying.

Note that the kernel is in wrap-around order.  The indices 0,0
correspond to x=0, y=0.  Positive indices 1,2,3 are forward
spatial offsets,and indices N-1,N-2, etc are negative spatial
offsets."

  (declare (type image im1 im2)
	   (type (or null  image) im-out)
	   (type (or null (simple-array (complex double-float) (* *))) imfft1 imfft2)
	   (type (member :convolve :deconvolve) action))
  
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
  (let ((imfft1 (or imfft1 (make-array (array-dimensions im1) :element-type '(complex double-float))))
	(imfft2 (or imfft2 (make-array (array-dimensions im1) :element-type '(complex double-float))))
	(im-out (or im-out (make-array (array-dimensions im1) :element-type 'single-float))))
    ;;
    (fftf-image-to-complex-double-array im1 :fft-im-out imfft1)
    (fftf-image-to-complex-double-array im2 :fft-im-out imfft2)
    ;;
    ;; either multiply or divide in fourier space
    (locally (declare (optimize speed))
      (cond ((eq action :convolve)
	     (loop 
		for i below (array-total-size im1)
		do (setf (row-major-aref imfft1 i)
			 (* (row-major-aref imfft1 i)
			    (row-major-aref imfft2 i)))))
	    ((eq action :deconvolve)
	     (loop 
		for i below (array-total-size im1)
		for kval of-type (complex double-float) = (row-major-aref imfft2 i)
		for ival  of-type (complex double-float) = (row-major-aref imfft1 i)
		do 
		  (when (zerop kval) (error "FFT kernel value is 0 in deconvolution"))
		  (setf (row-major-aref imfft1 i) (/ ival kval))))))

    ;;
    ;; and convert back
    (fftb-complex-double-array-to-image imfft1 :im-out im-out)))

		
	
