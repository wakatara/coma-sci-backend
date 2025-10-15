(eval-when (:load-toplevel :compile-toplevel :execute)
  (asdf:load-system "plot")
  (asdf:load-system "stats")
  (asdf:load-system "random")
  (asdf:load-system "imutils"))


(defparameter *nx* 256)
(defparameter *ny* 257)
(defparameter *sigma0* 5.0) ;; sigma of initial points


;; an initial grid of gaussians
(defparameter *im*
  (loop
    with ngrid = 3
    with sigma = *sigma0*
    with im = (imutils:make-image *ny* *nx*)
    with dy = (/ *ny* (+ 1.0 ngrid))
    with dx = (/ *nx* (+ 1.0 ngrid))
    for ix below ngrid
    for x from dx by dx
    do (loop for iy below ngrid
	     for y from dy by dy
	     do
		(imutils:add-gaussian-to-image 
		 im x y sigma sigma 0.0 1.0))
    finally (return im)))


;; make an elongated kernel
(defparameter *kernel*
  (let* ((n 51)
	 (cent (* 1.0 (ash n -1)))
	 (im (imutils:make-image n n)))
    (imutils:add-gaussian-to-image im cent cent  10.0 1.0 0.0 1.0 
				   :dist 51.0) 
    im))
		

;; the kernel packed into 4 corners for convolution purposes
(defparameter *ckernel*
  (imutils:wrap-image-into-fft-kernel *kernel* *ny* *nx*))


;; the convolved image
(defparameter *im-conv*
  (imutils:fft-convolve-images *im* *ckernel*))

;; tested - this works
(defparameter *im-deconv*
  (imutils:fft-convolve-images *im-conv* *ckernel* :action :deconvolve))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; try a 10% wrong kernel for the deconvolution
(defparameter *kernel-wrong*
  (let* ((n 51)
	 (cent (* 1.0 (ash n -1)))
	 (im (imutils:make-image n n)))
    (imutils:add-gaussian-to-image im cent cent  11.0 1.1 0.0 1.0 
				   :dist 51.0) 
    im))
		

;; the kernel packed into 4 corners for convolution purposes
(defparameter *ckernel-wrong*
  (imutils:wrap-image-into-fft-kernel *kernel-wrong* *ny* *nx*))

;; Reconstruction looks totally wrong with 10%-wrong deconv kernel
(defparameter *im-deconv-wrong*
  (imutils:fft-convolve-images *im-conv* *ckernel-wrong* :action :deconvolve))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try Wiener deconvolution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *wiener-noise* (* 0.1 (stats:max-of-elements *im*)))
(defparameter *im-noisy*
  (loop with im2 = (imutils:copy-image *im*)
	for i below (array-total-size *im*)
	for noise = (* *wiener-noise* (float (random:gaussian) 1.0))
	do (incf  (row-major-aref im2 i) noise)
	finally (return im2)))

(defparameter *im-conv-noisy*
  (imutils:fft-convolve-images *im-noisy* *ckernel*))


(defparameter *im-deconv-wiener*
  (imutils:fft-wiener-deconvolve-image
   *im-conv-noisy* *ckernel* (* 1 (expt *wiener-noise* 2))))

				       
