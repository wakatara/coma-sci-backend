
(eval-when (load eval compile)
  (require 'plot)
  (require 'imutils))

(defun test-fft (&key (nx 110) (ny 120) (x0 0.0) (y0 0.0))
  (let* ((im (imutils:make-test-pattern-image :nx nx :ny ny))
	 (imk (imutils:make-gaussian-image :nx nx :ny ny :x0 x0 :y0 y0))
	 (imconv nil)
	 (im2 nil)
	 (imp 0))

    #+nil
    (loop for i below (array-total-size imk) do
	 (setf (row-major-aref imk i) (random 1.0)))

    (plot:plot-image im :toplabel "Original")
    (plot:plot-image imk :toplabel "Kernel")
    
    (multiple-value-setq (imconv imp)
      (imutils:fft-convolve-images im imk))
    (format t "Imag after convolving IM: ~A~%" imp)

    (plot:plot-image imconv :toplabel "Convolution")

    (multiple-value-setq (im2 imp)
      (imutils::fft-convolve-images imconv imk :action :deconvolve))
    (format t "Imag after deconvolving IM: ~A~%" imp)

    (plot:plot-image im2 :toplabel "Deconvolution")
    ))


(defun random-fft (&key (nx 4) (ny 5))
  (let* ((arr (make-array (list nx ny) :element-type 'single-float))
	 (arr2 nil)
	 (n (array-total-size arr))
	 (fft nil)
	 (imp 0))

    (loop for i below n do (setf (row-major-aref arr i) (random 1.0)))
    (format t "Input array: ~A~%~%" arr)

    (multiple-value-setq (fft imp)
      (imutils:fftf-image-to-complex-double-array arr))
    (format t "Imag after  fft: ~A~%" imp)
    (format t "FFT array: ~A~%~%" fft)

    (multiple-value-setq (arr2 imp)
      (imutils:fftb-complex-double-array-to-image fft))
    (format t "Imag after inverse fft IM: ~A~%" imp)
    (format t "Restored array: ~A~%~%" arr2)) )



(defun random-conv (&key (nx 4) (ny 5))
  (let* ((arra (make-array (list nx ny) :element-type 'single-float))
	 (arrb (make-array (list nx ny) :element-type 'single-float))
	 (arrac nil)
	 (arra2 nil)
	 (n (array-total-size arra))
	 (imp 0))

    (loop for i below n do (setf (row-major-aref arra i) (random 1.0)))
    (loop for i below n do (setf (row-major-aref arrb i) (random 1.0)))
    (format t "Input array: ~A~%~%" arra)
    (format t "Input array: ~A~%~%" arrb)

    (multiple-value-setq (arrac imp)
      (imutils:fft-convolve-images  arra arrb))
    (format t "Imag after  fft: ~A~%" imp)
    (format t "convolved array: ~A~%~%" arrac)

    (multiple-value-setq (arra2 imp)
      (imutils:fft-convolve-images arrac arrb :action :deconvolve))
    (format t "Imag after deconvolve IM: ~A~%" imp)
    (format t "Restored array: ~A~%~%" arra2)) )



    