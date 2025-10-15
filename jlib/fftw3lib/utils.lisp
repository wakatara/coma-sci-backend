

(in-package fftw3lib)

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
frequency, using D=TIMESPAN/(N-1)."
  (declare (type fixnum i n)
	   (type (or single-float double-float) timespan))
  (let ((n/2 (ash n -1))
	(1/d (/ (1- n) timespan))) ;; inverse of time interval
    (* 1/d
       (cond ((<= i n/2)
	      (/ (float i) n))
	     (t
	      (- (/ (float (- n i)) n)))))))


