#|

Build a 1/r convolved with a Moffat

This is currently UNUSED but it is meant to address the issue of
modeling the coma using the approximate imutils:1/r-coma-func in
scad-coma-fraction.lisp

In theory, the code below provides a better model of a 1/r coma.  But
does it really matter, if a 1/r coma is a leap of faith anyway, as is
the Moffat function psf?

Basically it verifies that a 1/r coma convolved a FWHM Moffat function
looks a lot like the toy imutils:1/r-coma-func 1/(r^2+a^2)^0.5
function fixed to the same FWHM, but the match isn't perfect.  In
particular, the FWHM of the convolved function is close to the
original FWHM.

What we did find is that if the PSF is beta=2.8 Moffat profile,
with a given FWHM, then the convolved function has a FWHM that is 1.54 
times larger.  

We incorported this finding into the code.

|#

(in-package simple-comet-activity-detector)


(defun build-moffat-x-1/r-table (&key (n 1023) (n-fwhm 50) (beta 2.8))
  "Return a radial profile made by convolving a Moffat profile with index BETA
and a width given by FWHM=1 with a 1/r profile.   This is meant to represent 
a seeing-convolved comet coma.   The value of PROFILE(R=0) is set to be 1.0.

N and N-FWHM are used to construct the image for the Fourier convolution."

  (when (not (oddp n))
    (error "N must be odd"))
  (let* ((im-1/r (imutils:make-image n n))
	 (im-mof (imutils:make-image n n))
	 (mof-a (imutils:moffat-a-for-fwhm (* 1.0 n-fwhm) beta))
	 (kern-mof nil) ;; moffat wrapped around
	 (im-1/r-mof nil)
	 (n/2 (ash n -1))
	 (r/2 (* n/2 1.0)))
    ;;
    ;; fill 1/r image 
    (loop for ix below n
	  do (loop for iy below n
		   for r = (max 1.0 ;; prevent blowup at r=0
				(sqrt (+ (expt (- ix n/2) 2)
					 (expt (- iy n/2) 2))))
		   do (setf (aref im-1/r iy ix) (/ 1.0 r))))

    (imutils:add-moffat-to-image im-mof r/2 r/2 mof-a mof-a beta 0.0 1.0
				 :dist (* 1.0 n))

    (setf kern-mof (imutils:wrap-image-into-fft-kernel im-mof n n))

    (setf im-1/r-mof (imutils:fft-convolve-images kern-mof im-1/r))

    (multiple-value-bind (fvec rvec)
	(imutils:compute-radial-profile  im-1/r-mof r/2 r/2 :compute-error nil)
      ;;(imutils:compute-radial-profile  im-mof r/2 r/2 :compute-error nil)
      ;; now rescale r vector so that FWHM is 1.0 
      (loop for i below (length rvec)
	    do (setf (aref rvec i)
		     (/ (aref rvec i) n-fwhm)))
      ;; now rescale integrated flux to be 1.0 at center
      (loop with f0 = (aref fvec 0)
	    for i below (length rvec)
	    for f across fvec
	    for r across rvec
	    do (setf (aref fvec i) (/ f f0)))
      (values fvec rvec))))


(defun build-moffat-x-1/r-function (&key  (n 1023) (n-fwhm 50.0) (beta 2.8))
  "Use BUILD-MOFFAT-X-1/R-TABLE to build a function for a Moffat
  convolved with a 1/r coma.  Extrapolates as 1/r beyond a radius of ~n/2"
  (multiple-value-bind (fvec rvec)
      (build-moffat-x-1/r-table :n n :n-fwhm n-fwhm :beta beta)
    (declare (type (simple-array single-float (*)) fvec rvec))
    (let* ((dr (- (aref rvec 1) (aref rvec 0)))
	   (nr (length rvec))
	   (rmax (aref rvec (1- nr)))
	   (flast (aref fvec (1- nr)))
	   ;;  beyond rmax, function is c/r
	   (c (* flast rmax)))
      ;;(format t "rmax is ~A~%" rmax)
      (lambda (r)
	(declare (type (single-float 0.0) r))
	(cond
	  ;; extrapolate
	  ((>= r rmax)
	   (/ c r))
	  ;; interpolate
	  (t
	   (let* ((n (floor (/ r dr)))
		  (r1 (aref rvec n))
		  (f1 (aref fvec n))
		  (f2 (aref fvec (1+ n)))
		  (frac (/ (- r r1) dr)))
	     ;;(print (list :n n :r1 r1 :f1 f1 :f2 f2 :frac frac))
	     (+ (* frac      f2)
		(* (- 1.0 frac) f1)))))))))
	     
		      



    
