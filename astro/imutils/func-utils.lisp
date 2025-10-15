

(in-package imutils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a few helpers for building functions with the right FWHM
;; - all have been verified to be consistent

(defun gaussian-fwhm (sigma)
  "FWHM of a Gaussian of width SIGMA"
  (* sigma (* 2 (sqrt (* 2 (log 2))))))

(defun gaussian-sigma-for-fwhm (fwhm)
  "Return sigma a Gaussian needs to have FWHM"
  (/ fwhm (* 2 (sqrt (* 2 (log 2))))))



(defun moffat-fwhm (a beta) 
  "Return FWHM for moffatt psf"
  (* a 2 (sqrt (- (expt 2 (/ 1.0 beta)) 1.0))))

(defun moffat-a-for-fwhm (fwhm beta) 
  "Return the value of A a Moffat function needs to have for
a particular FWHM and BETA"
  (* (/ fwhm 2) (/ 1.0  (sqrt (- (expt 2 (/ 1.0 beta)) 1.0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some radial profiles, for convenience rather than speed
;; these coerce the result to a single float if R or X is
;; a single-float

(declaim (#-sbcl inline #+sbcl sb-ext:maybe-inline
	  gaussian/1dnorm gaussian/2dnorm moffat 1/r-coma-func))

;; make the result of BODY be a single float if VAR is a single-float
(defmacro %maybe-coerce-to-single-float (var &body body)
  `(let ((%result ,@body))
     (if (typep ,var 'single-float)
	 (float %result 1.0)
	 %result)))

(defun gaussian/1dnorm (x sigma &key  (norm 1d0) (backd 0d0))
  "Guassian normalized to NORM in one dimension as 1/[sqrt(2 pi) sigma]"
  (%maybe-coerce-to-single-float x
    (let ((twopi (* 2 pi)))
      (+ backd
	 (* (/ norm (sqrt twopi) sigma) (exp (* -0.5 (expt (/ x sigma) 2))))))))

(defun gaussian/2dnorm (r sigma &key (norm 1d0) (backd 0d0))
  "Guassian normalized to NORM in two dimensions as 1/(2 pi sigma^2)"
  (%maybe-coerce-to-single-float r
    (let ((twopi (* 2 pi)))
      (+ backd
	 (* (/ norm twopi sigma sigma) (exp (* -0.5 (expt (/ r sigma) 2))))))))

(defun moffat  (r a beta &key (backd 0d0) (norm 1d0))
  "Mofatt function normalized to NORM in 2D"
  (%maybe-coerce-to-single-float r
    (+ backd
       (* norm
	  (/ (- beta 1d0)
	     (* pi a a))
	  (expt (+ 1d0 (expt (/ r a) 2)) (- beta))))))


(defun 1/r-coma-func (r fwhm rmax &key (backd 0d0) (norm 1d0))
  "A 1/r coma function that is of the form ~1/(1+[r/c]^2)^(1/2) -
ie, 1/r that has a FWHM core - it is normalized to have NORM  integrated
from 0 to RMAX (otherwise, it diverges logarithmically).

Note that if a Mofatt of beta=2.8 point source PSF has FWHM_PtSrc, then
a 1/r convolved Moffat will have a broadened FWHM of 1.54 FWHM_PtSrc."
  (let* ((a (* #.(/ 1d0 (sqrt 3d0)) 0.5d0 fwhm)) ;; FWHM->HWHM->a
	 (a2 (* 1d0 a a))
	 (rmax2 (* 1d0 rmax rmax))
	 ;; normalization constant 
	 (c (* 2 pi (-
		     (* a (sqrt (the (double-float (0d0)) (+ a2 rmax2))))
		     a2))))
    (%maybe-coerce-to-single-float r
	(+ backd
	   (* norm
	      (/ 1.0 c)
	      (/ 1.0 (sqrt (the (double-float (0d0)) (+ 1.0 (expt (/ r a) 2))))))))))


