


(defpackage asteroids
  (:use #:cl)
  (:export 
   #:h-magnitude
   #:phase-correction
   #:apparent-magnitude
   #:h-from-diameter
   #:diameter-from-h
   #:h-from-diameter
   #:apparent-magnitude-from-diameter
   #:r-mag-from-diameter-alt-formula
   ))


(in-package asteroids)


(defun phase-correction (alpha &key (g 0.15))
  "Perform a phase angle ALPHA (deg) correction to the magnitude of an
asteroid, assuing the G value given by the keyword. This is the
additive change in magnitude by which an asteroid is brightened or
dimmed by being at angle ALPHA:  MAG_APPARENT=MAG_TRUE+PHASE_CORRECTION."
  (flet ((log10 (x) (log x 10))
	 (phi (a b alpha)
	   (exp (* (- a) (expt (tan (* 0.5 (* (/ pi 180) alpha))) b)))))
    (let ((a1 3.333) (a2 1.87) (b1 0.63) (b2 1.22))
      (* -2.5
	 (log10 (+ (* (- 1.0 g) (phi a1 b1 alpha))
		   (* g (phi a2 b2 alpha))))))))


(defun h-magnitude (mag-obs rhelio delta alpha &key (g 0.15))
  "Compute the true H magnitude from the apparent magnitude MAG-OBS,
and the known RHELIO, DELTA (in AU), and phase angle ALPHA (in degrees)."
  (let ((h (- mag-obs (* 5 (log (* rhelio delta) 10)))))
    (- h (phase-correction alpha :g g))))


(defun apparent-magnitude (h-mag rhelio delta alpha &key (g 0.15))
  "Compute the apparent magnitude given the H magnitude,
and the known RHELIO, DELTA (in AU), and phase angle ALPHA (in degrees)."
  (let ((mag-obs (+ h-mag (* 5 (log (* rhelio delta) 10)))))
    (+ mag-obs (phase-correction alpha :g g))))



(defun h-from-diameter (diameter albedo)
  "Compute H magnitude from the diameter (km) and albedo using 
D=1329 10^(-0.2 H) / sqrt(albedo)"
  (* 5d0 (log  (/ 1329 diameter (sqrt albedo))
	       10)))

(defun diameter-from-h (h albedo)
  "Compute the diameter (in km) of an asteroid with a given H and albedo,
using D=1329 10^(-0.2 H) / sqrt(albedo)"
  (/ (* 1329d0 (expt 10d0 (* -0.2d0 h)))
     (sqrt albedo)))


(defun apparent-magnitude-from-diameter
    (diameter rhelio delta alpha albedo &key (g 0.15))
  "Compute the apparent magnitude from the diameter (km), and the known
RHELIO, DELTA (in AU) and the phase angle ALPHA (in degrees."
  (apparent-magnitude (h-from-diameter diameter albedo) rhelio delta alpha :g g))


(defun r-mag-from-diameter-alt-formula
    (diameter rhelio delta alpha albedo &key (beta 0.04))
  "Alternative formula for R magnitude from DIAMETER (km), RHELIO, DELTA (AU), 
phase angle ALPHA (deg), and beta.  From K.Meech."
  (+ -27.1d0
     (* beta alpha)
     (* -2.5 (log (* (/ albedo 2.235d22)
		     (expt (/ (* 500 diameter)
			      (* rhelio delta))
			   2))
		  10))))
