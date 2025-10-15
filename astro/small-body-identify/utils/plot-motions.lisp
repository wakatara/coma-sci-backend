#|
   Routines to measure the rates of all objects in 
   ASTORB + comets, plot them, and figure out some rules
   of thumb for how fast things move as a function
   of Delta and eccentricity


|#


(asdf:load-system "pgplot")
(in-package sbid)


;; list of (ra dec delta dra/dt ddec/dt speed-deg/day)
;; for each object in *orbit-element-vector*
(defvar *ra-dec-delta-rate-vec*  nil)

;; compute rates for everything directly
(defun compute-rates-for-all-orbits ()
  (format t  "This will take a few minutes ...~%")
  (setf *ra-dec-delta-rate-vec*
	(loop with mjd = (astro-time:parse-ut-date-and-time-string-to-mjd
			  "2018-10-01T00:00:00") ;; pick one date
	      for elem across *orbit-element-vector*
	      collect (multiple-value-bind (ra dec delta dra/dt ddec/dt)
			  (slalib-ephem:compute-radecr-from-comet-elem-with-rates-for-observatory
		     elem mjd "MKO" :perturb nil)
			(list ra dec delta dra/dt ddec/dt
			      (* (/ 24 3600) ;; arcsec/hr -> deg/day
				 (sqrt (+ (expt dra/dt 2)
					  (expt ddec/dt 2))))))))
  (format t "Done computing ~A rates~%" (length *ra-dec-delta-rate-vec*)))


  

(defun plot-rates (&key (device :x11) (filename nil)
			(max-ecc 9999d0))

  (let ((p (pgplot:open-device device :filename filename)))

    (pgplot:set-window p 0 3 0 10.0)
    (pgplot:box p)
    (pgplot:toplabel p "Speed vs \\gD for ASTORB + Comets")
    (pgplot:xlabel p "\\gD [AU]")
    (pgplot:ylabel p "Deg/day")
    (pgplot:draw-plot-legend
     p 0.7 0.9
     '(("e > 0.5" :point :filled-circle :color :red)
       ("e = 0.3-0.5" :point :filled-circle :color :green)
       ("e = 0.2-0.3" :point :filled-circle :color :cyan)
       ("e < 0.2" :point :filled-circle :color :default))) 
       
     

    (loop for lst in *ra-dec-delta-rate-vec*
	  for elem across *orbit-element-vector*
	  for i from 0
	  for delta = (third lst)
	  for rate =  (sixth lst)
	  for ecc = (slalib-ephem:comet-elem-e elem)
	  for color = (cond ((> ecc 0.5) :red)
			    ((> ecc 0.3) :green)
			    ((> ecc 0.2) :cyan)
			    (t :default))
	  when (< ecc max-ecc)
	    do (pgplot:points p delta rate :point :color color))

    (when filename (pgplot:close-device p))))
    
