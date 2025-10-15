

(eval-when (:load-toplevel :execute :compile-toplevel)
  (asdf:load-system "stats")
  (asdf:load-system "random")
  (asdf:load-system "pgplot"))


(defun compare-mean-median-hodges-lehman (&key (ntot 100000) (npts 10)
					    (fbad 0.1) (xbad 10d0)
					    (log t))
  "Histogram the results of estimating boxcar-contaminated Gaussian data
with fraction FBAD in boxcar [-xbad,xbad] using

  BLUE - simple mean
  RED  - simple median
  DEFAULT - Hodges-Lehman method

where
  NPTS is the number of points to combine for a single estimate
  NTOT is the number of points with which to build histogram.
"

  (loop with v = (make-array npts :element-type 'double-float)
	with vscr = (make-array (ash (* npts (1- npts)) -1)
				:element-type 'double-float)
	with vmean = (make-array ntot :element-type 'double-float)
		 with vmed = (make-array ntot :element-type 'double-float)
	with vhl = (make-array ntot :element-type 'double-float)
		 for i below ntot
	do (loop for j below npts
		 for x = (if (< (random:uniform) fbad)
				      (random:uniform-in-range (- xbad) (+ xbad))
				      (random:gaussian))
		 do (setf (aref v j) (float x 1d0)))
	   (setf (aref vmean i) (stats:mean-of-elements v))
	   (setf (aref vmed i)  (stats:median-of-elements v))
	   (setf (aref vhl i)  (stats:hodges-lehman-estimator/dbl 
				v :vscratch vscr ))
	finally
	   (let ((p (pgplot:open-device :x11)))
	     (pgplot:hist p vmean :xleft -3 :xright 3 :color :blue :log log)
	     (pgplot:box p)
	     (pgplot:hist p vmed :xleft -3 :xright 3 :color :red :set-window nil :log log)
	     (pgplot:hist p vhl :xleft -3 :xright 3 :color :default 
					  :set-window nil :log log))))




					  
