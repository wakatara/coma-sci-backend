
;; misc test/debug routines
(in-package small-body-identify)
(asdf:load-system "pgplot")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test functions
(defparameter *elem4* 
  (astorb:get-comet-elem-for-nth-asteroid 4))
(defun elem (n)  (astorb:get-comet-elem-for-nth-asteroid n))
    
(defparameter *mjd* (astro-time:parse-ut-date-and-time-string-to-mjd
			 "2018-08-01"))
(defparameter *ccomp* (sbid::make-ccomp-for-mjdtt *mjd*))
(defun diffs (elem mjd)
	   (multiple-value-bind (ra dec)
	       (slalib-ephem:compute-radecr-from-comet-elem-for-observatory
		elem mjd "MKO" :perturb nil :correct-mjd-to-tt nil)
	     (multiple-value-bind (rae dece delta)
		 (sbid::compute-ra-dec-using-ccomp *ccomp* elem)
		 (values
		  (- ra rae)
		  (- dec dece)
		  ra dec delta))))

(defun diffs+pv (elem mjd)
	   (multiple-value-bind (ra dec delta dradt ddecdt)
	       (slalib-ephem:compute-radecr-from-comet-elem-with-rates-for-observatory
		elem mjd "MKO" :perturb nil)
	     (multiple-value-bind (rae dece deltae pv)
		 (sbid::compute-ra-dec-using-ccomp *ccomp* elem)
	       (declare (ignore deltae))
	       (multiple-value-bind (rapv decpv)
		   (slalib-ephem:compute-radecr-from-pv-for-observatory
		    *mjd*  "MKO" pv) 
		 (values
		  (- ra rae)
		  (- dec dece)
		  (- ra rapv)
		  (- dec decpv)
		  ra dec delta dradt ddecdt)))))

(defvar *bad-indices* nil)
(defun find-max-diffs (&key (mjdtt *mjd*) (nstep 1))
  (setf *bad-indices* nil)
  (loop with ngood = 0
	with maxerr = 0d0
	for i below (astorb:astorb-n astorb:*the-astorb*) by nstep
	for elem = (astorb:get-comet-elem-for-nth-asteroid i)
	do (multiple-value-bind (dra ddec ra dec delta)
	       (ignore-errors (diffs elem mjdtt))
	     (when dra
	       (incf ngood)
	       (let ((err (* 3600 (sqrt (+ (* dra dra) (* ddec ddec))))))
		 (when (> err 40)
		   (push (list i err ra dec delta) *bad-indices*))
		 (setf maxerr (max maxerr err)))))
	finally (return
		  (list
		   :ntot (astorb:astorb-n astorb:*the-astorb*)
		   :ngood ngood
		   :maxerr maxerr))))

(defun plot-bad-diffs-radec (&key (mjdtt *mjd*) (nstep 10))
  (let ((p (pgplot:open-device :x11)))
    (pgplot:set-window p 0 360 -90 90)
    (pgplot:box p)
     (loop with ngood = 0
	with maxerr = 0d0
	for i below (astorb:astorb-n astorb:*the-astorb*) by nstep
	for elem = (astorb:get-comet-elem-for-nth-asteroid i)
	do (multiple-value-bind (dra ddec ra dec delta)
	       (ignore-errors (diffs elem mjdtt))
	     (when dra
	       (incf ngood)
	       (let* ((err (* 3600 (sqrt (+ (* dra dra) (* ddec ddec)))))
		      (color
			(cond ((> err 80) :red)
			      ((> err 60) :orange)
			      ((> err 40) :green)
			      ((> err 20) :cyan)
			      ((> err 10) :blue)
			      (t :default)))
		      (ptype (cond ((> err 20) :filled-circle)
				   (t :point))))
		 (when (> err 10)
		   (pgplot:points p ra dec ptype :color color))))))))

(defun plot-diffs-vs-delta (&key (mjdtt *mjd*) (nstep 10) (dt 0d0))
  (let ((p (pgplot:open-device :x11)))
    (pgplot:set-window p 0 3 0 150)
    (pgplot:box p)
    (loop with ngood = 0
	  with mjdtt* = (+ mjdtt (/ dt (* 24d0 3600)))
	  with maxerr = 0d0
	  for i below (astorb:astorb-n astorb:*the-astorb*) by nstep
	  for elem = (astorb:get-comet-elem-for-nth-asteroid i)
	  do (multiple-value-bind (dra ddec ra dec delta)
		 (ignore-errors (diffs elem mjdtt*))
	       (setf dra (* dra 3600))
	       (pgplot:points p delta dra
			      (if (< dra 20) :point :filled-circle)
			      :color :blue)
	       (setf ddec (* ddec 3600))
	       (pgplot:points p delta  ddec 
			      (if (< ddec 20) :point :filled-circle)
			      :color :red)))))
						  
					      
