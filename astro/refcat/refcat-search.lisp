
(in-package refcat)



(defconstant +refcat-epoch+ 57205.5d0) ;; 2015.5 in MJD


(defun get-refcat-directory (&key (error-if-none t))
  (string-right-trim
   "/"
   (or (pconfig:get-config "REFCAT:REFCAT_DIR")
       (uiop:getenv "REFCAT_DIR")
       (if error-if-none
	   (error
	    "REFCAT:REFCAT_DIR not found in PCONFIG:GET-CONFIG, and REFCAT_DIR not in Unix environment")))))

(defun have-refcat ()
  "Return T/NIL if we think we have refcat,  based on directory.  If it is HTTP, give it benefit of doubt."
  (let ((rdir (get-refcat-directory :error-if-none nil)))
    (and rdir
	 (or
	  (%refcat-is-http rdir)
	  (not ;; turn into boolean
	   (not
	    (probe-file rdir))))))) 
       

;; not really used - mostly use (get-refcat-directory)
(defparameter *refcat-directory*
  (get-refcat-directory :error-if-none nil))


;; compute which refcat square a given ra,dec falls into
(defun %which-refcat-square (ra dec)
  (when (or (>= ra 360) (< ra 0)
	    (< dec -90) (> dec 90))
    (multiple-value-setq (ra dec)
      (astro-coords:regularize-equatorial-coordinates ra dec)))
  ;; 
  (when (= dec 90) (setf dec 89.999d0))
  (let ((nra  (floor (mod ra 360)))
	(ndec (floor dec)))
    (list nra ndec)))





(defun %refcat-square-is-in-range (ra dec ira idec radius/deg)
  (declare (type double-float ra dec radius/deg)
	   (type (integer -90 89) idec)
	   (type (integer 0 359)  ira)
	   (optimize speed))
  (block ret
    ;; if the center of the square is plausibly in range, then scan
    ;; the square carefully
    (when (< (astro-coords:sky-angle ra dec (+ ira 0.5d0) (+ idec 0.5d0) :units :degrees)
	     (+ radius/deg 1d0)) ;; conservative
      
      ;;(format t "Checking ~A ~A~%" ira idec)
      ;; check if top or bottom sides intersect circle
      (loop
	with fra of-type double-float = (float ira 1d0)
	and fdec of-type double-float = (float idec 1d0)
	;; fra scans from left to right
	for  fra*  of-type double-float from fra to (+ fra 1d0) by 0.025d0
	;; check if a point on top or bottom of square is in circle
	when (or (< (astro-coords:sky-angle ra dec fra* fdec :units :degrees)
		    radius/deg)
		 (< (astro-coords:sky-angle ra dec fra* (+ 1d0 fdec) :units :degrees)
		    radius/deg))
	  do (return-from ret t))
      ;; check if right or left sides intersect circle
      (loop
	with fra of-type double-float = (float ira 1d0)
	and fdec of-type double-float = (float idec 1d0)
	;; fra scans from left to right
	for  fdec* of-type double-float from fdec to (+ fdec 1d0) by 0.025d0
	;; check if a point on right or left of square is in circle
	when (or (< (astro-coords:sky-angle ra dec fra         fdec* :units :degrees)
		    radius/deg)
		 (< (astro-coords:sky-angle ra dec (+ 1d0 fra) fdec* :units :degrees)
		    radius/deg))
	  do (return-from ret t)))))
	       
    
  


;; define a band of decs that are plausible candidates, then search
;; all of ra in this band.  This will handle the annoying polar caps,
;; at a slight loss in efficiency.
(defun list-refcat-squares-for-position (ra dec radius/deg)
  (let ((ra (float ra 1d0))
	(dec (float dec 1d0))
	(radius/deg (float radius/deg 1d0)))
    (declare (type double-float ra dec radius/deg))
    (multiple-value-setq (ra dec)
      (astro-coords:regularize-equatorial-coordinates ra dec))
    (let* ((dec-max (min 90d0    (+ dec radius/deg)))
	   (idec-max (min 89     (1+ (ceiling dec-max))))
	   (dec-min (max -90d0   (- dec radius/deg)))
	   (idec-min (max -90    (1- (floor dec-min))))
	   (this-square (%which-refcat-square ra dec))
	   (outlist (list this-square))
	   (ira0 (first this-square))
	   (idec0 (second this-square)))
      ;;(print (list idec-min idec-max))
      (loop for idec from idec-min to idec-max
	    do (loop for ira from 0 to 359
		     when (and
			   (not (and (= ira ira0)
				   (= idec idec0)))
			   (%refcat-square-is-in-range ra dec ira idec radius/deg))
		       do (push (list ira idec) outlist)))
      outlist)))
	  
    
(defun refcat-position-to-file (position)
  (let ((ira (first position))
	(idec (second position)))
    (format nil "~3,'0D~A~2,'0D.rc2" ira (if (minusp idec) "-" "+") (abs idec ))))

(defun make-refcat-subdir-for-mag (mag refcat-dir)
  (format nil "~A/~2,'0D_m_~2,'0D" refcat-dir (if (= mag 16) 0 (- mag 1)) mag))
  
				
(defun list-refcat-files-for-position (ra dec radius/deg &key
							   (mags '(16 17 18 19 20))
							   (refcat-dir (get-refcat-directory)))
  (let* ((refcat-squares (list-refcat-squares-for-position ra dec radius/deg))
	 (refcat-files   (mapcar 'refcat-position-to-file refcat-squares)))
    (loop 
      with file-list = nil
      for mag in mags
      for subdir = (make-refcat-subdir-for-mag mag refcat-dir)
      do (dolist (file (mapcar (lambda (refcat-file) (format nil "~A/~A" subdir refcat-file))
			       refcat-files))
	   (push file file-list))
      finally (return file-list))))



(defun update-refcat-entry-to-mjd (re mjd)
  (let* ((years-difference (/ (- mjd +refcat-epoch+) 365.25d0))
	 (ra (refcat-dec re))
	 (dec (refcat-dec re))
	 (dra/dt (refcat-pmra re))
	 (ddec/dt (refcat-pmdec re))
	 (delta-ra  (* years-difference 1d-3 dra/dt))
	 (delta-dec (* years-difference 1d-3 ddec/dt)))
    (multiple-value-setq (ra dec)
      (astro-coords:sky-angles-slew ra dec delta-ra delta-dec :units :arcsec))
    (setf (refcat-ra re) ra)
    (setf (refcat-dec re) dec)
    re))



(defun read-refcat-entries-for-position (ra dec radius/deg
					 &key
					   (mags '(16 17 18 19 20))
					   (update-to-mjd nil)
					   (refcat-dir (get-refcat-directory)))
  "Read all the refcat files for a given RA,DEC,RADIUS/DEG.    Pick mag ranges
in MAGS (defaults to all from 0 to 20).    If UPDATE-TO-MJD is set, then update
the RA,DEC to the MJD given."
  (let ((file-list (list-refcat-files-for-position
		    ra dec radius/deg :mags mags :refcat-dir refcat-dir)))
    (loop with output-list = nil
	  for file in file-list
	  ;; read only those entries within position
	  for entries = (read-refcat-file file :ra ra :dec dec :radius/deg radius/deg)
	  do (setf output-list (nconc output-list entries))
	  finally
	     (when update-to-mjd
	       (loop for re in output-list
		     do (update-refcat-entry-to-mjd re update-to-mjd)))
	     (return output-list)))) 
  
  
									  
  
