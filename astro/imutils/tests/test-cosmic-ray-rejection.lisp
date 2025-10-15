

(defparameter *ds9* nil)

(eval-when (load eval compile)
  (asdf:load-system "imutils")
  (asdf:load-system "ds9")
  (asdf:load-system "pgplot")
  (asdf:load-system "sextractor-display")
  (asdf:load-system "cfitsio"))


(defun plot-cosmics (fits)
  "Interactively bring up a FITS file in ds9, then hand-mark cosmic rays and normal objects,
and plot the IS-COMIC-RAY fratio vs detection significance, to determine the cutoffs
to use for cosmic ray rejection."

  (let ((ds9 (ds9:open-ds9))
	(p (pgplot:open-device :x11))
	(im (cf:image-section-data (cf:read-image-section fits))))
    (pgplot:set-window p 0 2 -2 2)
    (pgplot:box p :x-log t :y-log t)
    (pgplot:xlabel p "significance")
    (pgplot:ylabel p "fratio")
    (setf *ds9* ds9)
    (ds9:load-fits-file ds9 fits) 
    (ds9:zscale ds9)
    (format t "Place cursor over objects, and hit 'c' for cosmic ray, any other key 
for other objects, and 'q' to quit.~%")
    (loop with quit = nil
	  until quit
	  do
	     (multiple-value-bind (c x y)
		 (ds9:imexam ds9)
	       (multiple-value-bind (is-cosmic detsig fedge ftot fratio)
		   (imutils:is-cosmic-ray (float x 1.0) (float y 1.0) im)
		 (declare (ignorable is-cosmic fedge ftot))
		 (let ((ldetsig (log (max 1 detsig) 10))
		       (lfratio (log (max 0.01 fratio) 10)))
			 
		   (cond ((equalp c "q")
			  (setf quit t))
			 ((equalp c "c") ;; a cosmic
			  (format t "COSMIC:  DETSIG=~,2F  FRATIO=~,2F~%" detsig fratio)
			  (ds9:plot-points ds9 (vector x) (vector y)
					   :marker :circle :size 5 :color :red)
			  (pgplot:points p (vector ldetsig) (vector lfratio)
					 :filled-circle :color :red))
			 (t
			  (format t "NORMAL: DETSIG=~,2F  FRATIO=~,2F~%" detsig fratio)
			  (ds9:plot-points ds9 (vector x) (vector y)
					   :marker :circle :size 5 :color :green)
			  (pgplot:points p (vector ldetsig) (vector lfratio)
					 :filled-circle :color :green)))))))))


(defun display-cosmics-for-fits-and-cat (fits catalog-file)
  "Display a FITS file and a sextractor CATALOG-FILE, marking cosmic rays red, good detections green,
and other sextractor flags blue."
    (sextractor-display:display-sextractor-catalog fits
     (terapix:read-sextractor-catalog catalog-file)
     :marker-function 
     (lambda (i hash)
       (let* ((flags (gethash "FLAGS" hash))
	      (flag (aref flags i)))
	 (cond ((zerop flag)
		(values :circle :green 5))
	       ((terapix:private-flag-cosmic-ray flag)
		(values :circle :red 5))
	       (t
		(values :circle :blue 5)))))))
