(eval-when (load eval compile)
  (asdf:load-system "artdata")
  (asdf:load-system "imutils")
  (asdf:load-system "pgplot"))



(defun make-populated-image (&key (nx 1024) (ny 1024)
			       (nstars 1000)
			       (min-flux 10000.0) (max-flux 1e6)
			       (background-flux 1000.0)
			       (ncosmics 0)
			       (cosmic-flux/pix 400.0))
  (let ((im (artdata:make-gaussian-star-array
	     :nstars nstars :nx nx :ny ny :min-flux min-flux :max-flux max-flux 
	     :background-flux background-flux
	     :add-gaussian-noise t)))

    (loop with im2 = (imutils:make-image ny nx)
	  for i below ncosmics
	  for cosmic-length = (+ 1 (random 10))
	  for cosmic-angle = (random (float (* 2 pi) 1.0))
	  for x0 = (float (random nx) 1.0)
	  for y0 = (float (random ny) 1.0)
	  for x1 = (+ x0 (* cosmic-length (cos cosmic-angle)))
	  for y1 = (+ y0 (* cosmic-length (sin cosmic-angle)))
	  do (imutils:draw-line im2 x0 y0 x1 y1 cosmic-flux/pix)
	  finally
	     (loop for j below (array-total-size im2)
		   do (incf (row-major-aref im j) (row-major-aref im2 j))))
    im))


(defun filter-image (im)
  (let ((imf (imutils:apply-3x3-laplace-filter im)))
    (loop for j below (array-total-size imf)
	  do (setf (row-major-aref imf j) (abs (row-major-aref imf j))))
    imf))
  
  

