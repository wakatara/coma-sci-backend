#|

Subtract static sky from an image

|#

(in-package imutils)

(defun subtract-static-sky (im imsky &key
				       (auto-backd nil)
				       (im-backd 0.0)
				       (imsky-backd 0.0)
				       (imout nil)
				       (subtract-backd-from-final-result t)
				       (out-of-bounds-value 1e5)
				       ix0 ix1 iy0 iy1)
  "Subtract a static sky image, minimizing 
  | (IM - IMS-BACKD) - (IMSKY - IMSKY-BACKD) |
By default, IMS-BACKD and IMSKY-BACKD are set to zero,
but setting flag AUTO-BACKD causes them to be computed
using the image medians.

IX0 IX1 IY1 IY1 are the region used to compute the difference
by default the whole image.  

IMOUT is the optional output image (can be either input image).

SUBTRACT-MEAN-BACKD determines whether the mean zeropoint of the image
 is subtracted.

Return (VALUES IMAGE-OUT SKY-SCALE IM-BACKD IMSKY-BACKD NEVAL)"
  (declare (type image im imsky)
	   (type (or null (unsigned-byte 24))
		 ix0 ix1 iy0 iy1)
	   (type (or null image) imout)
	   (type single-float im-backd imsky-backd out-of-bounds-value))
  
  (when (not (equalp (array-dimensions im)
		     (array-dimensions imsky)))
    (error "Images IM and IMSKY are not compatible in size"))
  
  (when (and imout (not (equalp (array-dimensions imout)
				 (array-dimensions imsky))))
    (error "Images IMOUT and IMSKY are not compatible in size"))
		     
  (let ((ix0 (or ix0 0))
	(ix1 (or ix1 (1- (array-dimension im 1))))
	(iy0 (or iy0 0))
	(iy1 (or iy1 (1- (array-dimension im 0))))
	(im-backd (* 1d0 (if auto-backd  (image-median im) im-backd)))
	(imsky-backd (* 1d0 (if auto-backd  (image-median imsky) imsky-backd)))
	(imout (or imout 
		   (make-image (array-dimension im 0) (array-dimension im 1))))
	(imflag (make-flag-image (array-dimension im 0) (array-dimension im 1))))

    (declare (type double-float im-backd imsky-backd)
	     (type image imout)
	     (type flag-image imflag)
	     (type (unsigned-byte 24) ix0 ix1 iy0 iy1))
    
    ;; fill the flag image with 1 where pixels NaN, Inf, or out of range
    (loop for i of-type fixnum below (array-total-size im)
	  for imval of-type single-float = (row-major-aref im i)
	  for skyval of-type single-float = (row-major-aref imsky i)
	  when (or  (float-utils:float-nan-or-infinity-p imval)
		    (float-utils:float-nan-or-infinity-p skyval)
		    (> (abs imval) out-of-bounds-value)
		    (> (abs skyval) out-of-bounds-value))
	    do (setf (row-major-aref imflag i) 1))
	     

    (flet ((optim-function (scaling)
	     (declare (type double-float scaling)
		      (optimize speed))
	     (loop
	       with medsum of-type double-float = 0d0
	       for ix of-type fixnum from ix0 to ix1
	       do
		  (loop
		    for iy of-type fixnum from iy0 to iy1
		    for flag = (aref imflag iy ix)
		    when (zerop flag)
		      do
			 (let* ((imval  (aref im iy ix))
				(skyval (aref imsky iy ix))
				(delta 
				  (- (- imval im-backd)
				     (* scaling
					 (- skyval imsky-backd)))))
			   (declare (type double-float delta))
			   (incf medsum (abs delta))))
	       finally
		  ;(format t "Scaling=~A  medsum=~A~%" scaling medsum)
		  (return medsum)))
	   ;;
	   (subtract-sky (scaling)
	      (declare (type double-float scaling)
		       (optimize speed))
	     (loop
	       with nx = (array-dimension imsky 1)
	       with ny = (array-dimension imsky 0)
	       for ix of-type fixnum below nx
	       do
		  (loop
		    ;; we may or may not subtract the final backd
		    for backd of-type double-float
		      = (if subtract-backd-from-final-result
			    im-backd
			    0d0)
		    for iy of-type fixnum below ny
		    for delta of-type double-float
		      = (- (- (aref im iy ix) backd)
			   (* scaling
			      (- (aref imsky iy ix)  imsky-backd)))
		    do (setf (aref imout iy ix) (float delta 1.0))))))

      ;; now get the minimum, trying golden section if it works, and brute
      ;; force if it scales
      (let (ax bx cx scale-best (neval 0))

	;; first try golden section method, but this can fail
	(when (not ax) 	;; bracketmin can fail
	  (multiple-value-setq  (ax bx cx)
	    (ignore-errors
	     (golden-section:bracketmin  0.1d0 2d0  #'optim-function :tiny 1d-10))))
	(when ax ;; bracketmin worked, so try golden section
	  (multiple-value-setq (scale-best neval)
	    (ignore-errors
	     (golden-section:findmin #'optim-function ax cx :eps 1d-5))))

	;; if golden section failed, just brute force it over 2 iterative scales
	(when (not scale-best)
	  ;; first do 10% steps in scale
	  (loop with dev-best = most-positive-double-float
		with sbest = 0d0
		for scale = 1d-7 then (* scale 1.10d0) 
		until (> scale 1d7)
		for dev = (optim-function scale)
		do (incf neval)
		when (< dev dev-best)
		  do (setf sbest scale)
		     (setf dev-best dev)
		finally (setf scale-best sbest))
	  ;; next do 0.1% steps between the two bounds of the above result
	  (loop with dev-best = most-positive-double-float
		with scale0 = (* 0.89d0 scale-best)
		with scale1 = (* 1.11d0 scale-best)
		with sbest = 0d0
		for scale = scale0 then (* scale 1.001)
		until (> scale scale1)
		for dev = (optim-function scale)
		do (incf neval)
		when (< dev dev-best)
		  do (setf sbest scale)
		     (setf dev-best dev)
		finally (setf scale-best sbest)))
	
	;;(format t "Subtracting SCALE-BEST=~A~%" scale-best)
	(subtract-sky scale-best) ;; save the final image
	(values imout scale-best im-backd imsky-backd neval)))))
			    
			

  
  
