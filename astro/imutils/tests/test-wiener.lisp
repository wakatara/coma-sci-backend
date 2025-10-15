

(eval-when (load eval compile)
  (require 'imutils)
  (require 'plot)
  (require 'cfitsio))

(defparameter *im*
  (cf:image-section-data
   (cf:read-image-section "/Volumes/data/Users/kleyna/Desktop/Lowell-Trip/Data/20141126/n4.528r.fits")))

(loop for i below (array-total-size *im*)
      for x = (row-major-aref *data* i)
      when (or (> (abs x) 1e4)
	       (sb-ext:float-nan-p x))
	do (setf (row-major-aref *data* i) 0.0))

(defparameter *trail* nil)


(defun make-trail (&key (length 8) (size 101))
  (let ((kern (imutils:make-image size size)))
    
    (loop with k0 = (ash size -1)
	  with l0 = (ash length -1) 
	  for k from (- l0) to l0
	  for x = (+ k0 (* 0.9 k))
	  for y = (+ k0 (* -0.2 k))
	  do (setf (aref kern (round y) (round x)) 1.0))
  
  (imutils:im-scale kern
		    (float
		     (/ 1.0 (imutils:image-sum *trail*)) 1.0)
		    :image-out kern)
    (setf *trail*
	  (imutils::wrap-image-into-fft-kernel
	   kern
	   (array-dimension *im* 0)
	   (array-dimension *im* 1)))
    t))

(defparameter *noise-variance*
   (imutils:make-image (array-dimension *im* 0) 
		      (array-dimension *im* 1)
		      :initial-value 221.0))


(defparameter *deconv-im*
  (imutils::fft-wiener-deconvolve-images
   *im* *trail* *noise-variance*))











