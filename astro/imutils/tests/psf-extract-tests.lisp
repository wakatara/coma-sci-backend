
(eval-when (:load-toplevel :execute :compile-toplevel)
  (asdf:load-system "plot")
  (asdf:load-system "imutils"))

(use-package 'imutils)

(defun im-norm (im)
  (loop for i below (array-total-size im) sum (row-major-aref im i)))
(defun im-absnorm (im)
  (loop for i below (array-total-size im) sum (abs (row-major-aref im i))))

(defun 3dplot-image (im)
  (let ((w (cl-plplot:basic-3d-window  :altitude 30 :azimuth 60  ))
	(c (cl-plplot:new-3d-mesh
	    nil nil
	    im
	    :line-color :blue)))
         (cl-plplot:add-plot-to-window w c)
         (cl-plplot:render w "xwin")))

;; one gauss image
(defparameter *im1g* (let ((im (make-image 101 101)))
		       (add-gaussian-to-image im 50.0 50.0 5.0 5.0 0.0  1.0 :subpix 5)
		       im))


(defun make-resample-subim (im nsubpix  &key (interp-method :lanczos2))
  (make-resampled-subimage-at-xy 
   im
   (* 1.0 (ash (array-dimension im 1) -1))
   (* 1.0 (ash (array-dimension im 0) -1)) 
   :nxout (* nsubpix 101)
   :nyout (* nsubpix 101)
   :interp-method interp-method
   :nsubpix nsubpix))
					

;; we find that the resampled subimage has a similar FWHM but not exact - can't
;; find a bug so we attribute it to difficulty of interpolating exact fwhm
;; it also varies by interpolation method

;; a gaussian with an image shifted over form center
(defparameter *im1g-s* (let ((im (make-image 101 101)))
		       (add-gaussian-to-image im 50.4 50.6 5.0 5.0 0.0  1.0 :subpix 5)
		       im))

;; a 'psf' that should be centered
(defparameter *impsf1g-1s* (make-resampled-subimage-at-xy *im1g-s* 50.4 50.6
							  :nsubpix 3
							  :nxout 301 :nyout 301))

;; check if the image is in fact correctly centered (on pixel 150,150)
(defparameter *qf-impsf1g-1s* (fit-quadratic/deluxe *impsf1g-1s* 150 150 5)) ;; yes, 150.02, 149.98

;; shift the psf image back to a new center
(defparameter *impsf1g-1s-shift* nil)
(defparameter *impsf1g-1s-dx* nil) ;; ends up 42
(defparameter *impsf1g-1s-dy* nil) ;; ends up 52
(multiple-value-setq (*impsf1g-1s-shift* *impsf1g-1s-dx* *impsf1g-1s-dy*)
  (imutils::make-psf-for-xy-in-image *impsf1g-1s*
				     3 ;; unbin from 3,2
				     67.5 77.7  
				     :nx 51 :ny 51))


;; test the position of the center - yes, it is at x=25.50, y=25.7, matching the 0.5, 0.7 offsets
(defparameter *qf-impsf1g-1s-shift* (fit-quadratic/deluxe *impsf1g-1s-shift* 25 25 5))
;; now x=67.5 of parent image should be the same as pixel (25.50+42) of  *qf-impsf1g-1s-shift*,
;; now x=67.5 of parent image should be the same as pixel (25.50+42) of  *qf-impsf1g-1s-shift*,

;; now subtract the image using the offset
(let ((im-parent  (let ((im (make-image 151 151)))
		       (add-gaussian-to-image im 67.5 77.7 5.0 5.0 0.0 1.0 :subpix 5)
		    im)))
  (defparameter *im1g-zeroed-before* (copy-image im-parent))
  (image-iterate (*impsf1g-1s-shift* iy ix)
    (decf (aref im-parent (+ iy *impsf1g-1s-dy*) (+ ix *impsf1g-1s-dx*))
	  (aref *impsf1g-1s-shift* iy ix)))
  (defparameter *im1g-zeroed* im-parent))

;; and (im-norm *im1g-zeroed-before*) is 0.9999 while (im-norm *im1g-zeroed*) is 8e-7!! Success!


(defvar *fits* 
      
