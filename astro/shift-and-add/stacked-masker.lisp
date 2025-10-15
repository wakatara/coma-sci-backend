#|

masker that uses sextractor catalogs to put a (by default) FWHM-FACTOR 2.5x fwhm 
blank mask around images, but takes list from a STACK image

defines class stacked-masker


|#

(in-package shift-and-add)


(defclass stacked-masker (image-weighter)
  ((fwhm-factor :initarg :fwhm-factor
		:initform 2.5
		:accessor masker-fwhm-factor)))

(defmethod run-weight-generation ((masker stacked-masker) (saaplan saaplan) fits-list)
  (stacked-masker-function saaplan fits-list
			   :fwhm-factor (masker-fwhm-factor masker)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun stacked-masker-function (saaplan fits-working-list
				&key (fwhm-factor 1.0))
  ;(print *imageout-base*)

  (let ((stack-fits (make-stationary-stack-name
		     saaplan :append-suffix t))
	(stack-dir (make-stationary-stack-name
		     saaplan :stack-dir t)))
    (saaplan-log-format saaplan "SHIFT-AND-ADD: Making stack image ~A for mask.~%"
			stack-fits)
    (build-stationary-stack saaplan fits-working-list :force-rebuild nil)
    ;;
    ;; run sextractor on the stack
    (terapix:run-sextractor 
     stack-fits
     :output nil)
    ;;
    (loop with shash =  (terapix:read-sextractor-catalog 
			 (format nil "~A/sex.cat" stack-dir))
	  for fits in fits-working-list
	  do (stack-mask-one-fits-file fits shash :fwhm-factor fwhm-factor))))



    
    
	

(defun stack-mask-one-fits-file (fits shash &key (fwhm-factor 1.0))
  (cf:maybe-with-open-fits-file (fits ff)

    ;; try to move to correct extension
    (let ((n-ext (or (ignore-errors
		      (instrument-id:get-image-extension-for-onechip-fits fits))
		     ;; if the first image is of finite size
		     (and
		      (eql (cf:fits-file-current-hdu-type ff) :image)
		      (plusp (aref (cf:fits-file-current-image-size ff) 0))
		      1)
		     2)))
      (cf:move-to-extension ff n-ext)
		      
    
      (let* ((fwhmvec (gethash "FWHM_IMAGE" shash))
	     (ra-vec (gethash "ALPHA_J2000" shash))
	     (dec-vec (gethash "DELTA_J2000" shash))
	     (wcs (cf:read-wcs ff))
	     (xvec (make-array (length ra-vec) :element-type 'double-float))
	     (yvec (make-array (length ra-vec) :element-type 'double-float))	 
	     ;;
	     (maskfits (concatenate 'string (file-io:file-basename fits) 
				    ".weight.fits"))
	     (nx (cf:read-fits-header ff "NAXIS1"))
	     (ny (cf:read-fits-header ff "NAXIS2"))
	     (im (make-array (list ny nx) :element-type '(unsigned-byte 16)
					  :initial-element 2)))
	
	;; now turn ra-vec, dec-vec into x-vec,y-vec
	(loop for i from 0
	      for ra across ra-vec
	      for dec across dec-vec
	      do (multiple-value-bind (x y)
		     (wcs:wcs-convert-ra-dec-to-pix-xy wcs ra dec)
		   (setf (aref xvec i) x
			 (aref yvec i) y)))
	
	
	(loop for x across xvec 
 	      for y across yvec
	      for fwhm across fwhmvec
	      for ix = (round x) and iy = (round y)
	      do (%simple-mask-star ix iy im fwhm :fwhm-factor fwhm-factor))
	
	(cf:write-2d-image-to-new-fits-file
	 im maskfits
	 ;; make primary hdu if the original image has one
	 :primary-hdu (= n-ext 2) 
	 :type :short :overwrite t)))))
	 
    

