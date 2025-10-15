
#|

Upsample an image, turning each image into multiple pixels through
interpolation, which can be usual methods of nearest pixel, bilinear
interpolation, and Lanczos2,3

|#


(in-package imutils)

(defun image-upsample (im &key (xscale 3) (yscale 3) (interp-method :lanczos2)
			  (preserve-flux t))
  "Upsample an image, ie increase pixel count by integer factors
  XSCALE,YSCALE

 INTERP-METHOD is one of :NEAREST, :LINEAR, :LANCZOS2, :LANCZOS3 :LANCZOS4

 PRESERVE-FLUX, T by default, means to preserve the total flux (ie, divide
    result by XSCALE*YSCALE)

Using LANCZOS will roughly bandwidth limit the subsampled image below
frequency of 1/2 (where a frequency of 1 is one cycle per pixel).  It
is not full bandwidth limitation because LANCZOS is not an ideal SINC
filter.

Returns (VALUES NEW-IMAGE XFORM-LINEAR) where XFORM-LINEAR is the object of type
XFORM-LINEAR that transforms x,y coordinates in the old image to those in the
new image, e.g. using function APPLY-XFORM-LINEAR."

  (declare (type image im)
	   (type (integer 1 10) xscale yscale)
	   (type (member :nearest :linear :lanczos2 :lanczos3 :lanczos4) interp-method))
  (let* ((nx (array-dimension im 1))
	 (ny (array-dimension im 0))
	 (mx (* nx xscale))
	 (my (* ny xscale))
	 (imout (make-image my mx))
	 ;; linear xform that maps input to output
	 (xform (imutils:build-xform-linear
		 :xscale (float xscale 1.0)
		 :yscale (float yscale 1.0)
		 ;; pixels 0,0 in the original image maps to the
		 ;; center pixel of the subarray of the new image corresponding
		 ;; to the 0,0 original pixel
		 :x0 (+ -0.5 (* 0.5 xscale))
		 :y0 (+ -0.5 (* 0.5 yscale)))))
					    
    (resample-image-with-linear-xform im imout xform :interp-method interp-method)

    (when preserve-flux
      (im-scale imout (/ 1.0 (* xscale yscale)) :image-out imout))
					  
    (values imout xform)))
    
	
	


