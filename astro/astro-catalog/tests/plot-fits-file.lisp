
(in-package astro-catalog-tests)

(defun ds9-plot-catalog-over-fits-image
  (astro-catalog
   fits-file
   &key
     (test-function (constantly t))
     ds9 wcs (marker :circle) (size 5) (color :red))
  "Open a DS9 display (or use :DS9), display FITS-FILE.
TEST-FUNCTION determines if a particular object is displayed "
  (let ((wcs (or wcs (cf:read-wcs fits-file)
		 (error "Could not get WCS for ~A" fits-file)))
	(ds9 (or ds9 (ds9:open-ds9))))
    (sleep 3)
    (ds9:load-fits-file ds9 fits-file)
    (ds9:zscale  ds9)
    (let* ((ravec (astro-catalog:get-astro-catalog-vector astro-catalog :ra))
	   (decvec (astro-catalog:get-astro-catalog-vector astro-catalog :dec))
	   (xypairs
	     (loop for ra across ravec and dec across decvec
		   for i from 0
		   when (funcall test-function astro-catalog i)
		     collect
		     (multiple-value-bind (x y)
			 (wcs:wcs-convert-ra-dec-to-pix-xy wcs ra dec)
		       (cons x y))))
	   (xvec (map 'vector 'car xypairs))
	   (yvec (map 'vector 'cdr xypairs)))
      (ds9:plot-points ds9 xvec yvec :marker marker :size size :color color)
      ds9)))
    
						 
