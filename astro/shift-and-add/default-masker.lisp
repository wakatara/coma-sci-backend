#|

simple masker that uses sextractor catalogs to put a (by default) 2.5x
fwhm blank mask around images

|#

(in-package shift-and-add) 

(defun default-masker-function (saaplan fits-working-list
				&key (fwhm-factor 2.5)) 
  (declare (ignore saaplan))
  (loop for fits in fits-working-list
	do (default-mask-one-fits-file fits :fwhm-factor fwhm-factor)))


;; mask a star by FWHM-FACTOR times its fwhm
(defun %default-mask-star (ix iy im fwhm &key (fwhm-factor 1.0))
  (declare (type (signed-byte 28) ix iy)
	   (type (simple-array (unsigned-byte 16) (* *)) im))
  (let* ((fwhm (float fwhm 1d0))
	 (fwhm-factor (float fwhm-factor 1d0))
	 (r  (min (* fwhm-factor fwhm) 10d0)))
    (mask-star-in-int16-image ix iy im r 0))) 
    
	

(defun default-mask-one-fits-file (fits &key (fwhm-factor 1.0))
  (let* ((hash (terapix:read-sextractor-catalog
		(concatenate 'string (file-io:file-basename fits)
			     "_DIR" "/sex.cat")))
	 ;(fluxvec (gethash "FLUX_BEST" hash))
	 (fwhmvec (gethash "FWHM_IMAGE" hash))
	 (xvec (gethash "X_IMAGE" hash))
	 (yvec (gethash "Y_IMAGE" hash))
	 ;;
	 (maskfits (concatenate 'string (file-io:file-basename fits) ".weight.fits"))
	 (nx (cf:read-fits-header fits "NAXIS1"))
	 (ny (cf:read-fits-header fits "NAXIS2"))
	 (im (make-array (list ny nx) :element-type '(unsigned-byte 16)
			 :initial-element 2)))

    (loop for x across xvec 
 	  for y across yvec
	  for fwhm across fwhmvec
	  for ix = (round x) and iy = (round y)
	  do (%default-mask-star ix iy im (float fwhm 1d0) :fwhm-factor fwhm-factor))

    (cf:write-2d-image-to-new-fits-file im maskfits :type :short :overwrite t)))
	 
    
