#|

simple masker that uses sextractor catalogs to put a (by simple) 2.5x
fwhm blank mask around images

defines class simple-masker

|#

(in-package shift-and-add) 

(defclass simple-masker (image-weighter)
  ((fwhm-factor :initarg :fwhm-factor
		:initform 2.5
		:accessor masker-fwhm-factor)))

(defmethod run-weight-generation ((masker simple-masker) (saaplan saaplan) fits-list)
  (simple-masker-function saaplan fits-list
			  :fwhm-factor (masker-fwhm-factor masker)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simple-masker-function (saaplan fits-working-list
				&key (fwhm-factor 2.5)) 
  (declare (ignore saaplan))
  (loop for fits in fits-working-list
	do (simple-mask-one-fits-file fits :fwhm-factor fwhm-factor)))


;; mask a star by FWHM-FACTOR times its fwhm
(defun %simple-mask-star (ix iy im fwhm &key (fwhm-factor 1.0))
  (declare (type (signed-byte 28) ix iy)
	   (type (simple-array (unsigned-byte 16) (* *)) im))
  (let* ((fwhm (float fwhm 1d0))
	 (fwhm-factor (float fwhm-factor 1d0))
	 (nx (array-dimension im 1))
	 (ny (array-dimension im 0))
	 (r  (min (* fwhm-factor fwhm) 10d0))
	 (r2 (expt r 2))
	 (jx0 (max (round (- ix r)) 0))
	 (jx1 (min (round (+ ix r)) (1- nx)))
	 (jy0 (max (round (- iy r)) 0))
	 (jy1 (min (round (+ iy r)) (1- ny))))

    (loop for jx from jx0 to jx1
	  do (loop for jy from jy0 to jy1
		   for rr2 = (+ (expt (float (- ix jx)) 2)
				(expt (float (- iy jy)) 2))
		   when (<= rr2 r2)
		     do (setf (aref im jy jx) 0)))))
    
    
	

(defun simple-mask-one-fits-file (fits &key (fwhm-factor 1.0))
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
	  do (%simple-mask-star ix iy im (float fwhm 1d0) :fwhm-factor fwhm-factor))

    (cf:write-2d-image-to-new-fits-file im maskfits :type :short :overwrite t)))
	 
    
