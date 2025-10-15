
#|

simple source detector that smooths an image and finds bumps of a
particular sigma and Gaussian fits them.

|#

(in-package imutils)

;; a photometric source
(defstruct source
  ;; floating point position - possibly tuned with quadfit
  (x 0.0 :type (single-float -1e9 1e9))
  (y 0.0 :type (single-float -1e9 1e9))
  ;; original integer position
  (ix 0 :type imindex)
  (iy 0 :type imindex)
  ;;
  (sigma :0.0 :type single-float)
  ;; quadratic fit values
  (quadfit-ok nil)
  (quadfit-result nil) ;; quadfit-result structure giving various useful data, like FWHM
  (fwhm nil :type (or null single-float)) ;; if we did quadfit
  ;;
  (ap-flux -1e10  :type single-float)
  (peak-flux 0.0  :type single-float) ;; in backd subtracted image
  (peak-flux-orig 0.0  :type single-float) ;; in original image
  (n-flagged-pix 0 :type (unsigned-byte 28))
  (errors NIL)) ;; is it OK, or was there an error?
  

#+progn
(progn
  (defparameter *imb* nil)
  (defparameter *ims* nil)
  (defparameter *imbf* nil))


(defun find-sources (image &key 
			     (sigma-detection 5.0)
			     (flag-image nil)
			     (nsmooth 5)
			     (fwhm-smooth 3.0)
			     (r-ap 10.0)
			     (subtract-background t)
			     (remove-sources-when-computing-backd t) 
			     (nbin-backd 16)
			     (r1-background 30.0)
			     (r2-background 60.0)
			     (perform-quadfit t)
			     (n-quadfit 5)
			     (negative-sources nil)
			     (return-backd-subtracted-image nil))
"Find sources in IMAGE of signifance SIGMA.  Smooths the image by
Gaussian FWHM-SMOOTH in a box of N-SMOOTH.  If SUBTRACT-BACKGROUND is
set (by default) subtracts annular median background using sampling
region R1-BACKGROUND to R2-BACKGROUND, finds peaks of at least
SIGMA-DETECTION, and fits them with a round Gaussian in a box of
N-GAUSSFIT.  Also performs aperture photometry inside R-AP.  If
NEGATIVE-SOURCES is T, then find negative sources instead of positive
ones. Returns a list of SOURCE objects.

Returns (VALUES
          SOURCE-LIST   ;; list of SOURCE objects
          BACKD-IMAGE-FUNCTION
          BACKD-SUBTRACTED-IMAGE-OR-NIL)
where BACKD-IMAGE-FUNCTION  returns (VALUES BACKD SIGMA FLAGGED-PIX-P);
see BUILD-BACKGROUND-IMAGE-FUNC for details."
           
  (declare (type image image)
	   (type (or null flag-image) flag-image)
	   (type (integer 0 1024) n-quadfit)
	   (type (single-float 0.0 1e6)
		 sigma-detection r-ap fwhm-smooth r1-background  r2-background)
	   (optimize debug))

  (let* ((null-pix-value most-negative-single-float) ;; bad pix in smoothed image
	 (nx (array-dimension image 1))
	 (ny (array-dimension image 0))
	 (nan-flag #.(ash 1 15))
	 ;; if searching for negative sources, then flip input image.  We
	 ;; always copy, because we may subtract background from image.
	 (image* (if (not negative-sources)
		     (copy-image image)
		     ;; im-scale checks for NaN and Inf and propagates them
		     (im-scale image -1.0 :image-out (make-image ny nx))))
	 ;;
	 ;; flag image, but we add invalid pixels with a logior to existing flag-image
	 (flag-image* (loop with fim of-type flag-image
			      = (if flag-image
				    (copy-flag-image flag-image)
				    (make-flag-image ny nx))
			    for i below (array-total-size image)
			    when (float-utils:single-float-nan-or-infinity-p
				  (row-major-aref image* i))
			      do (setf (row-major-aref fim i)
				       (logior (row-major-aref fim i)
					       nan-flag))
			    finally (return fim)))
	 ;;
	 (source-list nil)
	 (kernel 
	  (make-gaussian-kernel 
	   nsmooth
	   (gaussian-sigma-for-fwhm fwhm-smooth) ))
	 (im-smooth (convolve-image image* kernel :flag-image flag-image* 
						 :null-value null-pix-value))
	 ;;
	 (backd-function
	   ;; (func iy ix) => (values backd sigma flagged-p)
	   (build-background-image-func
	    image*
	    :nbin nbin-backd :rinner r1-background :router r2-background
	    :nsamp 256 :compute-sigma t :remove-sources remove-sources-when-computing-backd
	    :sigma-sources 2.5 :im-flag flag-image*)))
    
    (declare (type backd-image-function backd-function)
	     (type image image* im-smooth kernel)
	     (type flag-image flag-image*)
	     (type single-float null-pix-value)
	     (type imindex nx ny))
    
    #+nil
    (setf *imb* im-backd
	  *ims* im-sigma
	  *imbf* im-bit-flag)
    
    ;;
    ;; background subtract from BOTH the smooth image (to detection 
    ;; sigma correctly) and the copied image (to do photometry right)
    (when subtract-background
      (image-iterate (image* iy ix)
	(multiple-value-bind (backd sigma flagged-pix-p)
	    (funcall backd-function iy ix)
	  (declare (ignore sigma flagged-pix-p))
	  ;; preserve bad pix in smooth image
	  (when (not (= null-pix-value (aref im-smooth iy ix)))
	    (decf (aref im-smooth iy ix) backd))
	  (decf (aref image* iy ix) backd))))
  
 
	    
    ;; now go through the image, collecting the PIXEL location of detections.
    ;; we will then refine them.
    ;; don't scan up to edge because we're looking for local maxima
    (image-iterate (image* iy ix :ix0 1 :iy0 1 :ix1 (- nx 2) :iy1 (- ny 2))
      (multiple-value-bind (backd sigma flagged-pix-p)
	  (funcall backd-function iy ix)
	(declare (ignore flagged-pix-p))
	(let ((threshold
		(+ backd (* sigma-detection sigma)))
	      ;; central and surrounding points
	      (z (aref im-smooth iy ix)))
	  (flet ((is-a-peak () ;; is iy,ix a local peak?
		   (block nope
		     (loop for jy from -1 to 1
			   do (loop for jx from -1 to 1
				    ;; ignore central pix in comparison
				    when (not (and (= jy 0) (= jx 0)))
				      do (let ((zz (aref im-smooth
							 (+ iy jy)
							 (+ ix jx))))
					   (when
					       (or
						;; are we comparing an invalid pix?
						(= null-pix-value zz)
						;; is neighbor higher than iy,yx?
						(>= zz z))
					     (return-from nope nil))))
			   finally (return t)))))
	    ;;
	    (declare (inline is-a-peak))
	    
	    (when (and
		   ;; do NOT check 'flagged-pix-p' because
		   ;; it is flagged wherever there is a source, if we
		   ;; are removing sources, so there would be no sources
		   ;; if we rejected on the basis of flagged-pix-p
		   ;;
		   (not (zerop sigma)) ;; weird backd
		   (not (= z null-pix-value))
		   (>= z threshold)
		   (is-a-peak))

	      #+nil
	      (when (= sigma 0.0)
		(format t "Found sigma=0 at ~A ~A~%" iy ix)
		(break))

	      ;;
	      (push
	       (make-source
		:ix ix :iy iy
		:x (float ix) :y (float iy)
		:sigma (/ z sigma))
	       source-list))))))
    
    ;; 
    (dolist (source source-list)
      ;; check for bad pix in fit
      (loop
	with nx0  of-type (unsigned-byte 28) = (round (source-x source))
	with ny0  of-type (unsigned-byte 28) = (round (source-y source))
	;; NREG is the full size of the region for which we do quadfit or ap phot
	with nreg of-type (unsigned-byte 28)
	  = (max
	     (if perform-quadfit (ash n-quadfit -1) 0)
	     (1+ (round (* 2 r-ap))))
	for iy of-type (signed-byte 28) from (- ny0 nreg) to (+ ny0 nreg)
	do 
	   (loop
	     for ix of-type (signed-byte 28) from (- nx0 nreg) to (+ nx0 nreg)
	     when (and (< -1 ix nx) (< -1 iy ny)
		       (plusp (aref flag-image* iy ix)))
	       do (incf (source-n-flagged-pix source)))
	finally
	   (when (plusp (source-n-flagged-pix source))
	     (pushnew :flagged-pixels-in-fit (source-errors source))))
      ;; perform CRUDE aperture photometry, ignoring masked pixels - image
      ;; is already backd subtracted
      (loop
	with rap2 of-type single-float = (expt r-ap 2)
	with has-badpix = nil
	with flux of-type single-float = 0.0
	;; peak flux in backd subtracted image
	with peak-flux of-type single-float = most-negative-single-float
	;; peak flux in original image
	with peak-flux-orig of-type single-float = most-negative-single-float
	with nap/2 of-type (unsigned-byte 28) = (ash (1+ (ceiling (* 0.5 r-ap))) -1)
	with nx0  of-type (unsigned-byte 28) = (round (source-x source))
	with ny0  of-type (unsigned-byte 28) = (round (source-y source))
	for iy of-type (signed-byte 28) from (- ny0 nap/2) to (+ ny0 nap/2)
	do 
	   (loop
	     for ix of-type (signed-byte 28) from (- nx0 nap/2) to (+ nx0 nap/2)
	     when (and (< -1 ix nx) (< -1 iy ny)
		       	;; inside aperture? (crude)
		        (<= (+ (expt (* 1.0 (- ny0 iy)) 2)
			       (expt (* 1.0 (- nx0 ix)) 2))
			    rap2))
	       do
		  (let ((z (aref image* iy ix))
			(z-orig (aref image iy ix)))
		    (cond ((plusp (aref flag-image* iy ix))
			   (setf has-badpix t))
			  (t
			   (setf peak-flux (max z peak-flux))
			   (setf peak-flux-orig (max z-orig peak-flux-orig))
			   (incf flux z)))))
	finally
	   (setf (source-ap-flux source) (* (if negative-sources -1 +1) flux))
	   (setf (source-peak-flux source) peak-flux)
	   (setf (source-peak-flux-orig source) peak-flux-orig)
	   (when has-badpix
	     (push :bad-pixels-in-aperture-photometry (source-errors source))))
      
      (when perform-quadfit
	;; check if the quadfit region is valid
	(flet ((quadfit-region-ok-p (iy ix)
		 (let ((nq (ash n-quadfit -1)))
		   (and
		    ;; is the quadratic region not on edge?
		    (> ix nq) (< ix (- nx nq))
		    (> iy nq) (< iy (- ny nq))
		    ;; is quadfit region flagged by NaN?
		    (block scan-nan
		      (loop for jx from (- ix nq) to (+ ix nq)
			    do (loop for jy from (- iy nq) to (+ iy nq)
				     when (plusp (logand nan-flag
							 (aref flag-image* jy jx)))
				       do (return-from scan-nan nil)) ;; bad quadfit region
			    finally (return t)))))))
	  ;;
	  (let* ((backd (if subtract-background
			    0.0
			    (the single-float
				 (imutils:compute-sampled-image-median-and-sigma
				  image 50000))))
		 (ix (round (source-x source)))
		 (iy (round (source-y source)))
		 (quadfit-result (when (quadfit-region-ok-p iy ix)
				   (fit-quadratic/deluxe
						  image  
						  ix iy
						  n-quadfit
						  :background (* 1d0 backd)))))
	    (when quadfit-result
	      (setf (source-quadfit-result source) quadfit-result)
	      (setf (source-quadfit-ok source) (minusp (quadfit-result-sign quadfit-result)))
	      ;; use correct x,y from quadfit (ix,iy preserve the original pixel center)
	      (setf (source-x source) (float (quadfit-result-x0 quadfit-result) 0.0)
		    (source-y source) (float (quadfit-result-y0 quadfit-result) 0.0))
	      ;; use short-axis fwhm as the output fwhm
	      (setf (source-fwhm source)
		    (float (quadfit-result-fwhm-2 quadfit-result)
			   1.0))))))
      
      
      ) ;; end of dolist for sources
    
    
    (values source-list
	    backd-function
	    ;; return the now backd-subtracted image if asked
	    (when return-backd-subtracted-image image)
	    )))

	 
	

		      

   
		     
		     
