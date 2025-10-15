;; simple aperture photometry

(in-package imutils)


;; return subvector (or original) of non-NaN, non-Inf numbers 
(defun %get-good-number-subvector (v)
  (declare (type (simple-array single-float (*)) v))
  (let* ((n (length v))
	 (ngood (loop for x of-type single-float across v
		      when (not (float-utils:single-float-nan-or-infinity-p x))
			count 1))
	 (v2 (cond ((= ngood n)
		    v)
		   ((= ngood 0)
		    (error "No valid numbers in vector for %get-good-number-subvector."))
		   (t
		    (make-array ngood :element-type 'single-float)))))
    (when (not (= ngood n))
      (loop for x of-type single-float across v
	    with j = 0
	    when (not (float-utils:single-float-nan-or-infinity-p x))
	      do (setf (aref v2 j) x)
		 (incf j)))
    v2))

	   

(defun ap-phot (im x y
		&key
		  (r-phot 5.0)
		  (r-background-1 10.0) (r-background-2 20.0)
		  (gain 1.0)
		  (nsubpix 10)
		  (linterp nil)
		  (background-noise-method :sigma))
  "Perform aperture photometry at X,Y in array IM, within a central aperture
of R-PHOT, subtracting a background obtained by medianing in a ring
from R-BACKGROUND-1 to R-BACKGROUND-2.  Multiply final answer by GAIN (1.0 by
default).  GAIN is also used to compute errors as
   
    FLUX_ERR^2 = FLUX_IN_PHOTONS + (BACKD_ERR*APERTURE_AREA)

BACKGROUND-NOISE-METHOD is either 
   :SQRT-COUNTS - use the square root of counts and gain
   :SIGMA       - use the observed pixel histogram in the background aperture.
                  This is useful when the image is background subtracted, or
                  we don't trust gain.

If LINTERP is true, then bi-linearly interpolate pixel values.
   WARNING - this is not necessarily better. It may produce spurious results.

Returns 

  (VALUES FLUX FLUX-ERROR
          BACKGROUND-FLUX/PIX 
          BACKGROUND-FLUX/PIX-ERROR   ;; the error in the per-pixel background
          N-PIX-FLUX N-PIX-BACKGROUND
          SIGMA-BACKGROUND)  ;; the sigma in the background, not the error OF the background

where fluxes, background, and background eror are in PHOTONS (gain
adjusted), and the N-PIX-.. are the numbers of good pixels used for
the measurement.
" 
  (declare (type image im)
	   (type (single-float (0.0)) gain)
	   (type (single-float 0.0 1e6) x r-phot r-background-1 r-background-2)
	   (type (member :sqrt-counts :sigma) background-noise-method)
	   (optimize debug))

  (when (not (>= r-background-2 (+ 1.0 r-background-1)))
    (error " R-BACKGROUND-2 is not larger than R-BACKGROUND-1 + 1: R-BACKGROUND-1=~A and  R-BACKGROUND-2=~A" r-background-1 r-background-2))


  (when (not (member  background-noise-method
		      '(:sqrt-counts :sigma)))
    (error "BACKGROUND-NOISE-METHOD =~A not one of :SQRT-COUNTS :SIGMA"
	   background-noise-method)) 
  
  (multiple-value-bind (sum-center-adu npix-center)
      (if (not linterp)
	  ;(image-sum-in-annulus/wholepix im x y 0.0 r-phot)
	  (image-sum-in-annulus im x y 0.0 r-phot :nsubpix nsubpix)
	  (image-sum-in-annulus/linterp im x y 0.0 r-phot :nsubpix nsubpix))
    (declare (type single-float sum-center-adu npix-center))

    #+nil
    (format t "sum: ~A  npix: ~A  nan: ~A~%" sum-center-adu npix-center
	    (float-utils:single-float-nan-or-infinity-p sum-center-adu))
   
    (let* ((sum-center (* gain sum-center-adu)) ;; in electrons
	   (v-backd (gather-pixels-in-annulus im x y r-background-1 r-background-2))
	   (npix-background (length v-backd))
	   (v-backd-good
	     (%get-good-number-subvector v-backd))
	   (median-background
	     (* gain  ;; in electrons
		(fastmedian:fast-single-float-1d-array-median v-backd-good)))
	   (median-background-safe ;; used only for :SQRT-COUNTS noise method
	     (if (plusp median-background) median-background 1e10))
	   (sigma-backd ;; one sigma variation in background pixels, in electrons
	     ;; if requested compute the backd error using the sigma in the annulus
	     (cond ((eq background-noise-method :sigma)
		    (let ((q25 (fastmedian:fast-single-float-1d-array-fraction
				v-backd-good 0.25))
			  (q75 (fastmedian:fast-single-float-1d-array-fraction
				v-backd-good 0.75)))
		      (* gain ;; answer is in electrons
			 (* (- q75 q25) #.(/ 1.0 1.3489795003921632e0)))))
		   ;; otherwise using the sqrt of the median flux 
		   (t ;; sqrt-counts 
		    (sqrt (the (single-float (0.0))
			       median-background-safe)))))
	     
	   ;; all quantities should be in electrons, so we can ignore
	   ;; gain from now on
	   (backd-error 
	     ;; the median in backd aper is 1.25 x more noisy than mean
	     (* 1.25 (/ sigma-backd (sqrt npix-background))))

	   ;;
	   (adj-apflux ;; center flux in e- and backd subtracted
	     (- sum-center
		(* npix-center median-background)))
	   ;;
	   ;; regardless of whether we are computing backd error,
	   ;; error IN aperture is given by sqrt(excess over backd
	   ;; flux) added in quadrature to sigma-backd *
	   ;; sqrt(npix-center)
	   (adj-apflux-err 
	     (sqrt
	      (the (single-float (0.0))
		   (+
		    ;; square of error from backd
		    (* (expt sigma-backd 2) npix-center)
		    ;; square of error from excess (over backd) flux in center
		    (if (plusp adj-apflux) adj-apflux 1e20))))) )

      (declare (type (simple-array single-float (*)) v-backd)
	       (type single-float sigma-backd)
	       (type (unsigned-byte 24) npix-background))
	   
	(values
	 adj-apflux adj-apflux-err
	 median-background backd-error
	 npix-center npix-background
	 sigma-backd))))
	  
  
