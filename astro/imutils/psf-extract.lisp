

(in-package imutils)



(defun make-resampled-subimage-at-xy (im x0 y0
				      &key
					(nsubpix 3)
					(interp-method :lanczos2)
					(nxout 51) (nyout 51)
					(imout nil)
					(norm 1.0))
					 
				    
  "Resample an image NOUT into a possibly finer-sampled output image IMOUT,
   returning (VALUES IMOUT LINEAR-XFORM) where LINEAR-XFORM transforms
   coordinates in IM to IMOUT.

   This does not perform background subtraction.

   IM - input image
   X0,Y0 - center of PSF in image IM
   NSUBPIX - factor by which the pixels of IM are sub-sampled (3 means
             the output has 3x smaller pixels.
   INTERP-METHOD - one of :NEAREST :LINEAR :LANCZOS2 :LANCZOS3 :LANCZOS4
   NXOUT,NYOUT - the size of the output image - suggest they be ODD
                 so there is a single center pixel.
   IMOUT - optional output image to fill, must be NYOUT x NXOUT
   NORM - output image is normalized to this level, if non-nil."

  (declare (type image im)
	   (type single-float x0 y0)
	   (type (integer 1 10) nsubpix)
	   (type (integer 1 #.(expt 2 14)) nxout nyout)
	   (type (member :nearest :linear :lanczos2 :lanczos3 :lanczos4) interp-method)
	   (type (or null image) imout)
	   (type (or null single-float) norm))

  (when imout
    (%check-dims-n imout nxout nyout "IMOUT vs NXOUT,NYOUT"))
  
  (let* ((imout (or imout (make-image nyout nxout)))
	 ;; center pixel (or half-pixel if even) of output image
	 (ox0 (+ -0.5 (* 0.5 nxout)))
	 (oy0 (+ -0.5 (* 0.5 nyout)))
	 ;; linear xform that maps input to output
	 (xform (imutils:build-xform-linear
		 :xscale (float nsubpix 1.0)
		 :yscale (float nsubpix 1.0)
		 :angle-deg 0.0
		 ;; pixel x0,y0 in the original image maps
		 ;; to (nxout-1)/2.0, (nyout-1)/2.0 in the new image
		 :x0 (- ox0 (* nsubpix x0))
		 :y0 (- oy0 (* nsubpix y0)))))
    
    (resample-image-with-linear-xform im imout xform :interp-method interp-method)

    ;; normalize if requested
    (when norm
      (loop with sum of-type double-float
	    for i of-type fixnum below (array-total-size imout)
	    do (incf sum (row-major-aref imout i))
	    finally
	       (im-scale imout (float (/ norm sum) 1.0) :image-out imout)))
				   
    (values imout xform)))



(defun make-combined-resampled-images-at-xy-vectors
    (im xvec yvec
     &key
       (nsubpix 3)
       (interp-method :lanczos2)
       (nxout 51) (nyout 51)
       (stack-type :median))
  "Using MAKE-RESAMPLED-SUBIMAGE-AT-XY, create a set of resampled
sub-images of IM at locations in XVEC,YVEC, and combine to make a single
unit-normalized image.

This does not perform background subtraction.

   NSUBPIX - factor by which the pixels of IM are sub-sampled (3 means
             the output has 3x smaller pixels.
   INTERP-METHOD - one of :NEAREST :LINEAR :LANCZOS2 :LANCZOS3 :LANCZOS4
   NXOUT,NYOUT - the size of the output image - suggest they be ODD
                 so there is a single center pixel.
   COMBINE-TIME - :median or :mean


"
  (declare (type image im)
	   (type (integer 1 10) nsubpix)
	   (type vector xvec yvec))
  
  (when (or (not (= (length xvec) (length yvec)))
	    (> (length xvec) 100))
    (error "XVEC and YVEC should be equal length images of length less than 101"))

  (let ((image-list
	  (loop for x across xvec
		for y across yvec
		collect (make-resampled-subimage-at-xy
			 im
			 (float x 1.0)
			 (float y 1.0)
			 :nsubpix nsubpix
			 :interp-method interp-method
			 :nxout nxout :nyout nyout
			 :norm 1.0))))
    ;;
    (image-stack image-list :stack-type stack-type)))


(defun %put-sources-in-kd-tree (source-list)
  (let ((kdt (kdtree-jk:build-kdtree 2))
	(v2 (make-array 2 :element-type 'double-float)))
    (dolist (source source-list)
      (kdtree-jk:insert-2d kdt (source-x source) (source-y source)
			   source :vec v2 :defer t)
      (kdtree-jk:balance-kdtree kdt))
    kdt))


;; count how many other sources are within DIST of SOURCE, with flux > source-fluX * MAX-FRAC-FLUX
(defun %count-bright-sources-around (kdt source dist &key kdresult tmpvec (max-frac-flux 0.02))
  (let ((vxy (or tmpvec (make-array 2 :element-type 'double-float)))
	(kdr (or kdresult (kdtree-jk:build-kdresult))))
    (setf (aref vxy 0) (* 1d0 (source-x source)))
    (setf (aref vxy 1) (* 1d0 (source-y source)))
    (kdtree-jk:kd-search-in-radius kdt vxy (* 1d0 dist) :kdresult kdr)
    (loop with max-flux = (* max-frac-flux (source-ap-flux source))
	  for i below (kdtree-jk:kdresult-n kdr)
	  for dist across (kdtree-jk:kdresult-dist-vec kdr)
	  for index across (kdtree-jk:kdresult-index-vec kdr)
	  for neighbor-source = (aref (kdtree-jk:kdtree-obj-vec kdt) index)
	  for flux = (source-ap-flux neighbor-source)
	  when (and (not (eq source neighbor-source))
		    (>= flux max-flux))
	    count 1)))
	
  

(defun %get-good-sources
    (source-list
     &key
       (saturation-level 5e4)
       (flux-min 30000.0)  ;; min allowed ap-flux for source
       (flux-max 1e6)
       (fwhm-tol 0.05)	  ;; fwhm has to be within this 25th pecentile
       ;; criteria for rejecting crowding
       (dist-tol/fwhm 4) ;; good source must be this many FWHM away from others
       (frac-flux-neighbor 0.05) ;; good source, if within dist-tol/fwhm, must be this much less bright
       )
  ;;
  (let* ((good-flux-sources ;; sources that fulfill flux criteria, brightest first
	   (let ((slist
		   (sort 
		    (loop for source in source-list
			  when (and (< flux-min  (source-ap-flux source) flux-max)
				    (not (source-errors source))
				    (< (source-peak-flux-orig  source) saturation-level))
			    collect source)
		    '>
		    :key 'source-ap-flux)))
	     (when (zerop (length slist))
	       (error "Zero sources passed the test ~,3F < FLUX < ~,3F" flux-min flux-max))
	     slist))
	 (fwhm-vec ;; vector of FWHM of good sources
	   (map '(simple-array single-float (*))
		'source-fwhm
		good-flux-sources))
	 ;; take the lower 25% of fwhm to push us away from galaxies
	 ;; THIS SCRAMBLES FWHM-VEC!! 
	 (fwhm-25 (fastmedian:fast-single-float-1d-array-fraction  fwhm-vec 0.25))
	 ;; within 5% of typical fwhm
	 (good-fwhm-sources
	   (loop for source in good-flux-sources
		 for fwhm = (source-fwhm source)
		 when (< (* (- 1.0 fwhm-tol) fwhm-25) fwhm (* (+ 1.0 fwhm-tol) fwhm-25))
		   collect source))
	 ;; isolated sources
	 (isolated-good-sources
	   (loop with kdt = (%put-sources-in-kd-tree source-list)
		 with kdr = (kdtree-jk:build-kdresult)
		 with vxy = (make-array 2 :element-type 'double-float)
		 for source in good-fwhm-sources
		 for dist = (* dist-tol/fwhm (source-fwhm source))
		 when (zerop (%count-bright-sources-around
			      kdt source dist
			      :kdresult kdr
			      :tmpvec vxy
			      :max-frac-flux frac-flux-neighbor))
		   collect source))
							   
	 )

    #+nil
    (progn
      (format t "FWHM-25: ~,4F  dist-tol/fwhm: ~A Search dist: ~A~%"
	      fwhm-25 dist-tol/fwhm (* dist-tol/fwhm fwhm-25))
      (format t "good-flux: ~A   good-fwhm: ~A     isolated: ~A~%"
	      (length good-flux-sources)
	      (length good-fwhm-sources)
	      (length isolated-good-sources)))

    isolated-good-sources))


;; like %get-good-sources, but iterate until we get enough good sources
(defun %get-good-sources/iterate
    (source-list
     &key
       (n-min-sources 3)
       (n-max-sources 10)
       (saturation-level 5e4)
       (flux-min 30000.0)  ;; min allowed ap-flux for source
       (flux-max 1e6)
       (fwhm-tol 0.05)	  ;; fwhm has to be within this 25th pecentile
       ;; criteria for rejecting crowding
       (dist-tol/fwhm 8.0) ;; good source must be this many FWHM away from others
       (frac-flux-neighbor 0.02) ;; good source, if within dist-tol/fwhm, must be this much less bright
       )

  ;; gradually loosen criteria
  (loop for niter below 8
	for %flux-min = flux-min then (* %flux-min 0.9)
	for %fwhm-tol = fwhm-tol then (* %fwhm-tol 1.1)
	for %dist-tol/fwhm = dist-tol/fwhm then (* 0.90 %dist-tol/fwhm)
	for %frac-flux-neighbor = frac-flux-neighbor then (* 1.15 %frac-flux-neighbor)
	for good-sources
	  = (%get-good-sources source-list
			       :saturation-level saturation-level
			       :flux-min %flux-min
			       :flux-max flux-max
			       :fwhm-tol %fwhm-tol
			       :dist-tol/fwhm %dist-tol/fwhm
			       :frac-flux-neighbor %frac-flux-neighbor)
	for ngood = (length good-sources)
	when (>= ngood n-min-sources)
	  do (return
	       (values (subseq good-sources 0 (min n-max-sources ngood))
		       `((:flux-min . ,%flux-min)
			 (:flux-max . ,flux-max)
			 (:fwhm-tol . ,%fwhm-tol)
			 (:dist-tol/fwhm . ,%dist-tol/fwhm)
			 (:frac-flux-neighbor . ,%frac-flux-neighbor)
			 (:iterations . ,niter))))))
		 
  
	

(defun extract-psf-from-image (im &key
				    ;; for find-sources
				    (subtract-background t) ;; only if not subtracted before
				    (flag-image nil)
				    (r1-background 30.0)
				    (r2-background 60.0)
				    (r-ap 8.0) ;; used for flux estimate of source
				    (n-quadfit 5)
				    ;; for picking sources
				    (flux-min 30000.0)
				    (flux-max 3e6)
				    (n-min-sources 3)
				    (n-max-sources 10)
				    (saturation-level 60e4)
				    ;; output stack parameters
				    (nsubpix 3)
				    (nxout 51)
				    (nyout 51)
				    (interp-method :lanczos2)
				    (stack-type :median))
  "Extract a psf from image IM, of dimension NYOUT x NXOUT, with sub-pixel binning of NSUBPIX

First, sources are extracted with FIND-SOURCES; then isolated sources
close to the 25th percentile of FWHM are found and combined to produce
a PSF image.  FLUX-MIN and other paramers are iteratively loosened
until there are at least N-MIN-SOURCES good sources to use.

Returns PSF-IMAGE, with the PSF at the center position.

Keyword parameters

 - source extraction parameters; see function FIND-SOURCES for detail
    SUBTRACT-BACKGROUND - T by default; NIL only if image not backd subtracted
    R1-BACKGROUND, R2-BACKGROUND - size of background annulus
    R-AP - aperture for flux estimate
    FLAG-IMAGE - if given, 1 means a bad pixel
    N-QUADFIT - number of pixels use for quadratic fit to measure center
                and center of source

- source selection parameters
    FLUX-MIN, FLUX-MAX - min and max source fluxes to use
    N-MIN-SOURCES - minimum number of sources to use to compute PSF
    N-MAX-SOURCES - maximum number of sources to use
    SATURATION-LEVEL - initial flux level that disqualifies a source
   
 - psf image parameters
    NSUBPIX - subpixels relative to original pixels
    NXOUT, NYOUT - psf image size
    INTERP-METHOD - one of standard interpolation methods
    STACK-TYPE - :median or :mean
    
"				  
 

  (declare (type image im)
	   (type (or null flag-image) flag-image)
	   (type single-float saturation-level r1-background
		 r2-background r-ap flux-min flux-max)
	   (type (integer 1 10) nsubpix)
	   (type (integer 1 1024) n-quadfit)
	   (type image-interp-method interp-method))
  (multiple-value-bind (source-list backd-func image-bs)
      ;; image-bs is background subtracted image
      (find-sources im
		    :sigma-detection 20.0 ;; higher S/N
		    :flag-image flag-image 
		    :nsmooth 5
		    :r-ap r-ap 
		    :fwhm-smooth 3.0
		    :perform-quadfit t
		    :n-quadfit n-quadfit
		    :r-ap 8.0 ;; used for source estimate
		    :subtract-background subtract-background
		    :nbin-backd 16
		    :r1-background r1-background
		    :r2-background r2-background
		    :return-backd-subtracted-image t)
    (declare (ignorable backd-func))
    ;; filter out sources at edges
    (let ((source-list-not-at-edge
	    (loop with nx = (array-dimension im 1)
		  with ny = (array-dimension im 1)
		  ;; half-width of output stamp plus a few pixels to avoid edge effects
		  with dx = (+ 10 (ash nxout -1)) ;; a bit of a safety buffer
		  with dy = (+ 10 (ash nyout -1)) ;; a bit of a safety buffer
		  for source in source-list
		  for x = (source-x source) and y = (source-y source)
		  when (and (< dx x (- nx dx 1))
			    (< dy y (- ny dy 1)))
		    collect source)))
		  
      
      (let ((good-sources (%get-good-sources/iterate
			   source-list-not-at-edge
			   :n-min-sources n-min-sources
			   :n-max-sources n-max-sources
			   :saturation-level saturation-level
			   :flux-min flux-min
			   :flux-max flux-max
			   :dist-tol/fwhm 8.0
			   :frac-flux-neighbor 0.2)))
	(when (not good-sources)
	  (error "Could not find enough good sources satisfying flux, psf, and isolation criteria"))
	
	(let* ((x-vec (map '(simple-array single-float (*)) 'source-x good-sources))
	       (y-vec (map '(simple-array single-float (*)) 'source-y good-sources))
	       (im-psf
		 (make-combined-resampled-images-at-xy-vectors 
 		  image-bs x-vec y-vec
		  :nsubpix nsubpix
		  :nxout nxout
		  :nyout nyout
		  :interp-method interp-method
		  :stack-type stack-type)))
	  ;;
	  im-psf)))))
		
	      
(defun make-psf-for-xy-in-image (im-psf nsubpix x y
				 &key
				   (nx 51)
				   (ny 51)
				   im-psf-out
				   (interp-method :lanczos2)
				   (norm 1.0))
  "Make an NY x NX image from the IM-PSF image, so that the PSF is centered on
the same fractional pixel as X,Y in some parent image.

NBIN is the binning factor of the 

Returns

   (VALUES IM-PSF-OUT DX DY)

where IM-PSF is the NY x NX image, and DX and DY are typically negative
integer offsets to add to coordinates in the original image.

Ie, pixel  IY,IX in the original image corresponds to pixel IY+DY,IX+DX
of IM-PSF.

IM-PSF is an optional NY x NX image to use instead of allocating one."
  (declare (type (integer 1 1024) nx ny)
	   (type (integer 1 10) nsubpix)
	   (type (float 0.0 1e6) x y)
	   (type image im-psf)
	   (type (or null image) im-psf-out)
	   (type single-float norm))
  ;;
  (when im-psf-out (%check-dims-n im-psf-out nx ny "IM-PSF"))
  ;; integer and fractional pix of posiiton
  (multiple-value-bind (mx fx) (floor x)
    (multiple-value-bind (my fy) (floor y)
       
      (let* ((%im-psf-out (or im-psf-out (make-image ny nx)))
	     ;; size of input psf array
	     (nxi (array-dimension im-psf 1))
	     (nyi (array-dimension im-psf 0))
	     ;; center fractional pixel of input 
	     (xi0 (+ -0.5 (* 0.5 nxi)))
	     (yi0 (+ -0.5 (* 0.5 nyi)))
	     ;; center integer pixel of output
	     (nxo0 (ash nx -1))
	     (nyo0 (ash ny -1)) 
	     ;; fractional pixel of psf center in output
	     (xo0 (+ nxo0 fx))
	     (yo0 (+ nyo0 fy))
	     ;;
	     ;; the xform has to map (xo0,yo0) --> (xi0,yi0)
	     (xform (imutils:build-xform-linear
		     :xscale (* 1.0 nsubpix)
		     :yscale (* 1.0 nsubpix)
		     :angle-deg 0.0
		     ;; pixel x0,y0 in the original image maps
		     ;; to (nxout-1)/2.0, (nyout-1)/2.0 in the new image
		     :x0 (- xi0 (* 1.0 nsubpix xo0))
		     :y0 (- yi0 (* 1.0 nsubpix yo0))))
	     ;; normalization for output
	     (pscale (/ norm (* nsubpix nsubpix))))
	;;
	;; iterate over coords in new stamp
	(image-iterate/subpixel (%im-psf-out x y
				 nsubpix nsubpix
				 :ixvar ix :iyvar iy)
	  ;; compute coords in original stamp
	  (multiple-value-bind (xx yy)
	      (apply-xform-linear xform x y)
	    ;; interpolate in original image
	    (let ((interp-val (general-interpolate-image 
			       im-psf xx yy 
			       interp-method)))
	      (incf (aref %im-psf-out iy ix)
		    (* interp-val pscale)))))
	  
	(values %im-psf-out
		(- mx nxo0)
		(- my nyo0))))))
	  
    
        
				  
    
						       
		
  
    
	   
  
