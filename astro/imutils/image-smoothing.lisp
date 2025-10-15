;; various image smoothing
;;  median-filter-image          - square aperture median filter
;;  mean-filter-image            - square aperture mean filter
;;  median-ring-filter-image     - median filter image using pixels in annulus r1<r<r2
;;  make-pesudo-sigma-image      - compute a local 1 sigma image using quartiles
;;  make-ring-pseudo-sigma-image - compute a local 1 sigma image using quartiles,
;;                                  but on annulus

(in-package imutils)

 
(defun median-filter-image (image &key 
			    (flag-image nil)
			    (null-value 0.0)
			    (image-out nil) (nx 5) (ny 5))
  "Block median filter an image, with a block of size NYxNX where both are odd.
If FLAG-IMAGE is given, use only pixels where FLAG-IMAGE is zero.  If
no value at point due to complete flagging, place NULL-VALUE there."
  (declare (type image image)
	   (type (or null flag-image) flag-image)
	   (type image-or-null image-out)
	   (type (unsigned-byte 14) nx ny)
	   (type single-float null-value)
	   (optimize speed))
  (when (not (and (oddp nx) (oddp ny)))
    (error "NX=~A NY=~A but both must be odd" nx ny))
  (when (and flag-image
	     (not (equalp (array-dimensions image)
			  (array-dimensions flag-image))))
    (error "Dimensions of IMAGE and FLAG-IMAGE differ"))

  (let* ((nx/2 (ash nx -1))
	 (ny/2 (ash ny -1))
	 (vscr (make-array (* nx ny) :element-type 'single-float))
	 (mx (array-dimension image 1))
	 (my (array-dimension image 0))
	 (mx-1 (1- mx))
	 (my-1 (1- my))
	 (image-out (or image-out (%dup-image-nocopy image))))
    ;;
    (declare (type (unsigned-byte 20) nx/2 ny/2)
	     (type (simple-array single-float (*)) vscr)
	     (type image image-out)
	     (type (unsigned-byte 28) mx my))

    (flet ((compute-median-at-point (iy ix)
	     (declare (type (unsigned-byte 28) ix iy))
	     (loop
                with npts of-type (unsigned-byte 28) = 0  ;; how many pix fall in block
		for jy of-type (unsigned-byte 28) 
      	        from (max 0 (- iy ny/2)) to  (min my-1 (+ iy ny/2))
		do
		  (loop
		     for jx of-type (unsigned-byte 28) 
		     from (max 0 (- ix nx/2)) to  (min mx-1 (+ ix nx/2))
		     when (or (not flag-image)
			      (zerop (aref flag-image jy jx)))
		     do
		       (setf (aref vscr npts) (aref image jy jx))
		       (incf npts))
		finally
		  (return 
		    (if (plusp npts)
			(fastmedian:fast-single-float-1d-array-median vscr npts)
			null-value)))))
      
      (loop 
	 for iy of-type (unsigned-byte 28) below my
	 do
	   (loop
	      for ix of-type (unsigned-byte 28) below mx
	      do
		(setf (aref image-out iy ix)
		      (compute-median-at-point iy ix)))))
    ;;
    image-out))
	 

(defun mean-filter-image (image &key 
			  (flag-image nil)
			  (null-value 0.0)
			  (image-out nil) (nx 5) (ny 5))
  "Block mean filter an image, with a block of size NYxNX where both are odd.
If FLAG-IMAGE is given, use only pixels where FLAG-IMAGE is zero.  If
no value at point due to complete flagging, place NULL-VALUE there."
  (declare (type image image)
	   (type image-or-null image-out)
	   (type (or null flag-image) flag-image)
	   (type (unsigned-byte 14) nx ny)
	   (type single-float null-value)
	   (optimize speed))
  (when (not (and (oddp nx) (oddp ny)))
    (error "NX=~A NY=~A but both must be odd" nx ny))

  (let* ((nx/2 (ash nx -1))
	 (ny/2 (ash ny -1))
	 (mx (array-dimension image 1))
	 (my (array-dimension image 0))
	 (mx-1 (1- mx))
	 (my-1 (1- my))
	 (image-out (or image-out (%dup-image-nocopy image))))
    ;;
    (declare (type (unsigned-byte 20) nx/2 ny/2)
	     (type image image-out)
	     (type (unsigned-byte 28) mx my))

    (flet ((compute-mean-at-point (iy ix)
	     (declare (type (unsigned-byte 28) ix iy))
	     (loop
		with sum of-type single-float = 0.0
                with npts of-type (unsigned-byte 28) = 0  ;; how many pix fall in block
		for jy of-type (unsigned-byte 28) 
      	        from (max 0 (- iy ny/2)) to  (min my-1 (+ iy ny/2))
		do
		  (loop
		     for jx of-type (unsigned-byte 28) 
		     from (max 0 (- ix nx/2)) to  (min mx-1 (+ ix nx/2))
		     when (or (not flag-image)
			      (zerop (aref flag-image jy jx))) 
		     do
		       (incf sum (aref image jy jx))
		       (incf npts))
		finally
		  (return 
		    (if (plusp npts)
			(/ sum (float npts))
			null-value))))) ;; nead float to get inlining?!

      ;;(declare (inline compute-mean-at-point))
      
      (loop 
	 for iy of-type (unsigned-byte 28) below my
	 do
	   (loop
	      for ix of-type (unsigned-byte 28) below mx
	      do
		(setf (aref image-out iy ix)
		      (compute-mean-at-point iy ix)))))
    ;;
    image-out))
	 

;;; annular template calculator to speed up annulus computations
;; compute a 2xN array with the offsets of pixels that are in 
;; an annulus centered in r1<=r<=r1.   
;; (AREF A N 0) is the ny offset of active pixel N, and 
;; (AREF A N 0) is the nx offset of active pixel N.
(defun %compute-annular-template-arr (r1 r2)
  (declare (type (single-float 0.0 1e6) r1 r2)
	   (optimize speed))
  (let* ((nmax (+ (ceiling r2) 1))
	 (nmin (- nmax))
 	 (r1sqr (expt r1 2))
	 (r2sqr (expt r2 2))
	 (npix 0)
	 (a nil))
    (declare (type simindex nmin nmax)
	     (type (unsigned-byte 28) npix)
	     (type single-float r1sqr r2sqr)
	     (type (or null (simple-array (signed-byte 24))) a))
    ;; first count the pixels
    (loop 
       for iy of-type simindex from nmin to nmax
       do
	 (loop 
	    for ix of-type simindex from nmin to nmax
	    for r2 of-type single-float = (+ (expt (float ix) 2)
					     (expt (float iy) 2))
	    when (<= r1sqr r2 r2sqr) do (incf npix)))
    ;; then make the array and fill it
    (locally (declare (optimize (speed 1))) ;; quiet compiler notes
      (setf a (make-array (list npix 2) :element-type '(signed-byte 24))))
    (loop 
       with k of-type imindex = 0
       for iy of-type simindex from nmin to nmax
       do
	 (loop 
	    for ix of-type simindex from nmin to nmax
	    for r2  of-type single-float  = (+ (expt (float ix) 2)
					       (expt (float iy) 2))
	    when (<= r1sqr r2 r2sqr) 
	    do
	      (setf (aref a k 0) iy)
	      (setf (aref a k 1) ix)
	      (incf k)))
    a))


(defun median-ring-filter-image (image &key 
				 (flag-image nil)
				 (null-value 0.0)
				 (image-out nil) (r1 0.0) (r2 5.0))
  "Block median filter an image with an annulus such that r1<r<r2; for
r1=0, this is just a disk, and for r1>0 it suppresses point sources.
If FLAG-IMAGE is given, use only pixels where FLAG-IMAGE is zero.  If
no value at point due to complete flagging, place NULL-VALUE there.

The median ignores NaN or InF.  It is OK for IMAGE-OUT to be same as IMAGE."
  (declare (type image image)
	   (type (or null flag-image) flag-image)
	   (type image-or-null image-out)
	   (type single-float null-value)
	   (type real r1 r1))

  (let* ((r1 (float r1 1.0))
	 (r2 (float r2 1.0))
	 (at (%compute-annular-template-arr r1 r2))
	 (nhotpix (array-dimension at 0))
	 ;; size of scratch vector required to contain points in annulus
	 (nscr nhotpix)
	 ;; scratch vector 
	 (vscr (make-array nscr :element-type 'single-float))
	 (mx (array-dimension image 1))
	 (my (array-dimension image 0))
	 (mx-1 (1- mx))
	 (my-1 (1- my))
	 ;; copy image to save for original, if it is the same as image-out
	 (image (if (eq image image-out)
		    (copy-image image)
		    image))
	 (image-out (or image-out (%dup-image-nocopy image))))
 
    ;;
    (declare (type (simple-array single-float (*)) vscr)
	     (type image image image-out)
	     (type single-float r1 r2) 
	     (type (simple-array (signed-byte 24)) at)
	     (type (unsigned-byte 28) nhotpix nscr mx my))

    (locally
	(declare (optimize speed))  ;; put down here to suppress compile note
      
      (flet ((compute-median-at-point (iy ix)
	       (declare (type (unsigned-byte 28) ix iy))
	       (loop
		  with npts of-type (unsigned-byte 28) = 0  ;; how many pix fall in block
		  for ihot of-type (unsigned-byte 28) below nhotpix
		  ;; go through template to find hot pixels in ring
		  for jy of-type (signed-byte 28) = (+ (aref at ihot 0) iy)
		  for jx of-type (signed-byte 28) = (+ (aref at ihot 1) ix)
		  do
		     (when (and (<= 0 jx mx-1)
				(<= 0 jy my-1)
				(or (not flag-image)
				    (zerop (aref flag-image jy jx))))
		       (let ((x (aref image jy jx)))
			 (when (not (float-utils:single-float-nan-or-infinity-p x))
			   (setf (aref vscr npts) x)
			   (incf npts))))
		  finally
		    (return 
		      (if (plusp npts)
			  (fastmedian:fast-single-float-1d-array-median
			   vscr npts)
			  null-value))))) 
	
	(loop 
	   for iy of-type (unsigned-byte 28) below my
	   do
	   (loop
	      for ix of-type (unsigned-byte 28) below mx
	      do
		(setf (aref image-out iy ix)
		      (compute-median-at-point iy ix)))))
      ;;
      image-out)))  






	 

(defun make-pseudo-sigma-image (image &key 
				(flag-image nil)
				(null-value 0.0)
				(image-out nil) (nx 11) (ny 11))
  "Compute a sigma image by taking NY x NX blocks of pixels, and
computing the sigma from quartiles.  If the 1st and 3rd quartiles are
Q1 and Q3, sigma=(Q3-Q1)/(2*0.6744897).  If FLAG-IMAGE is given, use
only pixels where FLAG-IMAGE is zero.  If no value at point due to
complete flagging, place NULL-VALUE there."
  (declare (type image image)
	   (type image-or-null image-out)
	   (type (or null flag-image) flag-image)
	   (type (unsigned-byte 14) nx ny)
	   (type single-float null-value)
	   (optimize speed))
  (when (not (and (oddp nx) (oddp ny)))
    (error "NX=~A NY=~A but both must be odd" nx ny))

  (let* ((nx/2 (ash nx -1))
	 (ny/2 (ash ny -1))
	 (vscr (make-array (* nx ny) :element-type 'single-float))
	 (mx (array-dimension image 1))
	 (my (array-dimension image 0))
	 (mx-1 (1- mx))
	 (my-1 (1- my))
	 (image-out (or image-out (%dup-image-nocopy image))))
    ;;
    (declare (type (unsigned-byte 20) nx/2 ny/2)
	     (type (simple-array single-float (*)) vscr)
	     (type image image-out)
	     (type (unsigned-byte 28) mx my))

    (flet ((compute-sigma-at-point (iy ix)
	     (declare (type (unsigned-byte 28) ix iy))
	     (loop
                with npts of-type (unsigned-byte 28) = 0  ;; how many pix fall in block
		for jy of-type (unsigned-byte 28) 
      	        from (max 0 (- iy ny/2)) to  (min my-1 (+ iy ny/2))
		do
		  (sleep 0.3)
		  (loop
		     for jx of-type (unsigned-byte 28) 
		     from (max 0 (- ix nx/2)) to  (min mx-1 (+ ix nx/2))
		     when  (or (not flag-image)
			       (zerop (aref flag-image jy jx))) 
		     do
		       (setf (aref vscr npts) (aref image jy jx))
		       (incf npts))
		finally
		  (return
		    (if 
		     (plusp npts)
		     (let* 
			 ((q1 (fastmedian:fast-single-float-1d-array-fraction 
			       vscr 0.25 npts))
			  (q3 (fastmedian:fast-single-float-1d-array-fraction 
			       vscr 0.75 npts))
			  (sigma (* (- q3 q1) #.(/ (* 2.0 0.6744897)))))
		       sigma)
		     null-value)))))
      
      
      (loop 
	 for iy of-type (unsigned-byte 28) below my
	 do
	   (loop
	      for ix of-type (unsigned-byte 28) below mx
	      do
		(setf (aref image-out iy ix)
		      (compute-sigma-at-point iy ix)))))
    ;;
    image-out))
	 



(defun make-ring-pseudo-sigma-image (image &key 
				     (flag-image nil)
				     (null-value 0.0)
				     (image-out nil) (r1 0.0) (r2 5.0))
 "Compute a sigma image by taking using an annulus R1<=R<=R2 at each
point, and computing the sigma from quartiles.  If the 1st and 3rd
quartiles are Q1 and Q3, sigma=(Q3-Q1)/(2*0.6744897).  This is to provide a 
better suppression of point sources.

If FLAG-IMAGE is given, use only pixels where FLAG-IMAGE is zero.  If
no value at point due to complete flagging, place NULL-VALUE there."
  (declare (type image image)
	   (type (or null flag-image) flag-image)
	   (type image-or-null image-out)
	   (type single-float null-value)
	   (type real r1 r1))

  (let* ((r1 (float r1 1.0))
	 (r2 (float r2 1.0))
	 (n/2 (ceiling r2)) ;; half size of block to scan
	 (at (%compute-annular-template-arr r1 r2))
	 (nhotpix (array-dimension at 0))
	 (nscr nhotpix)
	 ;; scratch vector 
	 (vscr (make-array nscr :element-type 'single-float))
	 (mx (array-dimension image 1))
	 (my (array-dimension image 0))
	 (mx-1 (1- mx))
	 (my-1 (1- my))
	 (image-out (or image-out (%dup-image-nocopy image))))

    ;;
    (declare (type (unsigned-byte 20) n/2)
	     (type (simple-array single-float (*)) vscr)
	     (type image image-out)
	     (type single-float r1 r2) 
	     (type (simple-array (signed-byte 24)) at)
	     (type (unsigned-byte 28) nhotpix nscr mx my))

    (locally
	(declare (optimize speed))  ;; put down here to suppress compile note
      
      (flet ((compute-median-at-point (iy ix)
	       (declare (type (unsigned-byte 28) ix iy))
	       (loop
		  with npts of-type (unsigned-byte 28) = 0  ;; how many pix fall in block
		  for ihot of-type (unsigned-byte 28) below nhotpix
		  ;; go through template to find hot pixels in ring
		  for jy of-type (signed-byte 28) = (+ (aref at ihot 0) iy)
		  for jx of-type (signed-byte 28) = (+ (aref at ihot 1) ix)
		  when (and (<= 0 jx mx-1)
			    (<= 0 jy my-1)
			    (or (not flag-image)
				(zerop (aref flag-image jy jx))))
		  do
		    (setf (aref vscr npts) (aref image jy jx))
		    (incf npts)
		  finally
		    (return
		      (if (plusp npts)
			  (let* 
			      ((q1 (fastmedian:fast-single-float-1d-array-fraction vscr 0.25 npts))
			       (q3 (fastmedian:fast-single-float-1d-array-fraction vscr 0.75 npts))
			       (sigma (* (- q3 q1) #.(/ (* 2.0 0.6744897)))))
			    sigma)
			  null-value)))))
	(loop 
	   for iy of-type (unsigned-byte 28) below my
	   do
	   (loop
	      for ix of-type (unsigned-byte 28) below mx
	      do
		(setf (aref image-out iy ix)
		      (compute-median-at-point iy ix)))))
      ;;
      image-out)))  



