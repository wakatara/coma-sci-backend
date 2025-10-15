
;; reject junk detections in images (mostly hot pix)

(in-package imutils)


(defun default-cosmic-ray-selection-function (fratio detsig)
  "The default selection function to determine whether something
is a cosmic ray.  FRATIO = FLUX/EDGEFLUX and DETSIG is 
a rough detection significance."
  (cond ((< detsig 3.0) ;; low sig detections aren't tossed out because we can't tell if cosmic
	 nil)
	((< detsig 4.0)
	 (> fratio 0.9))
	((< detsig 10.0)
	 (> fratio 0.4))
	((< detsig 20.0)
	 (> fratio 0.3))
	(t
	 (> fratio 0.2))))

(defun is-cosmic-ray (x y img &key (scratch nil)
			(selector-function #'default-cosmic-ray-selection-function))
  "Return T if an image is a junk detection; X and Y are 1-based
floating point indices of detection center.

Method: compute total flux in a box, and then the 'edgy' flux, defined
as the pixel-summed variance of the flux.

Cosmic rays have a high edgy flux, and anything with EDGY_FLUX/FLUX>MAGIC-RATIO
is a cosmic ray.

SCRATCH can be NIL, or a 121 element single float vector.

SELECTOR-FUNCTION is a function of FRATIO and DETSIG and is set by default
to be one computed using code in test-cosmic-ray-detection.lisp.
"
  (declare (type (single-float -1e6 1e6) x y)
	   (type image img)
	   (type (or null (simple-array single-float (121))) scratch)
	   (optimize speed))

  (let ((nx (array-dimension img 1))
	(ny (array-dimension img 0))
	(ix (1- (round x)))  ;; convert to zero based indices
	(iy (1- (round y)))
	(scr (or scratch (make-array 121 :element-type 'single-float)))
	(nscr 0)
	(ihx 0) (ihy 0) ;; hottest pixel locations
	(max-flux most-negative-single-float) ;; hottest pix
	(medflux 0.0)
	(fluxdev 0.0)
	(detsig 0.0) ;; statistical signifance of detection, very rough
	(is-cosmic nil))
    (declare (type (signed-byte 20) nx ny ix iy nscr ihx ihy)
	     (type  (simple-array single-float (*)) scr)
	     (type single-float max-flux medflux fluxdev detsig))

    (block rblock
      (cond
	;; if out of bounds, it is junk
	((or (< ix 0) (>= ix nx)
	     (< iy 0) (>= iy ny))
	 (setf is-cosmic t)
	 (return-from rblock (values is-cosmic 9999.0)))
	;;
	(t
	 ;; find hottest pixel in 11x11 box, and compute median in this box
	 (loop
	   for kx of-type (signed-byte 20)
	   from (max 0 (- ix 5)) to (min (1- nx) (+ ix 5))
	   do (loop
		for ky of-type (signed-byte 20)
		from (max 0 (- iy 5)) to (min (1- ny) (+ iy 5))
		for imval = (aref img ky kx)
		when (not (float-utils:single-float-nan-p imval))
		  do (setf (aref scr nscr) imval)
		     (incf nscr)
		     ;; set hottest only for close neighbors (2 pix)
		     (when (and (<= (abs (- kx ix)) 2)
				(<= (abs (- ky iy)) 2))
		       (when (> imval max-flux)
			 (setf max-flux  (aref img ky kx))
			 (setf ihx kx
			       ihy ky)))))
	 ;; no pixels, so it is a bad detection
	 (when (zerop nscr)
	   (setf is-cosmic t)
	   (return-from rblock (values is-cosmic 9999.0)))
	 ;;
	 (setf medflux (fastmedian:fast-single-float-1d-array-median scr nscr))
	 (setf fluxdev ;; a pseudo sigma
	       (* 0.7413011 ;; convert quartiles to gaussian sigma
		(- (fastmedian:fast-single-float-1d-array-fraction scr 0.75 nscr)
		   (fastmedian:fast-single-float-1d-array-fraction scr 0.25 nscr))))

	 (setf fluxdev (max fluxdev 1.0)) ;; just in case deviation is zero

		   
	 ;;
	 ;;
	 ;; now compute ratio of total flux to 'edgy' flux defined as change from adjacent pixels
	 (let ((ftot 0.1)
	       (fedge 0.0))
	   (declare (type single-float ftot fedge))
	   (loop ;; loop for pixels around the hottest pixel
		 for kx of-type (signed-byte 20)
		 from (max 0 (- ihx 4)) to (min (1- nx) (+ ihx 4))
		 do (loop
		      for ky of-type (signed-byte 20)
		      from (max 0 (- ihy 4)) to (min (1- ny) (+ ihy 4))
		      for imval = (aref img ky kx)
		      when (not (float-utils:single-float-nan-p imval))
			do 
			   (let ((flux (- imval medflux)))
			     (incf ftot flux)
			     ;; loop over surrounding pixels, ignoring pix itself
			     (loop for hx of-type (signed-byte 20) from (- kx 1) to (+ kx 1)
				   do (loop for hy of-type (signed-byte 20) from (- ky 1) to (+ ky 1)
					    when (and (>= hx 0) (< hx nx)
						      (>= hy 0) (< hy ny)
						      (not (and (= hx kx) (= hy ky)))) ;; don't do pix itself
					      do (let ((fnext (aref img hy hx)))
						   (when (not (or (and (zerop hx) (zerop hy))
								  (float-utils:single-float-nan-p fnext)))
					      ;; add the jumps around kx,ky to fedge
					      ;; big jumps mean cosmic ray
						      (incf fedge
							    (* 0.125 ;; 8 pixels in neighbor
							       (expt (- (- fnext medflux)
									flux)
								     2))))))))))
	   (setf fedge (sqrt (the (single-float 0.0) fedge)))
	   ;; significance of detection, given pixel to pixel variation fluxdev and 5x5 box
	   (setf detsig (/ ftot (* 8 fluxdev))) ;; at least 10 sigma

	   ;; This was set by graphing a hand selected set of cosmics vs real data on a cfht field
	   ;; the separation was very clean
	   (let* ((fratio (/ fedge ftot)))
	     
	     (setf is-cosmic (funcall selector-function fratio detsig))
		   
	     (values
	      is-cosmic
	      detsig
	      fedge ftot fratio))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline median-3 bogo-median-9) )

;; compute fast median of 3 floats
(defun median-3 (x1 x2 x3)
  (declare (type single-float x1 x2 x3)
	   (optimize (speed 3) (safety 0)))
  (when (> x1 x3)  (rotatef x1 x3)) ;; now x1,x3 are in order but x2 can be anywhere
  (if (< x2 x1)
      (rotatef x2 x1)
      (if (> x2 x3)
	  (rotatef x2 x3)))
  x2)


#+NIL	 
(defun median-4 (x1 x2 x3 x4)
  (declare (type single-float x1 x2 x3 x4)
	   (optimize (speed 3) (safety 0)))
  ;; sort the first 3
  (when (> x1 x3)  (rotatef x1 x3)) ;; now x1,x3 are in order but x2 can be anywhere
  (if (< x2 x1)
      (rotatef x2 x1)
      (if (> x2 x3)
	  (rotatef x2 x3)))
  ;; x1,x2,x3 are sorted
  (* 0.5 
     (cond ((>= x4 x3)
	    (+ x2 x3))
	   ((>= x4 x1)
	    (+ x2 x4))
	   (t
	    (+ x1 x2)))))
	 

;; compute a bogus median of 9 elements - bogus because
;; it is really a median of 3 sub-medians.
(defun bogo-median-9 (x1 x2 x3 x4 x5 x6 x7 x8 x9)
  (declare (type single-float x1 x2 x3 x4 x5 x6 x7 x8 x9)
	   (optimize (speed 3) (safety 0)))
  (median-3
   (median-3 x1 x2 x3)
   (median-3 x4 x5 x6)
   (median-3 x7 x8 x9)))

 
;; compute a bogus median and deviation of 9 elements - bogus because
;; it is really a median of 3 sub-medians.  The deviation is then
;; the bogo-median of the absolute deviation from this median
#+NIL
(defun bogo-median-and-sigma-9 (x1 x2 x3 x4 x5 x6 x7 x8 x9)
  (declare (type single-float x1 x2 x3 x4 x5 x6 x7 x8 x9)
	   (optimize (speed 3) (safety 0)))
  (let* ((bogomedian
	   (median-3
	    (median-3 x1 x2 x3)
	    (median-3 x4 x5 x6)
	    (median-3 x7 x8 x9)))
	 (bogosigma
	   (median-3
	    (median-3 (abs (- x1 bogomedian))
		      (abs (- x2 bogomedian))
		      (abs (- x3 bogomedian)))
	    (median-3 (abs (- x4 bogomedian))
		      (abs (- x5 bogomedian))
		      (abs (- x6 bogomedian)))
	    (median-3 (abs (- x7 bogomedian))
		      (abs (- x8 bogomedian))
		      (abs (- x9 bogomedian))))))
    (values bogomedian
	    ;; correction factor to make bogosigma equal to Gaussian
	    ;; sigma when applied to Gaussian random x1..x9
	    (* #.(/ 1.0 0.5899968) bogosigma))))





    
    
    
(defun make-cosmic-ray-bitmap
  (im
   &key
     (sigma-cosmic 7.0)
     (grow-cosmics t)
     (sigma-cosmic-grow 3.0) ;; grow existing cosmics using this sigma
     (expand-cosmics-by-one t) ;; grow the cosmic by 1 at each spot
     (min-flux  100.0)
     (saturation-value 50000.0)
     (input-badpix-bitmap nil)
     (modify-input-image nil) ;; otherwise the bitflag contains the only result
     (bad-pix-value float-utils:*single-float-nan*)) ;; replace pixels with this value
  "Find cosmic rays in an image by comparing points to a 3x3 median.
This routine excludes points lying on a real source. 

The image is filtered using a fast 3x3 median, and points that rise
more than SIGMA-COSMIC above the median are flagged, as long as they have
more than MIN-FLUX above the 3x3 median.

A real astronomical source is identified by having a region of zeros in 
IMAGE - 3x3-FILTERED-IMAGE, which indicate a smoot slope.

If GROW-COSMICS is true (default), then any cosmic rays detected in
the first pass are grown outward.  Any pixel that exceeds
SIGMA-COSMIC-GROW is flagged if it is adjacent to another cosmic pixel.
This allows detecting the weaker edges of cosmic rays, and catches
more of extended cosmic rays streaks.

EXPAND-BY-ONE grows any cosmic ray by one in each direction, to ensure
it gets all the cosmic ray pixels.

Pixels greater than SATURATION-VALUE or with NaN values are ignored.

If MODIFY-INPUT-IMAGE is true, then BAD-PIX-VALUE is put where a
cosmic ray is found.  

Returns 
  (VALUES COSMIC-BITMAP NUMBER-OF-COMSIC-PIXELS)
where BITMAP is set to 1 where there is a cosmic.

This routine ignores edge rows and columns of image, and ignores 3x3
regions with a NaN or InF anywhere."
  
  (declare (type image im)
	   (type (or null bit-image) input-badpix-bitmap)
	   (type single-float sigma-cosmic sigma-cosmic-grow min-flux
		 saturation-value bad-pix-value)
	   (optimize debug))

  (when (and input-badpix-bitmap
	     (not (equalp (array-dimensions input-badpix-bitmap)
			  (array-dimensions im))))
    (error "INPUT-BADPIX-BITMAP does not have same dimensions as IM"))
  
  (let* ((imbit (make-same-size-bit-image im :initial-value 0))
	 ;; a bitmap to mark pixels with no adjacent NaNs
	 (imbitbad (make-same-size-bit-image im :initial-value 0))
	 ;; IMFILT will be IM - MEDIAN3x3(IM)
	 (imfilt (make-same-size-image im :initial-value 0.0))
	 (ncols (array-dimension im 1)) ;; column count
	 (nrows (array-dimension im 0)) ;; row count
	 (ncols-1 (1- ncols))
	 (nrows-1 (1- nrows))
	 (ncosmics 0)
	 (ngrow 0)    ;; number of cosmic pixels grown outward, for diagnostics
	 ;; image that represents the the coarse binned backd sigma
	 (nsbin 16) 
	 (im-sigma (make-image nsbin nsbin)))

    (declare (type bit-image imbit imbitbad)
	     (type image imfilt im-sigma)
	     (type fixnum ncols nrows ncols-1 nrows-1 ncosmics ngrow))


    (labels
         ;; find NaNs and InFs and things marked in
	 ;; INPUT-BADPIX-BITMAP, and mark the whole 3x3 square around
	 ;; them in imbit bad - these regions will be ignored
	((make-imbitbad (im imbitbad)
	   (declare (type image im)
		    (type bit-image imbitbad))
	   (loop with icolmax of-type fixnum = (1- ncols)
		 with irowmax of-type fixnum = (1- nrows)
		 for irow of-type fixnum below nrows
		 do
		    (loop
		      for icol of-type fixnum below ncols
		      for f of-type single-float = (aref im irow icol)
		      when (or (float-utils:single-float-nan-or-infinity-p f)
			       (> f saturation-value)
			       (and input-badpix-bitmap
				    (plusp (aref input-badpix-bitmap irow icol))))
			do
			   (loop for jx  of-type fixnum
				 from (max 0 (- icol 1)) to (min icolmax (+ icol 1))
				 do
				    (loop for jy  of-type fixnum
					  from (max 0 (- irow 1)) to (min irowmax (+ irow 1))
					  do (setf (aref imbitbad jy jx) 1))))))
	 ;; subtract a 3x3 median filtered image from im-in and place it
	 ;; into im-out
	 (filtersub-3x3 (im-in im-out)
	   (declare (type image im-in im-out))
	   (loop 
	     for irow of-type fixnum  below nrows
	     for irow+ of-type fixnum = (if (= irow nrows-1) (- irow 1) (+ irow 1))
	     for irow- of-type fixnum = (if (= irow 0)       (+ irow 1) (- irow 1))
	     do
		(loop
		  for icol of-type fixnum below ncols
		  for icol+ of-type fixnum = (if (= icol ncols-1) (- icol 1) (+ icol 1))
		  for icol- of-type fixnum = (if (= icol 0)       (+ icol 1) (- icol 1))


		  if (plusp (aref imbitbad irow icol)) ;; avoid this pix; do only clean non-NaN blocks
		    do (setf (aref im-out irow icol) float-utils:*single-float-nan*)
		  else
		    do (let* ((x1 (aref im-in irow- icol-))
			      (x2 (aref im-in irow- icol ))
			      (x3 (aref im-in irow- icol+))
			      ;;
			      (x4 (aref im-in irow  icol-))
			      (x5 (aref im-in irow  icol)) ;; the center pixel
			      (x6 (aref im-in irow  icol+))
			      ;;
			      (x7 (aref im-in irow+ icol-))
			      (x8 (aref im-in irow+ icol ))
			      (x9 (aref im-in irow+ icol+)))
			 ;;
			 (setf (aref im-out irow icol)
			       (- x5 (bogo-median-9 x1 x2 x3 x4 x5 x6 x7 x8 x9)))))))
	 ;;
	 ;; test if a bright spot is really a star by seeing if there is a cluster of zeros
	 ;; near it in the median filtered image, indicating a gentle slope
	 (is-star (imfilt icol irow)
	   (declare (type image imfilt)
		    (type fixnum icol irow))
	   (loop
	     with nzeros of-type (signed-byte 20) = 0
	     with npix   of-type (signed-byte 20) = 0
	     for jx of-type fixnum
	     from (max 0 (- icol 2)) to (min ncols-1 (+ icol 2))
	     do
		(loop
		  for jy of-type fixnum
		  from (max 0 (- irow 2)) to (min nrows-1 (+ irow 2))
		  for f of-type single-float = (aref imfilt jy jx)
		  when (not (float-utils:single-float-nan-or-infinity-p f))
		    do
		       (incf npix)
		       (when (zerop f) (incf nzeros)))
	     finally ;; if 35% of the points around are zeros then call it a star
		     (return (>= (* 1.0 nzeros) (* 0.35 npix)))))
	 ;;
	 ;; make a sigma image, which is a smaller nsbin x nxbin image with local image sigma,
	 ;; that we then nearest-neighbor interpolate
	 (make-sigma-image (imfilt imsigma)
	   (loop
	     with sigma-max of-type single-float = -1.0
	     with scratch-vec = (make-array 730 :element-type 'single-float)
	     with xstep of-type single-float = (/ ncols (* 1.0 nsbin))
	     for ixb below nsbin
	     for xx of-type single-float = (* 0.5 xstep) then (+ xx xstep) ;; index in imfilt
	     do (loop
		  with ystep of-type single-float = (/ nrows (* 1.0 nsbin))
		  for iyb below nsbin
		  for yy of-type single-float = (* 0.5 ystep) then (+ yy ystep)
		  do
		     (multiple-value-bind (median sigma npix)
			 (image-median-and-sigma-in-annulus 
			  imfilt xx yy 20.0 25.0
			  :scratch-vec scratch-vec
			  :no-median t
			  :ignore-nan t)
		       (declare (ignore median)
				(type single-float median sigma)
				(type fixnum npix))
		       (if (zerop npix) ;; error; bad region
			   (setf sigma -1.0)
			   (setf sigma-max (max sigma-max sigma)))
		       (setf (aref imsigma iyb ixb) sigma)))
	     finally ;; if we have any undefined sigmas, put sigma-max there
		     (loop for i below (array-total-size imsigma)
			   when (minusp (row-major-aref imsigma i))
			     do (setf (row-major-aref imsigma i) sigma-max))))
			     
	 ;;
	 ;; get the sigma
	 (get-sigma (icol irow)
	   (declare (type (unsigned-byte 28) irow icol))
	   (aref
	    im-sigma
	    (floor (the (single-float 0.0 1e9) (* nsbin (/ irow (* 1.0 nrows)))))
	    (floor (the (single-float 0.0 1e9) (* nsbin (/ icol (* 1.0 ncols)))))))
	 ;;
	 ;; create the bitmap with 1s at cosmic ray locations
	 (make-cosmic-ray-bitmap (imfilt imbit imbitbad) 
	   (declare (type image imfilt)
		    (type bit-image imbit imbitbad))
	    (loop for irow of-type fixnum below nrows
		  do
		     (loop
		       for icol of-type fixnum below ncols
		       for f of-type single-float = (aref imfilt irow icol)
		       ;; do only clean non-NaN blocks
		       when (and (zerop (aref imbitbad irow icol)) 
				 (> f min-flux))
			 do
			    (let ((sigma (get-sigma icol irow)))
			      (when (> (aref imfilt irow icol)
				       (* sigma sigma-cosmic))
				(when (not (is-star imfilt icol irow))
				  (when (not (plusp (aref imbit irow icol)))
				    (incf ncosmics)
				    (setf (aref imbit irow icol) 1))))))))
	 ;;
	 ;; Expand a cosmic ray using SIGMA-COSMIC-GROW based
	 ;; threshold in a hollow square of delta NS (NS=1 is 3x3
	 ;; square, 2 is 5x5, etc).  Called by grow-cosmic-rays
	 (expand-cosmic-in-square (icol irow ns imbit imbit-bad imfilt imfilt-threshold)
	   (declare (type (unsigned-byte 28) irow icol)
		    (type (unsigned-byte 8) ns)
		    (type bit-image imbit imbit-bad)
		    (type single-float imfilt-threshold))
	   (let ((did-expansion nil))
	     (flet ((set-point (jcol jrow)
		      (when ;; don't bother doing pixels at extreme edges, because too much
			  ;; bounds checking for too little benefit
			  (and (> jrow 0) (< jrow nrows-1) (> jcol 0) (< jcol ncols-1))
			(when (and
			       ;; not already flagged
			       (zerop (aref imbit jrow jcol))
			       ;; not a NaN at this spot
			       (zerop (aref imbit-bad jrow jcol))
			       ;; is bright at this spot
			       (>= (aref imfilt jrow jcol) imfilt-threshold)
			       ;; at least one neighboring pixel is flagged, for continuity
			       (or (plusp (aref imbit (+ -1 jrow) (+ +1 jcol)))
				   (plusp (aref imbit (+  0 jrow) (+ +1 jcol)))
				   (plusp (aref imbit (+ +1 jrow) (+ +1 jcol)))
				   ;;
				   (plusp (aref imbit (+ +1 jrow) (+  0 jcol)))
				   (plusp (aref imbit (+ -1 jrow) (+  0 jcol)))				 
				   ;;
				   (plusp (aref imbit (+ -1 jrow) (+ -1 jcol)))
				   (plusp (aref imbit (+  0 jrow) (+ -1 jcol)))
				   (plusp (aref imbit (+ +1 jrow) (+ -1 jcol)))))
			  (incf ngrow)
			  (when (not (plusp (aref imbit jrow jcol)))
			    (incf ncosmics)
			    (setf (aref imbit jrow jcol) 1))
			  (setf did-expansion t) ))))
	       ;;
	       (declare (inline set-point))
	       (loop for k from (- ns) to ns
		     do
			(set-point (+ icol k) (+ irow ns))  ;; top part of box
			(set-point (+ icol k) (- irow ns))) ;; bottom part of box
	       ;; corners already done, so smaller range for left/right sides
	       (loop for k from (1+ (- ns)) to (1- ns)  
		     do
			(set-point (+ icol ns) (+ irow k))   ;; left part of box
			(set-point (- icol ns) (- irow k)))) ;; right part of box
	     did-expansion)) ;; return T if we did any expanding
	 ;;
	 ;; For every set cosmic pixel in IMBIT, look in increasing squares
	 ;; around it of size 1...NGROW, setting pixels that exceed the
	 ;; secondary thresold of SIGMA-COSMIC-GROW
	 (grow-cosmic-rays (imfilt imbit imbitbad ngrow)
	   (declare (type image imfilt)
		    (type bit-image imbit imbitbad)
		    (type (unsigned-byte 8) ngrow))
	   (loop
	     ;; copy of imbit because we're modifying it
	     with imbit-orig of-type bit-image = (copy-bit-image imbit) 
	     for irow of-type fixnum below nrows
	     do
		(loop
		  for icol of-type fixnum below ncols
		  when (= 1 (aref imbit-orig irow icol)) ;; there's a cosmic here
		    do ;; loop in the neighborhood for new hot pix
		       (loop
			 with sigma of-type single-float =  (get-sigma icol irow) ;; close-enough sigma
			 with threshold of-type single-float = (* sigma sigma-cosmic-grow)
			 for nsquare of-type (unsigned-byte 8) from 1 to ngrow
			 do
			    (when (not ;; stop expanding at this location when no new pixels flagged
				   (expand-cosmic-in-square icol irow nsquare imbit imbitbad
							    imfilt threshold))
			      (return))))))
		
	 ;; expand the cosmic by one around every triggered pixel,
	 ;; regardless of whether the expansion region exceeds any
	 ;; threshold
	 (expand-by-one (imbit)
	   (declare (type image imfilt)
		    (type bit-image imbit imbitbad))
	   (loop
	     with ngrow = 1 ;; 1 in each direction
	     with imbit-orig of-type bit-image = (copy-bit-image imbit) ;; copy because we're modifying imbit
		 for irow of-type fixnum below nrows
		  do
		     (loop
		       for icol of-type fixnum below ncols
		       when (= 1 (aref imbit-orig irow icol)) ;; there's a cosmic here
			 do 
			    (loop
			      for jx of-type fixnum
			      from (max 0 (- icol ngrow)) to (min ncols-1 (+ icol ngrow))
			      do
				 (loop
				   for jy of-type fixnum
				   from (max 0 (- irow ngrow)) to (min nrows-1 (+ irow ngrow))
				   when (zerop (aref imbitbad irow icol)) ;; ignore NaN
				     do
					(when (not (plusp (aref imbit jy jx)))
					  (incf ncosmics)
					  (setf (aref imbit jy jx) 1)))))))
	 ;;
	 ;; set the cosmics to BAD-PIX-VALUE in the input image
	 (set-cosmics-in-input-image (im imbit)
	   (declare (type image im)
		    (type bit-image imbit))
	   (loop for i below (array-total-size im)
		 when (= (row-major-aref imbit i))
		   do (setf (row-major-aref im i) bad-pix-value)))
      )   ;; end LABELS

      (declare (inline get-sigma))
		   

      ;; fill imbitbad, the image that marks pixels with NaN neighbors
      (make-imbitbad im imbitbad)
      ;; do 3x3 filtration
      (filtersub-3x3 im imfilt)
      ;; make the sigma map
      (make-sigma-image imfilt im-sigma)
      ;; make the final bitmap
      (make-cosmic-ray-bitmap imfilt imbit imbitbad)
      ;; grow the cosmic rays to pixels of lower significance
      (when grow-cosmics (grow-cosmic-rays imfilt imbit imbitbad 7))
      (when expand-cosmics-by-one
	(expand-by-one imbit))
      ;; set flagged pixels in input image to BAD-PIX-VALUE if requested	   
      (when modify-input-image (set-cosmics-in-input-image im imbit))
      (values imbit ncosmics))))
		   
