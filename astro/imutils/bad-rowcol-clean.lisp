#|

Try to get rid of outlier rows and columns using

|#

(in-package imutils)


;; for DIRECTION=:ROW or :COLUMN, compute the median of the row/col from i0 to i1, applying ABS
;; if requested
(defun bad-rowcol-compute-median-value
    (imfilt k direction &key (abs t) (i0 nil) (i1 nil)
			  (scratch-vec nil) (nskip 1))
  (declare (type image imfilt)
	   (type (or null fixnum) i0 i1 k)
	   (type (integer 1 100) nskip)
	   (type (member :row :column) direction)
	   (type (or null (simple-array single-float (*))) scratch-vec)
	   (optimize speed))
  (let* ((nx (1- (array-dimension imfilt 1))) ;; max indices
	 (ny (1- (array-dimension imfilt 0)))
	 (j0 (or i0 0))
	 (j1 (or i1 (if (eq direction :row) nx ny)))
	 (%scratch-vec (or scratch-vec (make-array (1+ (- j1 j0)) :element-type 'single-float)))
	 (nout 0)
	 (nan float-utils:*single-float-nan*))
    (declare (type (signed-byte 28) nx ny j0 j1 nout)
	     (type  (simple-array single-float (*)) %scratch-vec))
    (loop
      for j from j0 to j1 by nskip
      for n of-type fixnum from 0
      for z of-type single-float =  (if (eq direction :row)
					(aref imfilt k j)
					(aref imfilt j k))
      do (when (not (float-utils:single-float-nan-or-infinity-p z))
	   (incf nout) ;; increment number of valid output values
	   (setf (aref %scratch-vec n) (if abs (abs z) z))))
    ;;
    (if (plusp nout)
	(fastmedian:fast-single-float-1d-array-median %scratch-vec nout)
	nan)))


;; compute an entire vector for one direction.  If :DIRECTION is :ROW, then the output vector
;; is of length NX=NCOLS and each value of the output vector is the median of IMFILT from
;; ROW indices I0 to i1 (by default whole thing)
(defun bad-rowcol-compute-median-vector (imfilt direction &key i0 i1 outvec scratch-vec (abs t) (nskip 10))
  (declare (type image imfilt)
	   (type (or null fixnum) i0 i1)
	   (type (member :row :column) direction)
	   (type (or null (simple-array single-float (*))) outvec scratch-vec)
	   (optimize speed))
  
  (let* ((nx (1- (array-dimension imfilt 1))) ;; max indices
	 (ny (1- (array-dimension imfilt 0)))
	 (j0 (or i0 0))
	 (j1 (or i1 (if (eq direction :row) nx ny)))
	 (%scratch-vec (or scratch-vec (make-array (1+ (- j1 j0)) :element-type 'single-float)))
	 ;; outvec has length NY=NROWS if direction is :COLUMN, etc
	 (nout  (if (eq direction :column) nx ny))
	 (%outvec (or outvec (make-array nout :element-type 'single-float))))
    (declare (type (signed-byte 28) nx ny j0 j1 nout)
	     (type  (simple-array single-float (*)) %scratch-vec))
    ;;
    (loop for k from 0 to nout
	  do (setf (aref %outvec k)
		   (bad-rowcol-compute-median-value
		    imfilt k direction :abs abs :i0 i0 :i1 i1
				       :nskip nskip
				       :scratch-vec %scratch-vec)))
    ;;
    %outvec))


(defun bad-rowcol-filter-image
    (im &key
	  ;;(ncolblocks 1) (nrowblocks 1) ;; maybe do it by blocks in future?
	  (modify-input-image nil) ;; otherwise the bitflag contains the only result
	  ;; replace bad pix with this, if MODIFY-INPUT-IMAGE
	  (bad-pix-value float-utils:*single-float-nan*) 
	  (input-bitflag-image nil) ;; optional starting bitflag, so we can add flags
	  (quartile-reject 8.0)
	  (abs t)
	  (rows t) (columns t) ;; do both directions by default
	  (nskip 3) ;; speed it up a little by looking at every NSKIP pixels
	  (filter-function (lambda (im) (apply-3x3-laplace-filter im :kernel-type 8)))
	  (saturation-value 55000.0)
	  (mark-saturated nil) ;; mark saturated pixels too
	  (invert-bitmap nil) ;; if T, then 1 means good pix , 0 means bad pix
	  ;; options to run FIND-COSMIC-RAYS-IN-IMAGE before finding bad rows,columns
	  (filter-cosmic-rays nil)
	  (sigma-cosmic 7.0)
	  (grow-cosmics t)
	  (sigma-cosmic-grow 3.0)
	  (expand-cosmics-by-one t)
	  (min-cosmic-flux 100.0))
	  
  
  "Compute where bad rows and columns are.  If MODIFY-INPUT-IMAGE is
true (default) then destructively modify image IM by replacing
anomalous rows or columns with BAD-PIX-VALUE (NaN by default). 
Otherwise, the returned bitmap image will contain 1 where the
image is marked as bad.

The following procedure is used

1) apply filter-function to image.  This can be IDENTITY or
   APPLY-3x3-LAPLACE-FILTER or similar.  The Laplace filter
   seems effective.

2) median collapse each filtered row (if ROWS is T) and/or
   column (if COLUMNS is T) into a vector.

3) wherever the collapsed value is QUARTILE-REJECT times the quartile
   deviation from the center of this vector, replace that row or colum
   with BAD-PIX-VALUE.

Returns (VALUES BITFLAG-IMAGE NBADROWS NBADCOLS NBPADPIX)

INPUT-BITFLAG-IMAGE is an optional bitflag that may start with
non-zero bits.  This is destructively modified if given.  Otherwise
a new bitflag is created.

NSKIP is the number of pixels to skip when collapsing, for the sake of
speed.  ABS is whether to apply the absolute value to the filtered
function.

If MARK-SATURATED is true, then pixels with a flux greater than
SATURATION-VALUE are also marked.  If SATURATION-VALUE is :AUTO
then a saturation is estimated as 0.95 times the maximum flux.

If FILTER-COSMIC-RAYS is T, then the options SIGMA-COSMIC,
SIGMA-COSMIC-GROW, MIN-COSMIC-FLUX are used to additionally flag cosmic rays."
  (declare (type image im)
	   (type single-float quartile-reject)
	   (type (integer 1 100) nskip))

  (when (and input-bitflag-image
	     (not (and (eq (array-element-type input-bitflag-image) 'bit)
		       (equalp (array-dimensions input-bitflag-image)
			       (array-dimensions im)))))
    (error "An invalid INPUT-BITFLAG-IMAGE was given"))
  
  (let* ((imfilt (funcall filter-function im))
	 (imbit (or input-bitflag-image ;; to mark bad locations with 1
		    (make-same-size-bit-image im :initial-value 0)))
	 (ncols (array-dimension imfilt 1)) ;; column count
	 (nrows (array-dimension imfilt 0)) ;; row count
	 (scratch-vec (make-array (max ncols nrows) :element-type 'single-float))
	 (outvec-col (make-array ncols :element-type 'single-float))
	 (outvec-row (make-array nrows :element-type 'single-float))
	 (true-saturation-value 0.0)
	 ;; final counter for number of bad pix, rows, colsq
	 (nbadpix 0) (nbadrows 0) (nbadcols 0)) 

    (declare (type image imfilt)
	     (type bit-image imbit)
	     (type single-float true-saturation-value)
	     (type fixnum ncols nrows nbadpix nbadrows nbadcols))

    ;; handle saturation value, particularly :AUTO
    (setf true-saturation-value 
	  (if (eq saturation-value :auto)
	      (loop with fmax of-type single-float = most-negative-single-float
		    for i of-type fixnum below (array-total-size im)
		    for f of-type single-float = (row-major-aref im i)
		    when (not (float-utils:single-float-nan-or-infinity-p f))
		      do (setf fmax (max f fmax))
		    finally
		       (return (if (plusp fmax)
				   (* 0.95 fmax)
				   most-positive-single-float)))
	      ;; else saturation-value was a number
	      (float saturation-value 1.0)))
	      
	      
    

   

    (flet ((mark-bad-col (icol &key imin imax)
	     (declare (type fixnum icol)
		      (type (or null fixnum) imin imax))
	     (incf nbadcols)
	     (loop for i of-type fixnum from (or imin 0) to (or imax (1- nrows))
		   when (zerop (aref imbit i icol))
		     do (incf nbadpix)
			(setf (aref imbit i icol) 1)))
	   (mark-bad-row (irow &key imin imax)
	     (declare (type fixnum irow)
		      (type (or null fixnum) imin imax))
	     (incf nbadrows)
	     (loop for i of-type fixnum from (or imin 0) to (or imax (1- ncols))
		   when (zerop (aref imbit irow i))
		     do (incf nbadpix)
			(setf (aref imbit irow i) 1))))

      ;; mark bad rows
      (when rows
	(let ((outvec outvec-row)) ;; collapse direction = :ROW
	  (bad-rowcol-compute-median-vector imfilt :row :outvec outvec
							:scratch-vec scratch-vec :abs abs :nskip nskip)
	  (let* ((fvec (coerce
			(loop for x across outvec
			      for i from 0
			      if (not (float-utils:single-float-nan-or-infinity-p x))
				collect x)
			'(simple-array single-float (*)))))
	    (if (plusp (length fvec)) ;; some valid elements
		(let* ((q1 (fastmedian:fast-single-float-1d-array-fraction fvec 0.25))
		       (q2 (fastmedian:fast-single-float-1d-array-fraction fvec 0.50))
		       (q3 (fastmedian:fast-single-float-1d-array-fraction fvec 0.75))
		       (q  (* 0.5 (- q3 q1))))
		  ;; note bad columns in imbit
		  (loop for i below nrows
			for x across outvec
			when (or (float-utils:single-float-nan-or-infinity-p x)
				 (> (abs (- x q2))
				    (* quartile-reject q)))
			  do
			     ;;(format t "Row ~A  D=~,3F REJ=~,3F~%" i (abs (- x q2)) (* quartile-reject q))
			     (mark-bad-row i)))
		;; other IF clause - no good points in F
		(loop for i below ncols do (mark-bad-row i))))))

      ;; mark bad columns
      (when columns
	(let ((outvec outvec-col)) ;; collpase direction = :COLUMN
	  (bad-rowcol-compute-median-vector imfilt :column :outvec outvec
							   :scratch-vec scratch-vec :abs abs :nskip nskip)
	  (let* ((fvec (coerce
			(loop for x across outvec
			      for i from 0
			      if (not (float-utils:single-float-nan-or-infinity-p x))
				collect x)
			'(simple-array single-float (*)))))
	    (if (plusp (length fvec)) ;; some valid elements
		(let* ((q1 (fastmedian:fast-single-float-1d-array-fraction fvec 0.25))
		       (q2 (fastmedian:fast-single-float-1d-array-fraction fvec 0.50))
		       (q3 (fastmedian:fast-single-float-1d-array-fraction fvec 0.75))
		       (q  (* 0.5 (- q3 q1))))
		  ;; note bad columns in imbit
		  (loop for i below ncols
			for x across outvec
			when (or (float-utils:single-float-nan-or-infinity-p x)
				 (> (abs (- x q2))
				    (* quartile-reject q)))
			  do
			     ;;(format t "Col ~A  D=~,3F REJ=~,3F~%" i (abs (- x q2)) (* quartile-reject q))
			     (mark-bad-col i)))
		;; other IF clause - no good points in fvec
		(loop for i below nrows do (mark-bad-col i))))))      


      ;; mark saturated pixels
      (when mark-saturated
	(loop for i of-type fixnum below (array-total-size im)
	      for f of-type single-float = (row-major-aref im i)
	      when (and (not (float-utils:single-float-nan-or-infinity-p f))
			(>= f true-saturation-value))
		do (setf (row-major-aref imbit i) 1)))
      
      ;; When filtering cosmic rays, create a cosmic bitmap and transfer
      ;; any 1 values into imbit.
      (when filter-cosmic-rays
	(let ((imbit-cosmic (make-cosmic-ray-bitmap
			     im 
			     :sigma-cosmic sigma-cosmic
			     :grow-cosmics grow-cosmics
			     :sigma-cosmic-grow sigma-cosmic-grow
			     :expand-cosmics-by-one expand-cosmics-by-one
			     :min-flux min-cosmic-flux
			     :saturation-value true-saturation-value
			     :input-badpix-bitmap imbit
			     :modify-input-image nil)))
	  (loop for i below (array-total-size imbit)
		when (= (row-major-aref imbit-cosmic i) 1)
		  do (setf (row-major-aref imbit i) 1))))    
	
	
      ;; finally, destructively mark the original image using imbit
      (when modify-input-image
	(loop for i of-type fixnum below (array-total-size im)
	      when (= (row-major-aref imbit i) 1)
		do (setf (row-major-aref im i) bad-pix-value)))
      ;;
      (when invert-bitmap
	(loop for i of-type fixnum below (array-total-size im)
	      do (setf (row-major-aref imbit i)
		       (logxor (row-major-aref imbit i) 1))))
		       
      (values imbit nbadrows nbadcols nbadpix))))
						    
    
  
  

	



		       
	      
		 
		 
