

(in-package imutils)


(defun %default-image-clean-function (x)
  (declare (type single-float x)
	   (optimize speed))
  (or 
   (float-utils:single-float-nan-p x)
   (< x #.(* 0.1 most-negative-single-float))
   (> x #.(* 0.1 most-positive-single-float))))

(defun imclean-avg (im &key
			 ix0 ix1 iy0 iy1
			 (max-iter 200)
			 (clean-function #'%default-image-clean-function))
  "Destructively cleans up an image IM from pixels IX0-IX1,IY0-IY1 by
substituting average values of valid pixels around each invalid pixel
in an iterative manner, until all bad values are exhausted.
   
    CLEAN-FUNCTION is a function that returns T if a pixel is bad; by
        default it flags NaN and numbers within a factor of 10 of infinity.

    MAX-ITER is the maximum number of iterations before failure"
             
  (declare (type image im)
	   ;; signed byte because we truncate at 0 if needed
	   (type (or null (signed-byte 32)) ix0 ix1 iy0 iy1)
	   (type (function (single-float) (values boolean))
		 clean-function)
	   (type (integer 1 1000) max-iter)
	   (optimize speed))

  (let*
      ((nx (array-dimension im 1))
       (ny (array-dimension im 0))
       ;; ensure that ix0,iy0,ix1,iy1 are in the array, but one bigger than
       ;; given values
       (ix0 (max 0 (1- (or ix0 0))))
       (iy0 (max 0 (1- (or iy0 0))))
       (ix1 (min (1- nx) (1+ (or ix1 (1- nx)))))
       (iy1 (min (1- ny) (1+ (or iy1 (1- ny)))))
       (nbad 0)
       ;; define an array of bad pixels - we're lazy and make it the size
       ;; of the whole array, because it's just a bit array
       (badim  (make-array (array-dimensions im) :element-type 'bit
						 :initial-element 0))
       ;; Create a list of bad pixels as (iy ix), and populate badim
       ;; array.  Note that we have to make the badim array one pixel
       ;; bigger than the badpix list to avoid picking up bad pix from
       ;; the boundaries when filtering.
       (badlist (loop
		  with bl = nil
		  for iy of-type fixnum from iy0 to iy1 
		  do
		     (loop
		       for ix of-type fixnum from ix0 to ix1 
		       when (funcall clean-function (aref im iy ix))
			 do ;; ix0,ix1,iy0,iy1 are too big by 1 
			    (when (and (< ix0 ix ix1) (< iy0 iy iy1))
			      (push (list iy ix) bl)
			      (incf nbad))
			    ;; but the badim has to be oversized by 1
			    (setf (aref badim iy ix) 1))
		  finally (return bl))))

    (declare (type fixnum nx ny nbad)
	     (type list badlist))

    
    (labels ((clean-one-pass ()
	       (setf badlist
		     (loop for iyx of-type list in badlist
			   when (not (maybe-fix-pixel (first iyx) (second iyx)))
			     collect iyx)))
	     ;;
	     (maybe-fix-pixel (iy ix)
	       (declare (type fixnum iy ix))
	       ;; loop around neighboring pixels to find the good ones
	       (loop
		 with n-fixers of-type (integer 0 8) = 0 ;; no. of fixer pixels
		 with fix-sum of-type single-float = 0.0
		 for jy from -1 to 1
		 for my of-type fixnum = (+ iy jy)
		 do
		    (loop
		      for jx from -1 to 1
		      for mx of-type fixnum = (+ ix jx)
		      when (and
			    (not (and (zerop jx) (zerop jy))) ;; not this pix
			    ;; pixels is in bounds
			    (< -1 mx nx)  (< -1 my ny)
			    ;; this neighbor pix is not bad
			    (not (= 1 (aref badim my mx))))
			do
			   (incf n-fixers 1)
			   (incf fix-sum (aref im my mx)))
		 finally ;; if we have a good repair
			 (return
			   (when (plusp n-fixers)
			     (setf (aref badim iy ix) 0)
			     (setf (aref im iy ix)
				   (/ fix-sum n-fixers))
			     t)))))
      
      
      (loop for k below max-iter
	    do (clean-one-pass)
	    when (not badlist)  ;; used up all the bad pix
	      do
		 (return nbad) 
	    finally
	       ;; 
	       (error
		"Too many iterations (> ~A) in IMCLEAN-AVG trying to clean ~A bad pixels"
		max-iter nbad)))))
