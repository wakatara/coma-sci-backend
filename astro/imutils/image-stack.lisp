#|

Image stacking - main routine is IMAGE-STACK

This works entirely in memory.  It can median stack 10 1k.1k images in 1.5 seconds.
One would expect it to be inefficient because of poor cache hits, but it is good enough.

|#

(in-package imutils)

(declaim (inline %vecmean))

(defun %vecmean (v k)
   (/ 
    (loop for kk below k sum (aref v kk) of-type single-float)
    k))

(defun image-stack (image-list
		    &key
		      (stack-type :median)
		      (ignore-val most-positive-single-float)
		      (null-val 0.0)
		      (clipping-function nil)
		      (img-out nil))
  "Create an output image by :MEDIAN or :MEAN stacking input imags.

Pixels with value IGNORE-VAL (by default most-positive-single-float)
are ignored.  If no valid values found, NULL-VAL is substituted.

CLIPPING-FUNCTION function is called as (CLIPPING-FUNCTION VEC SCRATCH-VEC KVALID)
 on the current vector of KVALID pixels being stacked, and returns
 N, the number of pixels left in the start of VEC after clipping is done.

IMG-OUT is target image, if specified.
"
  (declare (type list image-list)
	   (type single-float ignore-val null-val)
	   (type (or null image) img-out)
	   (type (or null function symbol) clipping-function)
	   (type (member :mean :median) stack-type))


  (when (and img-out (not (equalp (type-of img-out)
				  (type-of (first image-list)))))
    (error "type-of IMG-OUT = ~A is not equal to ~A" 
	   (type-of img-out)
	   (type-of (first image-list))))
  ;;
  (loop 
     with dims = (array-dimensions (first image-list))
     for im in image-list
     for k from 1
     when (not (and (typep im `(simple-array single-float ,dims))))
     do (error "Image ~A in list is not a ~A" k `(simple-array single-float ,dims)))


  ;;
  (let ((npix (array-total-size (first image-list)))
	(clip-func (cond ((not clipping-function) nil)
			 ((functionp clipping-function) clipping-function)
			 ((symbolp clipping-function) (symbol-function clipping-function))))
	(out-im (or img-out
		    (make-array (array-dimensions (first image-list))
				:element-type 'single-float)))
	(image-vec (map 'vector #'identity image-list))
	(nimages  (length image-list)))
    (locally (declare (optimize (speed 3))
		      (type image out-im)
		      (type (or null function) clip-func)
		      ;; this is necessary, and greatly decreases consing.  One would
		      ;; think not, because function is maybe-inline
		      (inline  fastmedian:fast-single-float-1d-array-median))
      (loop 
	with v of-type (simple-array single-float (*))
	  = (make-array nimages :element-type 'single-float)
	with vscr of-type (or null (simple-array single-float (*))) ;; scratch vec for clipping
	  = (when clip-func
	      (make-array nimages :element-type 'single-float))
	for i below npix
	do
	   (loop
	      with k of-type imindex = 0
	      for im of-type image across image-vec
	      for pix of-type single-float = (row-major-aref im i)
	      when (not (= pix ignore-val))
	      do
		(setf (aref v k) pix)
		(incf k)
	      finally
		(if (zerop k)
		    ;; no valid images
		    (setf (row-major-aref out-im i) null-val)
		    ;; else stack using appropriate method
		    (progn
		      (when clip-func
			(setf k (funcall clip-func v vscr k)))
		      (setf (row-major-aref out-im i) 
			    (if (eq stack-type :median)
				(fastmedian:fast-single-float-1d-array-median v k)
				(%vecmean v k))))))))
    
    out-im))
		
       
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun image-stack-with-function  (image-list combine-function
				   &key 
				   (ignore-val most-positive-single-float)
				   (null-val 0.0)
				   (img-out nil))
  "Create an output image by using a function to combine pixels as

   (FUNCALL COMBINE-FUNCTION IMAGE-OUT IY IX PIXEL-VEC NPIX)

The function is to combine the NPIX vectors in single-float PIXEL-VEC
and place the result into (AREF IMAGE-OUT IY IX).   Pixel with value
IGNORE-VAL are ignored when buildling PIXEL-VEL, and NULL-VAL is inserted
when there are no good pixels."
  (declare (type list image-list)
	   (type single-float ignore-val null-val)
	   (type (or null image) img-out)
	   (type (or symbol (function (image imindex imindex floatvec imindex)))
		 combine-function))
  

   (when (and img-out (not (equalp (type-of img-out)
				   (type-of (first image-list)))))
     (error "type-of IMG-OUT = ~A is not equal to ~A" 
	    (type-of img-out)
	    (type-of (first image-list))))
   ;;
   (loop 
      with dims = (array-dimensions (first image-list))
      for im in image-list
      for k from 1
      when (not (and (typep im `(simple-array single-float ,dims))))
      do (error "Image ~A in list is not a ~A" k `(simple-array single-float ,dims)))

   (let ((out-im (or img-out
		     (make-array (array-dimensions (first image-list))
				 :element-type 'single-float)))
	 (nimages  (length image-list))
	 (image-vec (map 'vector #'identity image-list))
	 (func (if (functionp combine-function)
		   combine-function
		   (symbol-function combine-function))))
    (locally (declare (optimize (speed 3))
		      (type image out-im))

      (loop 
	 with v of-type (simple-array single-float (*)) = 
	   (make-array nimages :element-type 'single-float)
	 with nx = (array-dimension out-im 1)
	 with ny = (array-dimension out-im 0)
	 for iy below ny
	 do
	   (loop 
	      for ix below nx  
	      do
		(loop 
		   with k of-type imindex = 0
		   for im of-type image across image-vec
		   for pix of-type single-float = (aref im iy ix)
		   when (not (= pix ignore-val))
		   do
		     (setf (aref v k) pix)
		     (incf k)
		   finally
		(if (zerop k)
		    ;; no valid images
		    (setf (aref out-im iy ix) null-val)
		    ;; else let the function stack it - it is the
		    ;; function's job to insert the value into the array
		    (funcall func out-im iy ix v k))))))))
		    
		

	       
	       

   
