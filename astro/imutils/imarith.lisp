

(in-package imutils)

#|

Image arithmetic.


General format for one operator functions is
 (IM-SCALE IMAGE-IN X) ;; Create new output image
 (IM-SCALE IMAGE-IN X :IMAGE-OUT IMAGE-C)  ;; put result in image C


General format for two-operator functions is
 (IM+ IMAGE-A IMAGE-B)  ;; Create new output image
 (IM+ IMAGE-A IMAGE-B :IMAGE-OUT IMAGE-C)  ;; put result in image C




|#






(defun copy-image (a)
  "Make a copy of a single float array - just a front end for copy-array-to-single-float-array"
  (declare (type image a))
  (copy-array-to-single-float-array a))

(defun copy-flag-image (a)
  (declare (type flag-image a))
  (loop 
     with aa = (make-flag-image (array-dimension a 0) (array-dimension a 1))
     for i below (array-total-size a)
     do (setf (row-major-aref aa i) (row-major-aref a i))
     finally (return aa)))

(defun copy-bit-image (a)
  (declare (type bit-image a))
  (loop 
     with aa = (make-bit-image (array-dimension a 0) (array-dimension a 1))
     for i below (array-total-size a)
     do (setf (row-major-aref aa i) (row-major-aref a i))
     finally (return aa)))

(defun copy-complex-image (a)
  (declare (type complex-image a))
  (loop 
     with aa = (make-complex-image (array-dimension a 0) (array-dimension a 1))
     for i below (array-total-size a)
     do (setf (row-major-aref aa i) (row-major-aref a i))
     finally (return aa)))

(defun copy-double-image (a)
  (declare (type double-image a))
  (loop 
     with aa = (make-double-image (array-dimension a 0) (array-dimension a 1))
     for i below (array-total-size a)
     do (setf (row-major-aref aa i) (row-major-aref a i))
     finally (return aa)))

(defun copy-double-complex-image (a)
  (declare (type double-complex-image a))
  (loop 
     with aa = (make-double-complex-image (array-dimension a 0) (array-dimension a 1))
     for i below (array-total-size a)
     do (setf (row-major-aref aa i) (row-major-aref a i))
     finally (return aa)))

(defun copy-*-image (a)
  (cond ((typep a 'image) (copy-image a))
	((typep a 'bit-image) (copy-bit-image a))
	((typep a 'flag-image) (copy-flag-image a))
	((typep a 'double-image) (copy-double-image a))
	((typep a 'complex-image) (copy-complex-image a))
	((typep a 'double-complex-image) (copy-double-complex-image a))
	(t
	 (error "Type not allowed: ~A" (type-of a)))))


(defun copy-array-to-single-float-array (a)
  "convert an array to its single float equivalent, always copying"
  (declare (type (simple-array * (* *)) a))
  ;;
  (let ((aout (make-array (array-dimensions a) :element-type 'single-float))
	(n (array-total-size a)))
    (declare (type image aout))

    (macrolet 
	((copy-for-type (type &key (optimize '(speed)))
	   `(locally (declare (optimize ,@optimize))
	     (loop 
	      with aa of-type ,type = a
	      for i of-type (unsigned-byte 27) below n
	      do (setf (row-major-aref aout i)
		       (float (row-major-aref aa i) 1.0))))))
    ;;
      (typecase a
	;;
	(image
	 (copy-for-type image))
	;;
	((simple-array double-float (* *))
	 (copy-for-type (simple-array double-float (* *))))
	;;
	((simple-array fixnum (* *))
	 (copy-for-type (simple-array fixnum  (* *))))
	;;
	((simple-array (unsigned-byte 64) (* *))
	 (copy-for-type (simple-array (unsigned-byte 64)  (* *))))
	((simple-array (signed-byte 64) (* *))
	 (copy-for-type (simple-array (signed-byte 64)  (* *))))
	;;
	((simple-array (unsigned-byte 32) (* *))
	 (copy-for-type (simple-array (unsigned-byte 32)  (* *))))
	((simple-array (signed-byte 32) (* *))
	 (copy-for-type (simple-array (signed-byte 32)  (* *))))
	;;
	((simple-array (unsigned-byte 16) (* *))
	 (copy-for-type (simple-array (unsigned-byte 16)  (* *))))
	((simple-array (signed-byte 16) (* *))
	 (copy-for-type (simple-array (signed-byte 16)  (* *))))
	;;
	((simple-array (unsigned-byte 8) (* *))
	 (copy-for-type (simple-array (unsigned-byte 8)  (* *))))
	((simple-array (signed-byte 8) (* *))
	 (copy-for-type (simple-array (signed-byte 8)  (* *))))
	;;
	(otherwise
	 (copy-for-type (simple-array T  (* *))
			:optimize (safety))))
      ;;
      aout)))


;; utility function to throw an error if image has wrong
;; dimensions (not NYxNX); returns IMAGE if OK
(defun %check-dims-n (image nx ny &optional where)
  (declare (type (or image flag-image bit-image) image)
	   (optimize speed))
  (if (not (and (= (array-dimension image 1) nx)
		(= (array-dimension image 0) ny)))
      (error "Array has wrong dimensions~A: ~A and not (~A,~A)"
	     (if where (format nil " [~A]" where) "")
	     (array-dimensions image) ny nx)
      ;; return IMAGE if OK
      image))
  
		
	   

; utility function to ensure that dimensions of 3 images are the same
(defun %check-dims2 (image-a image-out &optional where)
  (declare (type (or image flag-image bit-image) image-a image-out)
	   (optimize speed))
  (when 
      (or (not (and (= (array-dimension image-a 0)
		       (array-dimension image-out 0))))
	  (not (and (= (array-dimension image-a 1) 
		       (array-dimension image-out 1)))))
    (error "Arrays not of same dimension~A: IMAGE-A = ~A   IMAGE-OUT = ~A"
	   (if where (format nil " [~A]" where) "")
	   (array-dimensions image-a)  (array-dimensions image-out))))

;; utility function to ensure that dimensions of 3 images are the same
(defun %check-dims3 (image-a image-b image-out &optional where)
  (declare (type (or image flag-image bit-image) image-a image-b image-out)
	   (optimize speed))
  (when 
      (or (not (and (= (array-dimension image-a 0)
		       (array-dimension image-b 0))))
	  (not (and (= (array-dimension image-a 1) 
		       (array-dimension image-b 1))))
	  (not (and (= (array-dimension image-a 0) 
		       (array-dimension image-out 0))))
	  (not (and (= (array-dimension image-a 1) 
		       (array-dimension image-out 1)))))
    (error "Arrays not of same dimension~A: IMAGE-A = ~A  IMAGE-B = ~A  IMAGE-OUT = ~A"
	   (if where (format nil " [~A]" where) "")
	   (array-dimensions image-a) (array-dimensions image-b)
	   (array-dimensions image-out))))

(defun map-images (output-image-type function &rest images)
  "Map one or more images of any mixed types to a a new image of type
OUTPUT-IMAGE-TYPE (eg, IMAGE or COMPLEX-IMAGE) using a function of
elements (function image1[i,j] image[i,j] ...)"
  (when (not images) (error "No images given."))
  (when (every (lambda (im) (typep im 'any-type-image)) images)
    "Not every element of IMAGES is a valid image type.")
  (let* ((n-images (length images))
	 (im0 (first images))
	 (ntot (array-total-size im0))
	 (nx  (array-dimension im0 1))
	 (ny  (array-dimension im0 0)))
    (when (not (every (lambda (im) (and (= (array-dimension im 1) nx)
					(= (array-dimension im 0) ny)))
		      images))
      (error "Images are not all the same size."))
    (let ((im-out (make-array
		   (list ny nx)
		   :element-type (get-array-element-type-for-image-type output-image-type))))
      ;; handle a few basic cases fast
      (cond ((= n-images 1)
	     (dotimes (i ntot)
	       (setf (row-major-aref im-out i)
		     (funcall function
			      (row-major-aref im0 i)))))
	    ((= n-images 2)
	     (let ((im1 (second images)))
	       (dotimes (i ntot)
		 (setf (row-major-aref im-out i)
		       (funcall function
				(row-major-aref im0 i)
				(row-major-aref im1 i))))))
	    ((= n-images 3)
	     (let ((im1 (second images))
	       	   (im2 (third images)))
	       (dotimes (i ntot)
		 (setf (row-major-aref im-out i)
		       (funcall function
				(row-major-aref im0 i)
				(row-major-aref im1 i)
				(row-major-aref im2 i))))))
	    ((= n-images 4)
	     (let ((im1 (second images))
	       	   (im2 (third images))
		   (im3 (fourth images)))
	       (dotimes (i ntot)
		 (setf (row-major-aref im-out i)
		       (funcall function
				(row-major-aref im0 i)
				(row-major-aref im1 i)
				(row-major-aref im2 i)
				(row-major-aref im3 i)
				)))))
	    (t ;; more than 5 images, collect args into list and use apply
	       ;; (less efficient)
	     (dotimes (i ntot)
	       (setf (row-major-aref im-out i)
		     (apply function
			    (loop for im in images
				  collect (row-major-aref im i)))))))
      im-out)))
    
;; given var X and EXPR, return EXPR when X is a valid float (not NaN
;; or Inf), else return the NaN/Inf to propagate it without math
;; errors.  X should always be a variable, not an expression,
;; and EXPR is an expression containing X
(defmacro %if-valid-float-value-p (x expr)
  `(if (not (float-utils:single-float-nan-or-infinity-p ,x))
     ,expr
     ,x))

;; for 2 vars X1,X1, return EXPR if they're both valid numbers else
;; substitute the invalid one
(defmacro %if-valid-float-values2-p (x1 x2 expr)
  `(if (not (or (float-utils:single-float-nan-or-infinity-p ,x1)
		(float-utils:single-float-nan-or-infinity-p ,x2)))
       ,expr ;; both numbers OK
       ;; else return the one that is NaN or Inf
     (if (float-utils:single-float-nan-or-infinity-p ,x1)
	 ,x1
	 ,x2)))

(defun im-log (image-in &key image-out (base #.(exp 1.0)))
  "Take logarithm of IMAGE-IN in base BASE (by default e=2.71828),
and place result in new image or IMAGE-OUT or a new image. Negative or
zero values turn into MOST-NEGATIVE-SINGLE-FLOAT."
  (declare (type image image-in)
	   (type image-or-null image-out)
	   (type (single-float (0.0)) base)
	   (optimize speed))
  (setf image-out (or image-out (%dup-image-nocopy image-in)))
  (%check-dims2 image-in image-out)
  (loop 
     with scale = (/ 1.0 (log base))  ;; to convert log base e to log
     for i of-type (unsigned-byte 29) below (array-total-size image-in)
     for val of-type single-float = (row-major-aref image-in i)
     for logval of-type single-float
       = (%if-valid-float-value-p val
				  (if (plusp val) 
				      (* scale (log val)) 
				      most-negative-single-float))
     do (setf (row-major-aref image-out i) logval)))


(defun im-scale (image-in scale &key image-out)
  "Rescale IMAGE-A by SCALE and place result into IMAGE-OUT."
  (declare (type image image-in)
	   (type (or null image) image-out)
	   (type (single-float (0.0)) scale)
	   (optimize speed))
  (setf image-out (or image-out (%dup-image-nocopy image-in)))
  (%check-dims2 image-in image-out)
  (loop
    for i of-type (unsigned-byte 29) below (array-total-size image-in)
    for z = (row-major-aref image-in i)
    do
       (setf (row-major-aref image-out i)  
	     (%if-valid-float-value-p z (* scale z))))
    image-out)

   
(defun im-increment (image-in increment &key image-out)
  "Increment IMAGE-IN by INCREMENT and place result into IMAGE-OUT or
new image"
  (declare (type image image-in)
	   (type image-or-null image-out)
	   (type single-float increment)
	   (optimize speed))
  (setf image-out (or image-out (%dup-image-nocopy image-in)))
  (%check-dims2 image-in image-out)
  (loop 
    for i of-type (unsigned-byte 29) below (array-total-size image-in)
    for z = (row-major-aref image-in i)
    do (setf (row-major-aref image-out i) 
	     (%if-valid-float-value-p z (+ increment z))))
  image-out)



(defun im+ (image-a image-b &key image-out (a-scale 1.0) (b-scale 1.0))
  "Add IMAGE-A to IMAGE-B placing result in new image or IMAGE-OUT,
which is allowed to be the same as IMAGE-A or IMAGE-B."
  (declare (type image image-a image-b)
	   (type image-or-null image-out)
	   (type single-float a-scale b-scale)
	   (optimize speed))
  (setf image-out (or image-out (%dup-image-nocopy image-a)))
  (%check-dims3 image-a image-b image-out)
  (loop for i of-type (unsigned-byte 29) below (array-total-size image-a)
	for za = (row-major-aref image-a i)
	for zb = (row-major-aref image-a i)
	do (setf (row-major-aref image-out i)
		 (%if-valid-float-values2-p
		  za zb
		  (+ (* a-scale za)
		     (* b-scale zb)))))
  image-out)


(defun im- (image-a image-b &key image-out (a-scale 1.0) (b-scale 1.0))
  "Subract IMAGE-B from IMAGE-A placing result in new image or
IMAGE-OUT, which is allowed to be the same as IMAGE-A or IMAGE-B."
  (declare (type image image-a image-b)
	   (type image-or-null image-out)
	   (type single-float a-scale b-scale)
	   (optimize debug))
  (im+ image-a image-b :image-out image-out
		       :a-scale a-scale
		       :b-scale (- b-scale)))
       

(defun im* (image-a image-b  &key image-out (a-scale 1.0) (b-scale 1.0))
  "Multiply IMAGE-A by IMAGE-B placing result in new image or
IMAGE-OUT, which is allowed to be the same as IMAGE-A or IMAGE-B."
  (declare (type image image-a image-b)
	   (type image-or-null image-out)
	   (type single-float a-scale b-scale)
	   (optimize speed))
  (setf image-out (or image-out (%dup-image-nocopy image-a)))
  (%check-dims3 image-a image-b image-out)
  (loop for i of-type (unsigned-byte 29) below (array-total-size image-a)
	for za = (row-major-aref image-a i)
	for zb = (row-major-aref image-a i)
	do (setf (row-major-aref image-out i)
		 (%if-valid-float-values2-p
		  za zb
		  (* (* a-scale (row-major-aref image-a i))
		     (* b-scale (row-major-aref image-b i))))))
  image-out)

(defun im/ (image-a image-b &key image-out (a-scale 1.0) (b-scale 1.0) 
	    (plus-infinity most-positive-single-float)
	    (minus-infinity most-negative-single-float))
  "Divide IMAGE-A by IMAGE-B placing result in new image or IMAGE-OUT,
which is allowed to be the same as IMAGE-A or IMAGE-B.  PLUS-INFINITY
and MINUS-INFINITY are used to replace divide by 0, but will not catch
all overflows."
  (declare (type image image-a image-b)
	   (type image-or-null image-out)
	   (type single-float a-scale b-scale plus-infinity minus-infinity)
	   (optimize speed))
  (setf image-out (or image-out (%dup-image-nocopy image-a)))
  (%check-dims3 image-a image-b image-out)
  (loop 
   for i of-type (unsigned-byte 29) below (array-total-size image-a)
   for a of-type single-float = (row-major-aref image-a i)
   for b of-type single-float = (row-major-aref image-b i)
   for result of-type single-float 
     = (%if-valid-float-values2-p
	a b
	(cond ((not (zerop b)) 
	       (/ (* a-scale a) (* b-scale b)))
	      ((plusp a) 
	       plus-infinity)
	      ((minusp a) 
	       minus-infinity)
	      (t 0.0))) ;; won't happen but makes compiler happy
   do (setf (row-major-aref image-out i) result))
  image-out)


(defun fill-image (image x)
  "Fill a single-float 2d image with value X"
  (declare (type image image)
	   (type  single-float x)
	   (optimize speed))
  (loop for i below (array-total-size image)
     do (setf (row-major-aref image i) x))
  image)




  




(defun find-next-power-of-two (n)
  "find the next power of 2 that is greater than or equal to N"
  (declare (type (unsigned-byte 28) n))
  (loop
     initially (when (zerop n) (return 0))
     with i of-type (unsigned-byte 28) = 1
     until (>= i n)
     do (setf i (ash i 1))
     finally (return i)))

(defun is-power-of-two (n)
  "is N a power of 2?"
  (= n (find-next-power-of-two n)))


(defun pad-image-2^n (image &key (center nil))
  "inflate a two-dimensional single-float array so that its dimensions are
2^n - the array is padded with zeros at the large indices in each dimension

If CENTER is T, then then resulting image is centered as best possible, with
the short side of the empty frame toward the small indices.

Return (values image2n i0 j0) where i0,j0 are the initial indices of image[0,0]
in the new image image2n.  These can be passed to trim-2d-image to restore the original image.
"
  (declare (type image image)
	   (optimize speed))
  (loop
     with n0 = (array-dimension image 0)
     with n1 = (array-dimension image 1)
     with m0 = (find-next-power-of-two n0)
     with m1 = (find-next-power-of-two n1)
     with b2 = (make-array (list m0 m1) :element-type 'single-float :initial-element 0e0)
     ;; i0 and j0 determine if image is centered
     with ix0 = (if (not center) 0 (ash (- m0 n0) -1))
     with iy0 = (if (not center) 0 (ash (- m1 n1) -1))
     for iy of-type (unsigned-byte 28) below n0
     do
       (loop 
	for ix of-type (unsigned-byte 28) below n1
	do
	(setf (aref b2 (+ iy0 iy) (+ ix0 ix)) (aref image iy ix)))
     finally (return (values b2 iy0 ix0))))


(defun trim-image (image m0 m1 &key (iy0 0) (ix0 0))
  "trim a 2d single array so that it has dimensions m0 x m1, beginning the copy from 
array indices IY0 IX0 - note that M0 is Y dimension, M1 is X dimension"
  (declare (type image image)
	   (type (unsigned-byte 28) m0 m1 iy0 ix0)
	   (optimize speed))
  (loop
     with n0 = (array-dimension image 0)
     with n1 = (array-dimension image 1)
     with b2 = (make-array (list m0 m1) :element-type 'single-float :initial-element 0e0)
     initially
       (when (or (> m0 n0) (> m1 n1))
	 (error "array with array dimensions ~Dx~D can't be trimmed to ~Dx~D" n0 n1 m0 m1))
     for iy of-type (unsigned-byte 28) below m0
     do
       (loop for ix of-type (unsigned-byte 28) below m1
	  do
	    (setf (aref b2 iy ix) (aref image (+ iy iy0) (+ ix ix0))))
     finally (return b2)))


(defun sub-image (image ix0 ix1 iy0 iy1)
  "Return the sub-image of image containing the indices
  [iy0:iy1][ix0:ix1] - note that the ranges are inclusive, unlike
many standard Lisp functions"
  (trim-image image 
	      (1+ (- iy1 iy0))
	      (1+ (- ix1 ix0))
	      :iy0 iy0 :ix0 ix0))
