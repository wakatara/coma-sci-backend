(in-package imutils)


;; apply a Laplace filter
(defun apply-3x3-laplace-filter (im &key (kernel-type 4))
  "Apply a 3x3 Laplace filter to an image, returning a new image.

KERNEL-TYPE determines which approximation is used.

For KERNEL-TYPE=4:

     [[  0    -0.25    0 ]
      [-0.25  +1.00  -0.25]
      [  0    -0.25    0 ]]

For KERNEL-TYPE=8:

     [[-0.125 -0.125 -0.125]
      [-0.125 +1.000 -0.125]
      [-0.125 -0.125 -0.125]]
"
  
  (declare (type image im)
	   (type (member 4 8) kernel-type)
	   (optimize speed))
  ;; do the core of the image
  (let ((imout (make-same-size-image im :initial-value 0.0))
	(nx (1- (array-dimension im 1))) ;; max indices
	(ny (1- (array-dimension im 0))))
    (declare (type image imout))
    (float-utils:with-float-traps-masked (:invalid t) ;; allow NaN to quietly propagate - maybe not needed?
      (cond ((= kernel-type 8)
	     (loop  
	       for iy from 0 to ny
	       for iy-1 = (if (= iy  0) 1        (- iy 1)) ;; wrap back at edges
	       for iy+1 = (if (= iy ny) (- ny 1) (+ iy 1))
	       do
		  (loop
		    for ix from 0 to nx
		    for ix-1 = (if (= ix  0) 1        (- ix 1))
		    for ix+1 = (if (= ix nx) (- nx 1) (+ ix 1))
		    for delta of-type single-float
		      = (- 
			 (* 0.125 (+ (aref im iy+1 ix-1)
				     (aref im iy+1 ix)
				     (aref im iy+1 ix+1)
				     ;;
				     (aref im iy ix-1)
				     (aref im iy ix+1)
				     ;;
				     (aref im iy-1 ix-1)
				     (aref im iy-1 ix)
				     (aref im iy-1 ix+1))))
		    do (setf (aref imout iy ix) delta))))
	    ((= kernel-type 4)
	      (loop  
	       for iy from 0 to ny
	       for iy-1 = (if (= iy  0) 1        (- iy 1)) ;; wrap back at edges
	       for iy+1 = (if (= iy ny) (- ny 1) (+ iy 1))
	       do
		  (loop
		    for ix from 0 to nx
		    for ix-1 = (if (= ix  0) 1        (- ix 1))
		    for ix+1 = (if (= ix nx) (- nx 1) (+ ix 1))
		    for delta of-type single-float
		      = (- 
			 (* 0.25 (+ (aref im iy+1 ix)
				    (aref im iy ix-1)
				    (aref im iy ix+1)
				    (aref im iy-1 ix))))
		    do (setf (aref imout iy ix) delta)))))
    imout)))

