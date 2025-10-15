#|

routines to fill an image with a value (typically NaN)
for, eg, blocking out bad zones

|#

(in-package imutils)



(defun imfill-corner (image ix iy triangle-loc value)
  "Fill a corner of an image with VALUE, defined by index IY on the side,
by IX on the top/bottom, and TRIANGLE-LOC being one of
 :TOP-LEFT :TOP-RIGHT :BOTTOM-LEFT :BOTTOM-RIGHT"
  (declare (type image image)
	   (type imindex ix iy)
	   (type (member :top-left :top-right :bottom-left :bottom-right)
		 triangle-loc)
	   (type single-float value)
	   (optimize speed))
  (let* ((ix0 0) (ix1 0) (iy0 0) (iy1 0)
	 (nx (1- (array-dimension image 1)))
	 (ny (1- (array-dimension image 0)))
	 (dy 0)
	 (ix (max 0 (min ix nx)))
	 (iy (max 0 (min iy ny))))
    
    (declare (type imindex ix0 ix1 iy0 iy1)
	     (type (integer -1 1) dy))

    (cond ((eq triangle-loc :bottom-left)
	   (setf ix0 0
		 iy0 iy
		 ix1 ix
		 iy1 0
		 dy -1))
	  ((eq triangle-loc :top-left)
	   (setf ix0 0
		 iy0 iy
		 ix1 ix
		 iy1 ny
		 dy +1))
	  ((eq triangle-loc :bottom-right)
	   (setf ix0 ix
		 iy0 0
		 ix1 nx
		 iy1 iy
		 dy -1))
	  ((eq triangle-loc :top-right)
	   (setf ix0 ix
		 iy0 ny
		 ix1 nx
		 iy1 iy
		 dy +1)))

    ;; move from ix0 to ix1, computing y, and filling in
    ;; vertical line in direction dy
    (loop
      ;; line has slope y=ax+b
      with a of-type single-float = (/ (float (- iy1 iy0) 0.0)
				       (- ix1 ix0))
      with b of-type single-float = (- iy0 (* a ix0))
      for jx from ix0 to ix1 
      for y of-type (single-float -1e8 1e8) = (+  (* a jx) b)
      for jy0 = (max 0 (min (round y) ny))
      for jy1 = (if (= dy +1) (1+ ny) -1)
      do
	 (loop for jy of-type (signed-byte 28) = jy0 then (+ jy dy)
	       until (= jy jy1)
	       do
		  (setf (aref image jy jx) value)))))
      
	  

(defun imfill-rectangle (image ix0 iy0 ix1 iy1 value)
    (declare (type image image)
	     (type imindex ix0 iy0 ix1 iy1)
	     (type single-float value)
	     (optimize speed))
  "Fill a rectangle from IX0,IY0 to IX1,IY1 with VALUE"
  ;;
  (when (< ix1 ix0) (rotatef ix1 ix0))
  (when (< iy1 iy0) (rotatef iy1 iy0))
  ;;
  (let* ((nx (1- (array-dimension image 1)))
	 (ny (1- (array-dimension image 0)))
	 (ix0 (max 0 ix0))
	 (ix1 (min nx ix1))
	 (iy0 (max 0 iy0))
	 (iy1 (min ny iy1)))
    (loop for iy from iy0 to iy1
	  do (loop for ix from ix0 to ix1
		   do (setf (aref image iy ix) value)))))

(defun imfill-edge (image width side value)
  "Fill a strip of IMAGE of WIDTH pixels, on SIDE
in :TOP BOTTOM :LEFT :RIGHT, with VALUE."
  (declare (type image image)
	   (type (integer 1 #.(expt 2 28)) width)
	   (type single-float value)
	   (type (member :top :bottom :left :right))
	   (optimize speed))
  (let* ((nx (1- (array-dimension image 1)))
	 (ny (1- (array-dimension image 0)))
	 (w (1- width)) ;; counts are from 0 to width -1
	 (ix0 0) (iy0 0) (ix1 0) (iy1 0))

    (cond ((eq side :top)
	   (setf ix0 0
		 ix1 nx
		 iy0 (- ny w)
		 iy1 ny))
	  ((eq side :bottom)
	   (setf ix0 0
		 ix1 nx
		 iy0 0
		 iy1 w))
	  ((eq side :left)
	   (setf ix0 0
		 ix1 w
		 iy0 0
		 iy1 ny))
	  ((eq side :right)
	   (setf ix0 (- nx w)
		 ix1 nx
		 iy0 0
		 iy1 ny)))

    (imfill-rectangle image ix0 iy0 ix1 iy1 value)))
	  
    
(defun imfill-border (image width value)
  "Fill a border of WIDTH of an IMAGE with VALUE."
  (declare (type image image)
	   (type imindex width)
	   (type single-float value))
  (imfill-edge image width :top value)
  (imfill-edge image width :bottom value)
  (imfill-edge image width :left value)
  (imfill-edge image width :right value))


(defun imfill-above/below-line (image ix0 iy0 ix1 iy1 above/below value &key (extend t))
  "Fill above or below a line from (IX0,IY0) to (IX1,IY1) with VALUE,
where above/below is :ABOVE or :BELOW.   The line is extended in X to edges
of the chip if EXTED is true, by default."
  (declare (type image image)
	   (type imindex ix0 iy0 ix1 iy1)
	   (type (member :above :below) above/below)
	   (type single-float value))
  (let* ((nx (1- (array-dimension image 1)))
	 (ny (1- (array-dimension image 0)))
	 (dy (if (eq above/below :above) +1 -1)))
    ;; move from ix0 to ix1, computing y, and filling in
    ;; vertical line in direction dy
    (loop
      ;; line has slope y=ax+b
      with a of-type single-float = (/ (float (- iy1 iy0) 0.0)
				       (- ix1 ix0))
      with b of-type single-float = (- iy0 (* a ix0))
      for jx from (if extend 0 (max ix0 0)) to (if extend nx (min ix1 nx))
      for y of-type (single-float -1e8 1e8) = (+  (* a jx) b)
      for jy0 = (max 0 (min (round y) ny))
      for jy1 = (if (= dy +1) (1+ ny) -1)
      do
	 (loop for jy of-type (signed-byte 28) = jy0 then (+ jy dy)
	       until (= jy jy1)
	       do
		  (setf (aref image jy jx) value)))))


(defun imfill-polygon (image xvec-poly yvec-poly value)
  "Fill a polygonal region in IMAGE with VALUE, with polygon defined
by vertices in single-float arrays XVEC-POLY, YVEC-POLY."
  (declare (type image image)
	   (type (simple-array single-float (*)) xvec-poly yvec-poly)
	   (type single-float value)
	   (optimize speed))

  (when (not (= (length xvec-poly)
		(length yvec-poly)))
    (error "Polygon vectors not of equal length."))
  
  (let* ((nx (1- (array-dimension image 1)))
	 (ny (1- (array-dimension image 0)))
	 (ix0 #.(ash most-positive-fixnum -2))
	 (iy0 #.(ash most-positive-fixnum -2))
	 (ix1 #.(ash most-negative-fixnum -2))
	 (iy1 #.(ash most-negative-fixnum -2)))

    (declare (type fixnum nx ny ix0 iy0 ix1 iy1))

    ;; only look inside points that COULD be in polygon
    (loop for x of-type (float -1e8 1e8) across xvec-poly
	  for y of-type (float -1e8 1e8) across yvec-poly
	  do
	     (setf ix0 (min (floor x) ix0))
	     (setf ix1 (max (ceiling x) ix1))
	     (setf iy0 (min (floor y) iy0))
	     (setf iy1 (max (ceiling y) iy1)))
    
    ;; make sure limits are in bounds
    (setf ix0 (max ix0 0)
	  iy0 (max iy0 0)
	  ix1 (min ix1 nx)
	  iy1 (min iy1 ny))

    (print (list ix0 iy0 ix1 iy1))
    (flet ((is-in-polygon (x y)
	     (declare (type (single-float -1e8 1e8) x y))
	     (loop with inside = nil
		   with nm1 of-type (unsigned-byte 27) = (1- (length xvec-poly))
		   with j of-type (unsigned-byte 27) = nm1
		   for i of-type (unsigned-byte 27) from 0 to nm1
		   do
		      (when (and (or (and (<= (aref yvec-poly i) y) (< y (aref yvec-poly j)))
				     (and (<= (aref yvec-poly j) y) (< y (aref yvec-poly i))))
				 (< x (+ (aref xvec-poly i)
					 (/ (* (- (aref xvec-poly j) 
						  (aref xvec-poly i)) 
					       (- y (aref yvec-poly i)))
					    (- (aref yvec-poly j) (aref yvec-poly i))))))
			(setf inside (not inside)))
		      (setf j i)
		   finally (return inside))))
      ;;
      (loop for iy from iy0 to iy1
	    do (loop for ix from ix0 to ix1
		     when (is-in-polygon (float ix 1.0) (float iy 1.0))
		       do (setf (aref image iy ix) value))))))
    
    
    
	   
    
		 
	   
    
