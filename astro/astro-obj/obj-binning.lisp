;; routines for binning objects and retrieving nearest neighbors
;; some routines have variants for XYOBJ and OBJ, the difference being
;; that the OBJ version does a magnitude comparison too


(in-package astro-obj)


(eval-when (load eval compile)
  (defparameter *debug-setting* 1)
  (defparameter *safety-setting* 1))  ;; change for debugging

(defun dvecmax (v)
  "max of double-float vec"
  (declare (type (simple-array double-float) v)
	   (optimize
	    (speed 0)
	    (safety #.*safety-setting*)
	    (debug  #.*debug-setting*)))
  (loop for x of-type double-float across v
	for max of-type double-float = (aref v 0)
	do (setf max (max max x))
	finally (return max)))

	
(defun dvecmin (v)
  "min of double-float vec"
  (declare (type (simple-array double-float) v)
	   (optimize (speed 3)
		     (safety #.*safety-setting*)
		     (debug  #.*debug-setting*)))
  (loop for x of-type double-float across v
	for min of-type double-float = (aref v 0)
	do (setf min (min min x))
	finally (return min)))  

;; floor and ceiling that inline nicely with a fixnum result
(defmacro dbl-ceiling (x)
  `(locally
    (declare (inline ceiling))
    (ceiling (the (double-float -1d8 1d8) ,x))))
(defmacro dbl-floor (x)
  `(locally
    (declare (inline floor))
    (floor (the (double-float -1d8 1d8) ,x))))


(declaim (inline objgrid-xy->ix-iy))
(defun objgrid-xy->ix-iy (x y objgrid)
  "convert x y position to the bin into which the objects fall
return NIL if out of bounds, else return (values ix iy)"
  (declare (type double-float x y)
	   (type objgrid objgrid)
	   (optimize (speed 3)
		     (safety #.*safety-setting*)
		     (debug  #.*debug-setting*)))
  (cond ((or (< x (objgrid-xmin objgrid)) (> x (objgrid-xmax objgrid))
	     (< y (objgrid-ymin objgrid)) (> y (objgrid-ymax objgrid)))
	 nil)
	(t ;; in bounds
	 (values
	  (dbl-floor (/ (- x (objgrid-xmin objgrid))
			(objgrid-dxbin objgrid)))
	  (dbl-floor (/ (- y (objgrid-ymin objgrid))
			(objgrid-dybin objgrid)))))))


(defun insert-object (obj objgrid)
  (declare (type xyobj obj)
	   (type objgrid objgrid))
  (multiple-value-bind (ix iy)
      (objgrid-xy->ix-iy (xyobj-x obj) (xyobj-y obj) objgrid)
    (when ix ;; ie, when in-bounds
      (push obj (aref (objgrid-bins objgrid) iy ix))
      (push obj (objgrid-list objgrid))
      (incf (objgrid-nobj objgrid)))))



   
		 
(defun bin-objects (obj-list &key (nx 100) (ny 100)
		    (xmin nil) (xmax nil)
		    (ymin nil) (ymax nil))
  (declare (type (integer 0 1000000) nx ny)
	   (type (or null real) xmin xmax ymin ymax)
	   (type list obj-list))
  "create a OBJGRID structure from a list of OBJ structures, in a grid
of count NX times NY, using supplied min xmin,xmax values if given"

  (when (not obj-list)
    (error "BIN-OBJECTS: OBJ-LIST is empty."))
  (let ((xminc (or xmin (xyobj-x (car obj-list)))) 
	(xmaxc (or xmax (xyobj-x (car obj-list))))
	(yminc (or ymin (xyobj-y (car obj-list))))
	(ymaxc (or ymax (xyobj-y (car obj-list))))
	(dx   0.0d0)
	(dy   0.0d0)
	(dxbin 0.0d0)
	(dybin 0.0d0))
    (loop for obj in obj-list ;; use entire list in case just 1 obj
	  do
	  (when (not xmin) (setf xminc (min xminc (xyobj-x obj))))
	  (when (not xmax) (setf xmaxc (max xmaxc (xyobj-x obj))))
	  (when (not ymin) (setf yminc (min yminc (xyobj-y obj))))
	  (when (not ymax) (setf ymaxc (max ymaxc (xyobj-y obj)))))
    ;; when xmin.. not specified, nudge our computed values out
    ;; a little to ensure that all objects are contained inside grid
    (when (not xmin) (decf xminc (* (- xmaxc xminc) 0.01d0)))
    (when (not xmax) (incf xmaxc (* (- xmaxc xminc) 0.01d0)))
    (when (not ymin) (decf yminc (* (- ymaxc yminc) 0.01d0)))
    (when (not ymax) (incf ymaxc (* (- ymaxc yminc) 0.01d0)))
    ;; when xminc and xmxac are the same (just one object) nudge them
    (when (= xminc xmaxc)
      (decf xminc 1.0)
      (incf xmaxc 1.0))
    (when (= yminc ymaxc)
      (decf yminc 1.0)
      (incf ymaxc 1.0))
    ;;
    (when (or (>= xminc xmaxc) (>= yminc ymaxc))
      (error "bin-objects: xmin=~F >= xmax=~F   or   ymin=~F >= ymax=~F
Perhaps values specified on invocation were inconsistent with each
other or with remaining computed values xminc, xmaxc, yminc, ymaxc"
	     xmin xmax ymin ymax))
    ;;
    (setf xminc (coerce xminc 'double-float))
    (setf xmaxc (coerce xmaxc 'double-float))
    (setf yminc (coerce yminc 'double-float))
    (setf ymaxc (coerce ymaxc 'double-float))
    ;;
    (setf dx (float (- xmaxc xminc) 1d0)
	  dy (float (- ymaxc yminc) 1d0)
	  dxbin (float (/ dx nx) 1d0)
	  dybin (float (/ dy ny) 1d0))
    (locally 
	(declare (type double-float dx dy dxbin dybin)
		 (optimize speed)
		 (inline make-objgrid))
      (loop with objgrid =  (make-objgrid
			     :xmin xminc :xmax xmaxc :dx dx
			     :ymin yminc :ymax ymaxc :dy dy
			     :dxbin (/ dx nx) :dybin (/ dy ny)
			     :nx nx :ny ny
			     :bins (make-array (list ny nx) :initial-element nil)
			     :list nil)
	    for obj in obj-list
	    do (insert-object obj objgrid)
	    finally
	    #+nil 
	    (setf (objgrid-list objgrid)
		  (sort (objgrid-list objgrid)
			(lambda (s1 s2) (< (obj-mag s1) (obj-mag s2)))))
	    (return objgrid)))))
    
    
	


	   

(defmacro xydist-sqr (x1 y1 x2 y2)
  `(+ (expt (- ,x1 ,x2) 2) (expt (- ,y1 ,y2) 2)))


(declaim (inline find-nearest-obj-in-list find-nearest-xyobj-in-list))
(defun find-nearest-obj-in-list (x y mag maxdistsqr dmag objlist
				 &optional (not-this-object nil)
				 (test-func nil))
  "find the nearest object in objlist, provided it is within maxdistsqr and
dmag of x,y,mag.  If NOT-THIS-OBJECT is an OBJ, then any object that is EQ
to NOT-THIS-OBJECT is ignored, so that it is possible to find an objects's
neighbor even if it is in the list.

Test FUNC, if defined, is a function that returns T if this object is acceptable.

As second value, return the distance.
"
  (declare (type double-float x y mag dmag maxdistsqr)
	   (type (or null (function (obj))) test-func)
	   (optimize
	    (speed 3)
	    (safety #.*safety-setting*)
	    (debug  #.*debug-setting*)))
  (loop 
     for obj of-type obj in objlist
     with bestobj = nil
     with bestdistsqr of-type (double-float 0d0) = maxdistsqr
     for distsqr of-type (double-float 0d0) =
       (xydist-sqr x y (obj-x obj) (obj-y obj))
     do
     ;;(print (list x (obj-x obj) y (obj-y obj) (sqrt distsqr)))
       (when (and 
	      (< distsqr bestdistsqr) ;; close enough? 
	      (<  (abs (- mag (obj-mag obj))) dmag)
	      (not (eq not-this-object obj)) 
	      (or (not test-func) (funcall test-func obj)))
	 (setf bestdistsqr distsqr)
	 (setf bestobj obj))
     finally (return (values bestobj 
			     (if (not bestobj) 0d0 
				 (sqrt (the (double-float 0d0) bestdistsqr)))))))
	
 

(defun find-nearest-xyobj-in-list (x y maxdistsqr objlist
				   &optional (not-this-object nil)
				   (test-func nil))
  "find the nearest object in objlist, provided it is within
maxdistsqr of x,y.  If NOT-THIS-OBJECT is an OBJ, then any object that
is EQ to NOT-THIS-OBJECT is ignored, so that it is possible to find an
objects's neighbor even if it is in the list.
Test FUNC, if defined, is a function that returns T if this object is acceptable.

As second value, return the distance.
"
  (declare (type double-float x y maxdistsqr)
	   (type (or null (function (obj))) test-func)
	   (optimize (speed 3)
		     (safety #.*safety-setting*)
		     (debug  #.*debug-setting*)))
  (loop for obj of-type xyobj in objlist
	with bestobj = nil
	with bestdistsqr of-type double-float = maxdistsqr
	for distsqr of-type (double-float 0d0) =
        	(xydist-sqr x y (xyobj-x obj) (xyobj-y obj))
	do
       ;;(print (list x (xyobj-x obj) y (xyobj-y obj) (sqrt distsqr)))
	(when (and 
	       (< distsqr bestdistsqr) ;; close enough? 
	       (not (eq not-this-object obj)) 
	       (or (not test-func) (funcall test-func obj)))
	  (setf bestdistsqr distsqr)
	  (setf bestobj obj))
	finally (return (values bestobj 
				(if (not bestobj) 0d0 
				    (sqrt (the (double-float 0d0) bestdistsqr)))))))
	
	





;; slightly clever version that breaks off
;; search; original version is commented out at bottom of file
(defun get-nearest-object (obj objgrid dist dmag &optional
			     (dx 0.0d0) (dy 0.0d0) (ignore-eq t)
			     (test-func nil))
  "get the nearest obj object in objgrid within dist of obj's x,y;
if no suitable obj object is found, then return NIL - dx and dy are
offsets to add to obj's x,y before finding nearest obj

IGNORE-EQ (T by default) causes us to ignore any object that is EQ to 
OBJ.  This is useful for finding a gridded object's neighbors.

TEST-FUNC is a function applied to the candidate returned object,
returning T if the candidate is acceptable for a match. It can be
used to provide further object rejection."
  (declare (type objgrid objgrid)
	   (type obj obj)
	   (type double-float dist dx dy)
	   (optimize (speed 3)
		     (safety #.*safety-setting*)
		     (debug  #.*debug-setting*)))
  
  (when (> (/ dist (min (objgrid-dxbin objgrid)
			(objgrid-dybin objgrid)))
	   1d8)
    (error "DIST is too large - rounding to fixnum will fail. Use something sensible."))

  (let ((nx 0)
	(ny 0)
	(mag (obj-mag obj))
	(x   (+ dx (obj-x obj)))
	(y   (+ dy (obj-y obj)))
	(distsqr (* dist dist)))
    (declare (type (or null (unsigned-byte 25)) nx ny)
	     (type double-float x y distsqr))
    (multiple-value-setq (nx ny) (objgrid-xy->ix-iy x y objgrid))
    (cond ((not nx) ;; out of bounds, return no match
	   nil) 
	  (t
	   ;; do the primitive thing of searching all the adjacent bins
	   ;; within index range ix-nsx, ix+nsx, etc, abandoning search after
	   ;; can't do better - could be fixed by searching in squares 1 pix on side,
	   ;; then 3x3, then 5x5, but this would be a PITA
	   (let* ((dxbin (objgrid-dxbin objgrid))
		  (dybin (objgrid-dybin objgrid))
		  (nsx (dbl-ceiling (/ dist dxbin)))
		  (nsy (dbl-ceiling (/ dist dybin)))
		  (nx0 (max (- nx nsx) 0))
		  (nx1 (min (+ nx nsx) (1- (objgrid-nx objgrid))))
		  (ny0 (max (- ny nsy) 0))
		  (ny1 (min (+ ny nsy) (1- (objgrid-ny objgrid)))))
	     (declare (type (unsigned-byte 25) 
			    nx ny ;; now these are integers
			    nsx nsy nx0 nx1 ny0 ny1))
	     (loop 
		with best-object = nil
		with best-dist of-type double-float = 0.0d0
		for idx of-type (unsigned-byte 25) from 0 to nsx
		;; now a slightly clever bit - we break off the
		;; iteration when we can no longer beat the best
		;; object found by going farther out the grid - rule
		;; is that the largest distance between any point in
		;; center bin and any point in this bin must be more
		;; than best-dist. This cleverness is only useful for
		;; dist>>dxbin, however.  In such a case, it can be
		;; huge advantage.
		until (and best-object (> (* idx dxbin) (+ dxbin best-dist)))
		do
		  (loop 
		     with isx-max = (if (zerop idx) 0 1) ;; avoid double-looping for idx=0
		     for isx of-type (signed-byte 3) from -1 to isx-max by 2
		     for ix of-type (signed-byte 25) = (+ nx (* idx isx))
		     when (and (>= ix nx0) (<= ix nx1))
		     do
		     ;; now do y loop
		       (loop
			  for idy of-type (unsigned-byte 25) from 0 to nsy
			  ;; again, break off search when no chance of finding better
			  until (and best-object (> (* idy dybin) (+ dybin best-dist)))
			  do
			    (loop 
			       with isy-max = (if (zerop idy)  0 1) ;; avoid double-looping for idy=0
			       for isy of-type (signed-byte 3) from -1 to isy-max by 2
			       for iy of-type (signed-byte 25) = (+ ny (* idy isy))
			       when (and (>= iy ny0) (<= iy ny1))
			       do
				 (let ((objlist (aref (objgrid-bins objgrid) iy ix)))
				   (when objlist
				     (multiple-value-bind (nearest-object mindist)
					 (find-nearest-obj-in-list 
					  x y (float mag 1d0) distsqr dmag
					  objlist (if ignore-eq obj)
					  test-func)
				       (declare (type (or null obj) nearest-object)
						(type (double-float 0d0) mindist))
				       (when (and nearest-object
						  ;; we already tested EQ, and TEST-FUNC
						  (or (not best-object)
						      (< mindist best-dist)))
					 (setf best-object nearest-object) 
					 (setf best-dist mindist))))))))
		finally (return best-object)))))))
			      

			      
 

;; a version working on just XY objects
(defun get-nearest-xyobject (xyobj objgrid dist &optional
			     (dx 0.0d0) (dy 0.0d0) (ignore-eq t)
			     (test-func nil))
  "get the nearest xyobj object in objgrid within dist of obj's x,y;
if no suitable obj object is found, then return NIL - dx and dy are
offsets to add to obj's x,y before finding nearest obj

IGNORE-EQ (T by default) causes us to ignore any object that is EQ to 
OBJ.  This is useful for finding a gridded object's neighbors.

TEST-FUNC is a function applied to the candidate returned object,
returning T if the candidate is acceptable for a match. It can be
used to provide further object rejection."
  (declare (type objgrid objgrid)
	   (type xyobj xyobj)
	   (type double-float dist dx dy)
	   (optimize (speed 3)
		     (safety #.*safety-setting*)
		     (debug  #.*debug-setting*)))

  (when (> (/ dist (min (objgrid-dxbin objgrid)
			(objgrid-dybin objgrid)))
	   1d8)
    (error "DIST is too large - rounding to fixnum will fail. Use something sensible."))

  (let ((nx 0)
	(ny 0)
	(x   (+ dx (xyobj-x xyobj)))
	(y   (+ dy (xyobj-y xyobj)))
	(distsqr (* dist dist)))
    (declare (type (or null (unsigned-byte 25)) nx ny)
	     (type double-float x y distsqr))
    (multiple-value-setq (nx ny) (objgrid-xy->ix-iy x y objgrid))
    (cond ((not nx) ;; out of bounds, return no match
	   nil) 
	  (t
	   ;; do the primitive thing of searching all the adjacent bins
	   ;; within index range ix-nsx, ix+nsx, etc, abandoning search after
	   ;; can't do better - could be fixed by searching in squares 1 pix on side,
	   ;; then 3x3, then 5x5, but this would be a PITA
	   (let* ((dxbin (objgrid-dxbin objgrid))
		  (dybin (objgrid-dybin objgrid))
		  (nsx (dbl-ceiling (/ dist dxbin)))
		  (nsy (dbl-ceiling (/ dist dybin)))
		  (nx0 (max (- nx nsx) 0))
		  (nx1 (min (+ nx nsx) (1- (objgrid-nx objgrid))))
		  (ny0 (max (- ny nsy) 0))
		  (ny1 (min (+ ny nsy) (1- (objgrid-ny objgrid)))))
	     (declare (type (unsigned-byte 25) 
			    nx ny ;; now these are integers
			    nsx nsy nx0 nx1 ny0 ny1))
	     (loop 
		with best-object = nil
		with best-dist of-type double-float = 0.0d0
		for idx of-type (unsigned-byte 25) from 0 to nsx
		;; now a slightly clever bit - we break off the
		;; iteration when we can no longer beat the best
		;; object found by going farther out the grid - rule
		;; is that the largest distance between any point in
		;; center bin and any point in this bin must be more
		;; than best-dist.  This cleverness is only useful for
		;; dist>>dxbin, however.  In such a case, it can be
		;; huge advantage.
		until (and best-object (> (* idx dxbin) (+ dxbin best-dist)))
		do
		  (loop 
		     with isx-max = (if (zerop idx) 0 1) ;; avoid double-looping for idx=0
		     for isx of-type (signed-byte 3) from -1 to isx-max by 2
		     for ix of-type (signed-byte 25) = (+ nx (* idx isx))
		     when (and (>= ix nx0) (<= ix nx1))
		     do
		     ;; now do y loop
		       (loop
			  for idy of-type (unsigned-byte 25) from 0 to nsy
			  ;; again, break off search when no chance of finding better
			  until (and best-object (> (* idy dybin) (+  dybin best-dist)))
			  do
			    (loop 
			       with isy-max = (if (zerop idy)  0 1) ;; avoid double-looping for idy=0
			       for isy of-type (signed-byte 3) from -1 to isy-max by 2
			       for iy of-type (signed-byte 25) = (+ ny (* idy isy))
			       when (and (>= iy ny0) (<= iy ny1))
			       do
				 (let ((objlist (aref (objgrid-bins objgrid) iy ix)))
				   (when objlist
				     (multiple-value-bind (nearest-object mindist)
					 (find-nearest-xyobj-in-list
					  x y distsqr 
					  objlist (if ignore-eq xyobj)
					  test-func)
				       (declare (type (or null xyobj) nearest-object)
						(type (double-float 0d0) mindist))
				       (when (and nearest-object
						  ;; we already tested EQ, and TEST-FUNC
						  (or (not best-object)
						      (< mindist best-dist)))
					 (setf best-object nearest-object) 
					 (setf best-dist mindist))))))))
		finally (return best-object)))))))
			      



(declaim (inline obj-distance))
(defun obj-distance (obj1 obj2)
  (declare (type xyobj obj1 obj2))
  "Return distance between two objects, using x,y coordinates."
  (sqrt (+ (expt  (- (xyobj-x obj1) (xyobj-x obj2)) 2)
	   (expt  (- (xyobj-y obj1) (xyobj-y obj2)) 2))))



(defstruct nearest-objects
  (n 0 :type (unsigned-byte 25)) ;; number of elements in v; last index is n-1
  (v (make-array 500 :initial-element nil) :type (simple-array t (*)))
  (n-avail 500 :type (unsigned-byte 25)))

;; double size of nearest-objects structure, preserving content
(defun extend-nearest-objects (nearest-objects)
  (declare (type nearest-objects nearest-objects))
  (let ((v (make-array (* 2 (nearest-objects-n-avail nearest-objects))
		       :element-type t :initial-element nil)))
    (loop for x across  (nearest-objects-v nearest-objects)
	  for i of-type (unsigned-byte 25)  = 0 then (1+ i)
	  do (setf (aref v i) x))
    (setf (nearest-objects-v nearest-objects) v)
    (setf (nearest-objects-n-avail nearest-objects) (* 2 (nearest-objects-n-avail nearest-objects)))
    nearest-objects))
    
    
  


(defun get-nearest-objects (obj objgrid dist dmag &optional
			    (dx 0.0d0) (dy 0.0d0) (nearest-objects nil)
			    (ignore-eq nil)
			    (test-func nil))
  "get the NEAREST-OBJECTS (a structure) in objgrid within dist of
obj's x,y; -- returns struct of type nearest-objects, or uses nearest-objects if it
is NIL -- if no suitable obj object is found, then return NIL - match
must also be within dmag of obj's magnitude - dx and dy are offsets to
add to obj's x,y before finding nearest obj

TEST-FUNC is a function applied to the candidate returned object,
returning T if the candidate is acceptable for a match. It can be
used to provide further object rejection."
  (declare (type objgrid objgrid) 
	   (type obj obj)
	   (type (or null (function (obj))) test-func)
	   (type double-float dist dmag dx dy)
	   (optimize (speed 3) (safety #.*safety-setting*))) 
  (let ((nearest-objects (or nearest-objects (make-nearest-objects)))
	(nx 0)
	(ny 0)
	(mag (obj-mag obj))
	(x   (+ dx (obj-x obj)))  
	(y   (+ dy (obj-y obj)))
	(distsqr (* dist dist)))
    (declare (type (or null (unsigned-byte 25)) nx ny)
	     (type single-float mag)
	     (type double-float x y distsqr))
    (setf (nearest-objects-n nearest-objects) 0)
    (multiple-value-setq (nx ny) (objgrid-xy->ix-iy x y objgrid))
    (cond ((not nx) ;; out of bounds, return no match
	   nil)
	  (t
	   ;; do the primitive thing of searching all the adjacent bins
	   ;; within index range ix-nsx, ix+nsx, etc
	   (let* ((nsx (dbl-ceiling (/ dist (objgrid-dxbin objgrid))))
		  (nsy (dbl-ceiling (/ dist (objgrid-dybin objgrid))))
		  (nx0 (max (- nx nsx) 0))
		  (nx1 (min (+ nx nsx) (1- (objgrid-nx objgrid))))
		  (ny0 (max (- ny nsy) 0))
		  (ny1 (min (+ ny nsy) (1- (objgrid-ny objgrid)))))
	     (declare (type (unsigned-byte 25) nsx nsy nx0 nx1 ny0 ny1))
	     (loop
	      for ix of-type (unsigned-byte 25) from nx0 to nx1
	      do
	      (loop
	       for iy of-type (unsigned-byte 25) from ny0 to ny1
	       for objlist = (aref (objgrid-bins objgrid) iy ix)
	       do
	       (when objlist
		 (loop for this-object of-type obj in objlist
		       do
		       (when (and 
			      (or (not ignore-eq)
				  (not (eq obj this-object)))
			      (or (not test-func)
				  (funcall test-func this-object))
			      (>= distsqr
				  (xydist-sqr x y (obj-x this-object)
					      (obj-y this-object)))
			      (>= dmag
				  (abs (- mag (obj-mag this-object)))))
			 (when (= (nearest-objects-n-avail nearest-objects)
				  (nearest-objects-n nearest-objects))
			   (setf nearest-objects (extend-nearest-objects nearest-objects)))
			 (setf (aref (nearest-objects-v nearest-objects)
				     (nearest-objects-n nearest-objects)) this-object)
			 (incf (nearest-objects-n nearest-objects))))))))))
    nearest-objects));; return nearest-objects structure
    







(defun get-nearest-xyobjects (xyobj objgrid dist &optional
			      (dx 0.0d0) (dy 0.0d0) (nearest-objects nil)
			      (ignore-eq nil)
			      (test-func))
  "get the NEAREST-OBJECTS (a structure) in objgrid within dist of
obj's x,y; -- returns struct of type nearest-objects, or uses nearest-objects if it
is NIL -- if no suitable obj object is found, then return NIL - match
must also be within dmag of obj's magnitude - dx and dy are offsets to
add to obj's x,y before finding nearest obj

TEST-FUNC is a function applied to the candidate returned object,
returning T if the candidate is acceptable for a match. It can be
used to provide further object rejection."
  (declare (type objgrid objgrid) 
	   (type xyobj xyobj)
	   (type (or null (function (obj))) test-func)
	   (type double-float dist  dx dy)
	   (optimize (speed 3) (safety #.*safety-setting*))) 
  (let ((nearest-objects (or nearest-objects (make-nearest-objects)))
	(nx 0)
	(ny 0)
	(x   (+ dx (xyobj-x xyobj)))  
	(y   (+ dy (xyobj-y xyobj)))
	(distsqr (* dist dist)))
    (declare (type (or null (unsigned-byte 25)) nx ny)
	     (type nearest-objects nearest-objects)
	     (type double-float  x y distsqr))
    (setf (nearest-objects-n nearest-objects) 0)
    (multiple-value-setq (nx ny) (objgrid-xy->ix-iy x y objgrid))
    (cond ((not nx) ;; out of bounds, return no match
	   nil)
	  (t
	   ;; do the primitive thing of searching all the adjacent bins
	   ;; within index range ix-nsx, ix+nsx, etc
	   (let* ((nsx (dbl-ceiling (/ dist (objgrid-dxbin objgrid))))
		  (nsy (dbl-ceiling (/ dist (objgrid-dybin objgrid))))
		  (nx0 (max (- nx nsx) 0))
		  (nx1 (min (+ nx nsx) (1- (objgrid-nx objgrid))))
		  (ny0 (max (- ny nsy) 0))
		  (ny1 (min (+ ny nsy) (1- (objgrid-ny objgrid)))))
	     (declare (type (unsigned-byte 25) nsx nsy nx0 nx1 ny0 ny1))
	     (loop
	      for ix of-type (unsigned-byte 25) from nx0 to nx1
	      do
	      (loop
	       for iy of-type (unsigned-byte 25) from ny0 to ny1
	       for objlist = (aref (objgrid-bins objgrid) iy ix)
	       do
	       (when objlist
		 (loop for this-object of-type xyobj in objlist
		       do
		       (when  
			   (and  
			    (or (not ignore-eq)
				(not (eq xyobj this-object)))
			    (or (not test-func)
				(funcall test-func this-object))
			    (>= distsqr
				(xydist-sqr x y (xyobj-x this-object)
					    (xyobj-y this-object))))
			 ;;
			 ;; extend nearest-objects if needed
			 (when (= (nearest-objects-n-avail nearest-objects)
				  (nearest-objects-n nearest-objects))
			   (setf nearest-objects 
				 (extend-nearest-objects nearest-objects)))
			 
			 (setf (aref (nearest-objects-v nearest-objects)
				     (nearest-objects-n nearest-objects)) 
			       this-object)
			 (incf (nearest-objects-n nearest-objects))))))))))
    nearest-objects));; return nearest-objects structure
    





(defun get-brightest-nobjects  (objgrid n)
  "return new objgrid structure with just the brightest n objects"
  (cond ((<= n (objgrid-nobj objgrid))
	 objgrid)
	(t
	 (let ((newobjs (copy-structure objgrid)))
	   ;; nobjs will be incremented by insert-object
	   (setf (objgrid-nobj newobjs) 0) 
	   (loop for i from 1 to n
		 for obj in (objgrid-list objgrid)
		 do (insert-object obj newobjs)
		 finally
		 ;; because insert-object PUSHes objgrid
		 (setf (objgrid-list newobjs)  
		       (nreverse (objgrid-list newobjs)))
		 (return newobjs))))))


;; shift objects by dx,dy, and rebin - everything is copied
(defun shift-objects (objgrid dx dy)
  "shift XYOBJ objects in objgrid by dx,dy, and rebin - objects are copied"
  (declare (type objgrid objgrid)
	   (type double-float dx dy)
	   (optimize (speed 3)
		     (safety #.*safety-setting*)
		     (debug  #.*debug-setting*)))
  (loop for obj in (objgrid-list objgrid)
	for new-object of-type xyobj = (copy-structure obj)
	with new-obj-list = nil
	do
	(incf (obj-x new-object) dx)
	(incf (obj-y new-object) dy)
	(push new-object new-obj-list)
	finally
	(return (bin-objects new-obj-list))))
  

(defun clear-objgrid (objgrid)
  "remove objects from an objgrid, but maintain other data"
  (declare (type objgrid objgrid))
  (setf (objgrid-nobj objgrid) 0)
  (setf (objgrid-list objgrid) nil)
  (when (objgrid-bins objgrid)
    (loop
     with bins = (objgrid-bins objgrid)
     for i below (array-dimension bins 0)
     do
     (loop
      for j below (array-dimension bins 1)
      do
      (setf (aref bins i j) nil)))))
	

(defun delete-xyobj-from-objgrid (xyobj objgrid &key (delete-from-list nil))
  "Delete an XYOBJ (or normal OBJ) from an OBJGRID, using EQ as the
test. If DELETE-FROM-LIST is T (default is NIL), then delete it from
OBJGRID-LIST as well; this involves an O(N) list traversal.  The
operation on the list at the bin in question and on OBJGRID-LIST is a
destructive DELETE. Returns T if the object was deleted."
  (declare (type xyobj xyobj)
	   (type objgrid objgrid)
	   (optimize speed))

  (when delete-from-list
    (setf (objgrid-list objgrid)
	  (delete xyobj (the list (objgrid-list objgrid)) :test #'eq)))
  (multiple-value-bind (ix iy)
      (objgrid-xy->ix-iy (xyobj-x xyobj) (xyobj-y xyobj) objgrid)
    (when ix ;; ie, when in-bounds
      (decf (objgrid-nobj objgrid)) ;; note that list will not be OK
      (setf (aref (objgrid-bins objgrid) iy ix)
	    (delete xyobj  
		    (the list (aref (objgrid-bins objgrid) iy ix))
		    :test #'eq))
      T)))
		    
  


(defun benchmark-obj-binning (&key (nx 100) (ny 100) (nobj 30000) (r-match 1d0)  (niter 1))
  "Put NOBJ objects into a NXxNY grid of size 100d0, and find the
   closest object near each one, repeating NITER times with the
   same grid"
  (loop 
     with obj-list = (loop for i below nobj 
			collect (make-xyobj :x (random 100d0) :y (random 100d0)))
     with grid = (bin-objects obj-list :nx nx :ny ny)
     for i below niter
     do
       (loop for obj in obj-list
	    for nearest-obj = (get-nearest-xyobject obj grid r-match 0d0 0d0 t) 
	    when (and nearest-obj (= (xyobj-x nearest-obj) 101d0)) ;; will never happen
	    do (print "FOO! This never happens."))))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OLD VERSIONS IN CASE NEW ONES ARE BROKEN
#|



(defun get-nearest-object-old (obj objgrid dist dmag &optional
			   (dx 0.0d0) (dy 0.0d0) (ignore-eq t)
			   (test-func nil))
  "get the nearest obj object in objgrid within dist of obj's x,y; if
no suitable obj object is found, then return NIL  - match must also be
within dmag of obj's magnitude - dx and dy are offsets to add to obj's x,y
before finding nearest obj 

IGNORE-EQ (T by default) causes us to ignore any object that is EQ to 
OBJ.  This is useful for finding a gridded object's neighbors.

TEST-FUNC is a function applied to the candidate returned object, returning T if
the candidate is acceptable for a match. It can be used to provide further object rejection."
  (declare (type objgrid objgrid)
	   (type obj obj)
	   (type double-float dist dmag dx dy)
	   (optimize (speed 3) (safety #.*safety-setting*) (debug 1))) 

  (when (> (/ dist (min (objgrid-dxbin objgrid)
			(objgrid-dybin objgrid)))
	   1d8)
    (error "DIST is too large - rounding to fixnum will fail. Use something sensible."))

  (let ((nx 0)
	(ny 0)
	(mag (obj-mag obj))
	(x   (+ dx (obj-x obj)))
	(y   (+ dy (obj-y obj)))
	(distsqr (* dist dist)))
    (declare (type (or null (unsigned-byte 25)) nx ny)
	     (type double-float mag x y distsqr))
    (multiple-value-setq (nx ny) (objgrid-xy->ix-iy x y objgrid))
    (cond ((not nx) ;; out of bounds, return no match
	   nil)
	  (t
	   ;; do the primitive thing of searching all the adjacent bins
	   ;; within index range ix-nsx, ix+nsx, etc
	   ;;
	   ;; FIXME - we could improve this a lot by scanning outward from
	   ;; center bin, and stopping search after it is impossible for an 
	   ;; object to improve on current object
	   (let* ((nsx (dbl-ceiling (/ dist (objgrid-dxbin objgrid))))
		  (nsy (dbl-ceiling (/ dist (objgrid-dybin objgrid))))
		  (nx0 (max (- nx nsx) 0))
		  (nx1 (min (+ nx nsx) (1- (objgrid-nx objgrid))))
		  (ny0 (max (- ny nsy) 0))
		  (ny1 (min (+ ny nsy) (1- (objgrid-ny objgrid)))))
	     (declare (type (unsigned-byte 25) nsx nsy nx0 nx1 ny0 ny1))
	     (loop 
		with best-object = nil
		with best-dist of-type double-float = 0.0d0
		for ix of-type (unsigned-byte 25) from nx0 to nx1
		do
		  (loop for iy of-type (unsigned-byte 25) from ny0 to ny1
		     for objlist = (aref (objgrid-bins objgrid) iy ix)
		     do
		       (when objlist
			 (multiple-value-bind (nearest-object mindist)
			     (find-nearest-obj-in-list
			      x y mag distsqr dmag
			      objlist (if ignore-eq obj) 
			      test-func)
			   (declare (type (or null obj) nearest-object)
				    (type double-float mindist))
			   (when (and nearest-object
				      ;; already tested EQ, and TEST-FUNC
				      (or (not best-object)
					  (< mindist best-dist)))
			     (setf best-object nearest-object) 
			     (setf best-dist mindist)))))
		finally (return best-object)))))))



;; a version working on just XY objects
(defun get-nearest-xyobject-old (xyobj objgrid dist &optional
			     (dx 0.0d0) (dy 0.0d0) (ignore-eq t)
			     (test-func nil))
  "get the nearest xyobj object in objgrid within dist of obj's x,y;
if no suitable obj object is found, then return NIL - dx and dy are
offsets to add to obj's x,y before finding nearest obj

IGNORE-EQ (T by default) causes us to ignore any object that is EQ to 
OBJ.  This is useful for finding a gridded object's neighbors.

TEST-FUNC is a function applied to the candidate returned object,
returning T if the candidate is acceptable for a match. It can be
used to provide further object rejection."
  (declare (type objgrid objgrid)
	   (type xyobj xyobj)
	   (type double-float dist dx dy)
	   (optimize (speed 3) (safety #.*safety-setting*) (debug 1))) 

  (when (> (/ dist (min (objgrid-dxbin objgrid)
			(objgrid-dybin objgrid)))
	   1d8)
    (error "DIST is too large - rounding to fixnum will fail. Use something sensible."))

  (let ((nx 0)
	(ny 0)
	(x   (+ dx (xyobj-x xyobj)))
	(y   (+ dy (xyobj-y xyobj)))
	(distsqr (* dist dist)))
    (declare (type (or null (unsigned-byte 25)) nx ny)
	     (type double-float x y distsqr))
    (multiple-value-setq (nx ny) (objgrid-xy->ix-iy x y objgrid))
    (cond ((not nx) ;; out of bounds, return no match
	   nil)
	  (t
	   ;; do the primitive thing of searching all the adjacent bins
	   ;; within index range ix-nsx, ix+nsx, etc
	   ;; FIXME - could improve by searching outward from center
	   (let* ((nsx (dbl-ceiling (/ dist (objgrid-dxbin objgrid))))
		  (nsy (dbl-ceiling (/ dist (objgrid-dybin objgrid))))
		  (nx0 (max (- nx nsx) 0))
		  (nx1 (min (+ nx nsx) (1- (objgrid-nx objgrid))))
		  (ny0 (max (- ny nsy) 0))
		  (ny1 (min (+ ny nsy) (1- (objgrid-ny objgrid)))))
	     (declare (type (unsigned-byte 25) nsx nsy nx0 nx1 ny0 ny1))
	     (loop 
		with best-object = nil
		with best-dist of-type double-float = 0.0d0
		for ix of-type (unsigned-byte 25) from nx0 to nx1
		do
		  (loop for iy of-type (unsigned-byte 25) from ny0 to ny1
		     for objlist = (aref (objgrid-bins objgrid) iy ix)
		     do
		       ;(format t "method1: ix=~A iy=~A~%" ix iy)
		       (when objlist
			 (multiple-value-bind (nearest-object mindist)
			     (find-nearest-xyobj-in-list
			      x y distsqr 
			      objlist (if ignore-eq xyobj)
			      test-func)
			   (declare (type (or null xyobj) nearest-object)
				    (type double-float mindist))
			   ;(print (list :meth1 ix iy x y distsqr objlist))
			   ;(terpri)
			   (when (and nearest-object
				      ;; we already tested EQ, and TEST-FUNC
				      (or (not best-object)
					  (< mindist best-dist)))
			     (setf best-object nearest-object) 
			     (setf best-dist mindist)))))
		finally (return best-object)))))))
			      
(defun benchmark-obj-binning-old (&key (nx 100) (ny 100) (nobj 30000) (r-match 1d0)  (niter 1))
  "Put NOBJ objects into a NXxNY grid, and find the clostest object near each one, repeating NITER times
   with the same grid"
  (loop 
     with obj-list = (loop for i below nobj 
			collect (make-xyobj :x (random 100d0) :y (random 100d0)))
     with grid = (bin-objects obj-list :nx nx :ny ny)
     for i below niter
     do
       (loop for obj in obj-list
	    for nearest-obj = (get-nearest-xyobject-old obj grid r-match 0d0 0d0 t) 
	    when (and nearest-obj (= (xyobj-x nearest-obj) 101d0)) ;; will never happen
	    do (print "FOO!"))))


			  
(defun benchmark-obj-binning-compare-versions (&key (nx 100) (ny 100) (nobj 30000) (r-match 1d0) (niter 1))
  "Put NOBJ objects into a NXxNY grid, and find the clostest object near each one, repeating NITER times
   with the same grid - SEE IF NEW AND OLD VERSIONS OF GET-NEAREST-OBJECTS RETURNS SAME ANSWER"
  (loop 
     with obj-list = (loop for i below nobj 
			collect (make-xyobj :x (random 100d0) :y (random 100d0)))
     with grid = (bin-objects obj-list :nx nx :ny ny)
     for i below niter
     do
       (loop for obj in obj-list
	    for nearest-obj = (get-nearest-xyobject obj grid r-match 0d0 0d0 t) 
	    for nearest-obj2 = (get-nearest-xyobject-old obj grid r-match 0d0 0d0 t) 
	    when (not (eq nearest-obj nearest-obj2))
	    do (error "NOT EQUAL obj: ~A  obj2: ~A" nearest-obj nearest-obj2)
	    do (progn "stop"))))


|#
