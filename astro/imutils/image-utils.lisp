

(in-package imutils)



(in-package imutils)

(deftype image ()
  '(simple-array single-float (* *)))

(deftype double-image () ;; used rarely, interally for higher precision
  '(simple-array double-float (* *)))

(deftype image-or-null ()
  '(or null image))

;; for fourier analysis
(deftype complex-image ()
  '(simple-array (complex single-float) (* *)))

;; for fourier analysis
(deftype double-complex-image ()
  '(simple-array (complex double-float) (* *)))

;; a flag image of integers to indicate bad pixels, used in interpolating
(deftype imflag ()
  '(unsigned-byte 16))
(deftype flag-image ()
  '(simple-array (unsigned-byte 16) (* *)))
(deftype flag-image-or-null ()
  '(or null flag-image))
(deftype bit-image ()
  '(simple-array (unsigned-byte 1) (* *)))
(deftype bit-image-or-null ()
  '(or null bit-image))

(deftype any-type-image ()
  '(or image double-image complex-image double-complex-image
    bit-image flag-image))

;; eg 'IMAGE -> 'SINGLE-FLOAT
(defun get-array-element-type-for-image-type (image-type)
  (cond ((eq image-type 'image) 'single-float)
	((eq image-type 'double-image) 'double-float)
	((eq image-type 'complex-image) '(complex single-float))
	((eq image-type 'double-complex-image) '(complex double-float))
	((eq image-type 'bit-image)  '(unsigned-byte 1))
	((eq image-type 'flag-image) 'imflag)))


;; image index type
(deftype imindex ()
  '(integer 0 #.(ash most-positive-fixnum -1))) 
;; SIGNED image index type
(deftype simindex ()
  '(integer
    #.(ash most-negative-fixnum -1)
    #.(ash most-positive-fixnum -1)))

(deftype floatvec ()
  `(simple-array single-float (*)))


(defun make-image (ny nx &key (initial-value 0.0))
  "Create a floating point image of size NY,NX with INITIAL-VALUE"
  (make-array (list ny nx) :element-type 'single-float
			   :initial-element initial-value))

(defun make-double-image (ny nx &key (initial-value 0d0))
  "Create a floating point image of size NY,NX with INITIAL-VALUE"
  (make-array (list ny nx) :element-type 'double-float
			   :initial-element initial-value))

(defun make-same-size-image (a &key (initial-value 0.0))
  "Make an image of the same dimensions as original image"
  (declare (type (simple-array * (* *)) a))
  (make-image (array-dimension a 0) (array-dimension a 1)
	      :initial-value (float initial-value 1.0)))

(defun make-flag-image (ny nx &key (initial-value 0))
  "Create an integer flag image of size NY,NX with INITIAL-VALUE"
  (make-array (list ny nx) :element-type '(unsigned-byte 16)
			   :initial-element initial-value))

(defun make-same-size-flag-image (a &key (initial-value 0))
  "Make an image of the same dimensions as original image"
  (declare (type (simple-array * (* *)) a))
  (make-flag-image (array-dimension a 0) (array-dimension a 1)
		   :initial-value initial-value))

(defun make-bit-image (ny nx &key (initial-value 0))
  "Create an integer flag image of size NY,NX with INITIAL-VALUE"
  (make-array (list ny nx) :element-type '(unsigned-byte 1)
			   :initial-element initial-value))

(defun make-same-size-bit-image (a &key (initial-value 0))
  "Make an image of the same dimensions as original image"
  (declare (type (simple-array * (* *)) a))
  (make-bit-image (array-dimension a 0) (array-dimension a 1)
		   :initial-value initial-value))


(defun make-complex-image (ny nx &key (initial-value #C(0.0 0.0)))
  "Create an integer flag image of size NY,NX with INITIAL-VALUE"
  (make-array (list ny nx) :element-type '(complex single-float)
			   :initial-element initial-value))

(defun make-double-complex-image (ny nx &key (initial-value #C(0d0 0d0)))
  "Create an integer flag image of size NY,NX with INITIAL-VALUE"
  (make-array (list ny nx) :element-type '(complex double-float)
			   :initial-element initial-value))

(defun make-same-size-complex-image (a &key (initial-value #C(0.0 0.0)))
  "Create an integer flag image of size NY,NX with INITIAL-VALUE"
  (declare (type (simple-array * (* *)) a))
  (make-complex-image (array-dimension a 0) (array-dimension a 1)
		      :initial-value initial-value))

;; duplicate an image in size, with no copying of elements
(defun %dup-image-nocopy (a) 
  (declare (type image a))
  (the image
    (make-array (array-dimensions a) :element-type 'single-float)))

(defun image-sizes-match-p (im1 im2)
  (declare (type (simple-array * (* *)) im1 im2))
  (and (= (array-dimension im1 0) (array-dimension im2 0))
       (= (array-dimension im1 1) (array-dimension im2 1))))
	  


(defmacro image-iterate ((im iyvar ixvar &key ix0 ix1 iy0 iy1
					   return-block-name) &body body)
  "Iterate on an image from IM[ix0..ix1][iy0..iy1], defaulting to the entire
array, limiting IX0,IX1,etc to be in valid range.

If RETURN-BLOCK-NAME is set, then the iteration code is wrapped in a
block (BLOCK return-block-name (looping-code ....) to allow convenient
mid-loop escape."

  (let ((%ix0var (gensym "ix0-"))
	(%ix1var (gensym "ix1-"))
	(%iy0var (gensym "iy0-"))
	(%iy1var (gensym "iy1-")))
    `(,@(if return-block-name (list 'block return-block-name) (list 'progn))
      (let* ((%im ,im)
	     (nx (array-dimension %im 1))
	     (ny (array-dimension %im 0))
	     ;; if IX0... is NIL, then just set to zero - otherwise
	     ;; evaluate IX0 in the expanded code, because it could
	     ;; still EVALUATE to nil at runtime
	     (,%ix0var ,(if ix0 `(max 0 (or ,ix0 0)) 0))
	     (,%iy0var ,(if iy0 `(max 0 (or ,iy0 0)) 0))
	     (,%ix1var ,(if ix1 `(min (1- nx) (or ,ix1 (1- nx)))
			    '(1- nx)))
	     (,%iy1var ,(if iy1 `(min (1- ny) (or ,iy1 (1- ny)))
			    '(1- ny))))
	
	(declare (type imindex ,%ix0var ,%iy0var ,%ix0var ,%iy0var))
	(loop for ,iyvar of-type imindex from ,%iy0var to ,%iy1var
	      do (loop for ,ixvar of-type imindex from ,%ix0var to ,%ix1var
		       do (progn ,@body)))))))


(defmacro image-iterate/subpixel ((im
				   yvar xvar
				   nybin nxbin
				   &key iyvar ixvar 
				     ix0 ix1 iy0 iy1
				     return-block-name
				     (pixel-center 0.0)
				     pixel-completion-form)
				  &body body)
  "Iterate on an image subpixels from IM[ix0..ix1][iy0..iy1],
defaulting to the entire array, limiting IX0,IX1,etc to be in valid range.

XVAR,YVAR are the variables for the subpixel coordinates.
NYBIN,NXBIN are the binning factors in the Y,X directions.

NOTE THE ORDER!! - Y (row) is always first, and X (column) is always
second.

PIXEL-CENTER (default 0.0) means that the center of the iy,ix pixel is
at y=(iy+PIXEL-CENTER),x=(ix+PIXEL_CENTER).  Thus the default is that
pixels are centered on their integer index, and extend [-0.5,+0.5]
around it.  An alternative might be PIXEL-CENTER=0.5, which means a pixel
starts at its iy,ix index, and ends at [1+,+1]

IYVAR,IXVAR are optional integer variables representing the coordiantes
of the current pixel.

PIXEL-COMPLETION-FORM is an optional expression that is evaluated when
finished iterating over each pixel.  It will see IYVAR, IXVAR but
not YVAR,XVAR.

If RETURN-BLOCK-NAME is set, then the iteration code is wrapped in a
block (BLOCK return-block-name (looping-code ....) to allow convenient
mid-loop escape."

  (let ((%ix0var (gensym "ix0-"))
	(%ix1var (gensym "ix1-"))
	(%iy0var (gensym "iy0-"))
	(%iy1var (gensym "iy1-"))
	;; integer variables running over of pixels
	(%ixvar (or ixvar (gensym "ix-")))
	(%iyvar (or iyvar (gensym "iy-")))
	;; subpix binning vars
	(%nxbinvar (gensym "nxbin-"))
	(%nybinvar (gensym "nybin-"))
	(%xbinsize (gensym "xbinsize-"))
	(%ybinsize (gensym "ybinsize-"))
	(%ixbinvar (gensym "ixbin-"))
	(%iybinvar (gensym "iybin-"))
	;; starting values within a pixel
	(%ixbstart (gensym "ixbstart-"))
	(%iybstart (gensym "iybstart-")))
    `(,@(if return-block-name (list 'block return-block-name) (list 'progn))
      (let* ((%im ,im)
	     (nx (array-dimension %im 1))
	     (ny (array-dimension %im 0))
	     ;;
	     (,%nxbinvar ,nxbin)
	     (,%nybinvar ,nybin)
	     (,%xbinsize (/ 1.0 ,%nxbinvar))
	     (,%ybinsize (/ 1.0 ,%nybinvar))
	     ;; if IX0... is NIL, then just set to zero - otherwise
	     ;; evaluate IX0 in the expanded code, because it could
	     ;; still EVALUATE to nil at runtime
	     (,%ix0var ,(if ix0 `(max 0 (or ,ix0 0)) 0))
	     (,%iy0var ,(if iy0 `(max 0 (or ,iy0 0)) 0))
	     (,%ix1var ,(if ix1 `(min (1- nx) (or ,ix1 (1- nx)))
			    '(1- nx)))
	     (,%iy1var ,(if iy1 `(min (1- ny) (or ,iy1 (1- ny)))
			    '(1- ny)))
	     (,%ixbstart (+ -0.5 ,pixel-center (* ,%xbinsize 0.5)))
	     (,%iybstart (+ -0.5 ,pixel-center (* ,%ybinsize 0.5))))
	(declare (type imindex ,%ix0var ,%iy0var ,%ix0var ,%iy0var
		       ,%nybinvar ,%nxbinvar)
		 (type single-float ,%xbinsize ,%ybinsize))
	(loop for ,%iyvar of-type imindex from ,%iy0var to ,%iy1var
	      do (loop for ,%ixvar of-type imindex from ,%ix0var to ,%ix1var
		       do
			  ;; subpixel y loop
			  (loop for ,%iybinvar of-type (unsigned-byte 20) from
				1 to ,%nybinvar
				for ,yvar of-type single-float
				  = (+ ,%iyvar ,%iybstart) then (+ ,yvar ,%ybinsize)
				do
				   ;; subpixel x loop
				   (loop for ,%ixbinvar of-type (unsigned-byte 20)
					 from 1 to ,%nxbinvar
					 for ,xvar of-type single-float
					   = (+ ,%ixvar ,%ixbstart) then (+ ,xvar ,%xbinsize)
					 do
					    (progn ,@body)))
			;; optional form evaluated after full pixel is completed
		       ,@(if pixel-completion-form (list pixel-completion-form))))))))
			  





(defun subimage (im ix0 iy0 ix1 iy1)
  "Grab a subimage IX0,IY0 to IX1,IY1 of any type of image - must be within array bounds."
  (declare (type (simple-array * (* *)) im)
	   (type fixnum ix0 iy0 ix1 iy1))
  (when (not (and (< -1 ix0 (array-dimension im 1))
		  (< -1 ix1 (array-dimension im 1))
		  (< -1 iy0 (array-dimension im 0))
		  (< -1 iy1 (array-dimension im 0))
		  (<= ix0 ix1)
		  (<= iy0 iy1)))
    (error "Dimensions IX0=~A IY0=~A IX1=~A IY1=~A not a valid subimage of an array with dimensions ~A"
	   ix0 iy0 ix1 iy1 (array-dimensions im)))

  (flet ((%subimage ()
	   (let ((imout (make-array
			 (list (1+ (- iy1 iy0))
			       (1+ (- ix1 ix0)))
			 :element-type (array-element-type im))))
	     (loop for jx of-type fixnum from 0
		   for ix  of-type fixnum from ix0 to ix1
		   do (loop for jy of-type fixnum from 0
			    for iy of-type fixnum from iy0 to iy1
			    do (setf (aref imout jy jx) (aref im iy ix))))
	     imout)))
    (declare (inline %subimage)
	     (optimize speed))
    ;; hope the optimized version of %subimage is inlined for each case
    (cond ((typep im 'image) (%subimage))
	  ((typep im 'flag-image) (%subimage))
	  ((typep im 'complex-image) (%subimage))
	  ((typep im 'double-complex-image) (%subimage))
	  ((typep im 'bit-image) (%subimage))
	  ((typep im '(simple-array * (* *))) (%subimage)))))


(defun extract-image-around-position (im x0 y0 nx ny &key (out-of-bounds-value nil) (output-image nil))
  "Extract an image IM of any type into an image of size NX,NY (which must be odd),
so that the point at X0,Y0 is in (ash nx -1), (ash ny -1), which, for
odd NX,NY is the center pixel.

OUTPUT-IMAGE, if specified, is used for output, but must be of correct dimensions and type.

It is possible to extract pixels outside IM, in which case OUT-OF-BOUNDS-VALUE is
used to fill in the image; by default zero is used, otherwise OUT-OF-BOUNDS-VALUE.

OUT-OF-BOUNDS-VALUE must be appropriate for the image type.

Returns (VALUES EXTRACTED-IMAGE X0-NEW Y0-NEW NUMBER-OF-PIX-OUT-OF-BOUNDS)"
  (declare (type (simple-array * (* *)) im)
	   (type (or null  (simple-array * (* *))) output-image)
	   (type single-float x0 y0)
	   (type imindex nx ny))

  (when (and output-image (not (equal (array-element-type output-image)
				      (array-element-type im))))
    (error "OUTPUT-IMAGE is of type ~A but IM is of type ~A; must be of same type"
	   (array-element-type output-image) (array-element-type im)))
  
  (when (and out-of-bounds-value
	     (not (subtypep (type-of out-of-bounds-value) (array-element-type im))))
    (error "OUT-OF-BOUNDS-VALUE of type ~A is not a subtype of the type of IM, which is ~A"
	   (type-of out-of-bounds-value) (array-element-type im)))
    
  
  (let* ((imtype (array-element-type im))
	 (oobv (or out-of-bounds-value
		   (coerce 0 imtype)))
	 (imout (or output-image (make-array (list ny nx) :element-type imtype)))
	 (ix0 (floor x0))
	 (iy0 (floor y0))
	 (ixmax (1- (array-dimension im 0)))
	 (iymax (1- (array-dimension im 1)))
	 (num-out-of-bounds 0))

    (declare (type imindex ix0 iy0 ixmax iymax num-out-of-bounds))

    (flet ((%copy-data (%im %imout %oobv)
	     (loop
	       ;; index in IM
	       with ix-final = (if (oddp nx) (+ ix0 (ash nx -1))  (+ -1  (+ ix0 (ash nx -1))))
	       for ix of-type simindex from (- ix0 (ash nx -1)) to ix-final
	       ;; index in IMOUT
	       for jx of-type simindex from 0
	       do
		  (loop
		    ;; index in IM
		    with iy-final = (if (oddp ny) (+ iy0 (ash ny -1))  (+ -1  (+ iy0 (ash ny -1))))
		    for iy of-type simindex from (- iy0 (ash ny -1)) to iy-final
		    ;; index in IMOUT
		    for jy of-type simindex from 0
		    do
		       (if (and (<= 0 ix ixmax)
				(<= 0 iy iymax))
			   (setf (aref %imout jy jx) (aref %im iy ix)) ;; in bounds
			   (progn
			     (incf num-out-of-bounds)
			     (setf (aref %imout jy jx) %oobv))))))) ;; out of bounds

      (cond ((typep im 'image)
	     (%copy-data (the image im) (the image imout) (the single-float oobv)))
	    ((typep im 'flag-image)
	     (%copy-data (the flag-image im) (the flag-image imout) (the imflag oobv)))
	    ((typep im 'complex-image)
	     (%copy-data (the complex-image im) (the complex-image imout)
			 (the (complex single-float) oobv)))
	    ((typep im 'double-complex-image)
	     (%copy-data (the double-complex-image im) (the double-complex-image imout)
			 (the (complex double-float) oobv)))
	    ((typep im 'bit-image)
	     (%copy-data (the bit-image im) (the bit-image imout) (the bit oobv)))
	    ((typep im '(simple-array * (* *)))
	     (%copy-data im imout oobv))))

      (values imout
	      (+ (nth-value 1 (floor x0))  (ash nx -1))
	      (+ (nth-value 1 (floor y0))  (ash ny -1))
	      num-out-of-bounds)))
  
  

  


(defun image-row (im irow &key imin imax)
  "Extract a row of an image as a single-float vector, optionally from
column indices IMIN to IMAX"
  (declare (type image im)
	   (type (unsigned-byte 30) irow)
	   (type (or null (unsigned-byte 30)) imin imax))
  (let* ((nmax (1- (array-dimension im 1))) ;; direction of extraction
	 (mmax (1- (array-dimension im 0))) 
	 (imin (or imin 0))
	 (imax (or imax nmax)))
    (when (or (> imin imax)
	      (> imax nmax))
      (error "Cannot extract row in index range [~A,~A] in array with columns spanning [0,~A]"
	     imin imax nmax))
    (when (> irow mmax)
      (error "Cannot extract row ~A in array with columns spanning [0,~A]" irow mmax))
    (loop with vout of-type (simple-array single-float (*))
	    = (make-array (1+ (- imax imin)) :element-type 'single-float)
	  ;;;initially (describe vout)
	  for i of-type fixnum from imin to imax
	  for j of-type fixnum from 0
	  do
	    ;;(format t "vout[~A]=im[~A][~A]~%" j i irow)
	     (setf (aref vout j) (aref im irow i))
	     finally (return vout))))

(defun image-column (im icol &key imin imax)
  "Extract a column of an image as a single-float vector, optionally from
row indices IMIN to IMAX"
  (declare (type image im)
	   (type (unsigned-byte 30) icol)
	   (type (or null (unsigned-byte 30)) imin imax))
  (let* ((nmax (1- (array-dimension im 0))) ;; direction of extraction
	 (mmax (1- (array-dimension im 1))) 
	 (imin (or imin 0))
	 (imax (or imax nmax)))
    (when (or (> imin imax)
	      (> imax nmax))
      (error "Cannot extract column in index range [~A,~A] in array with columns spanning [0,~A]"
	     imin imax nmax))
    (when (> icol mmax)
      (error "Cannot extract column ~A in array with rows spanning [0,~A]" icol mmax))
    (loop with vout of-type (simple-array single-float (*))
	    = (make-array (1+ (- imax imin)) :element-type 'single-float)
	  for i of-type fixnum from imin to imax
	  for j of-type fixnum from 0
	  do (setf (aref vout j) (aref im i icol))
	     finally (return vout))))
	 
		    


;; macro to do solve Ax=b, returning (values x1 x2)
(defmacro 2x2-lin-solve-macro (a11 a12 a21 a22 b1 b2)
  "macro to solve 2x2 linear equation A*x=b, returning (values x1 x2)"
  (let ((det-var (gensym "det-")))
    `(let ((,det-var (- (* ,a11 ,a22) (* ,a12 ,a21))))
       (values
        (/ (- (* ,a22 ,b1) (* ,a12 ,b2)) ,det-var)
        (/ (- (* ,a11 ,b2) (* ,a21 ,b1)) ,det-var)))))


(defun image-to-double-image (im)
  (declare (type image im))
  (let ((imd (make-double-image
	      (array-dimension im 0) (array-dimension im 1))))
    (loop for i of-type fixnum below (array-total-size im)
	  do (setf (row-major-aref imd i)
		   (float (row-major-aref im i) 1d0)))
    imd))

(defun double-image-to-image (imd)
  (declare (type double-image imd))
  (let ((im (make-image
	      (array-dimension imd 0) (array-dimension imd 1))))
    (loop for i of-type fixnum below (array-total-size imd)
	  do (setf (row-major-aref im i)
		   (float (row-major-aref imd i) 1e0)))
    im))
