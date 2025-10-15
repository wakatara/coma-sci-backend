#|

Define the idea of a 'generic image' which is a value, a 2d image like a[iy][ix],  or a function (func iy ix)

Then GENERIC-IMREF gets the value of a generic image at IY,IX.

This allows, for example, mapping a sub-image from a sub-image, or creating a psf as a function.

The functions IMAGE-TO-IMFUNC and FLAG-IMAGE-TO-FLAG-IMFUNC convert
images and flag images to their functional forms, allowing default
values for out of bounds, and 


|#

(in-package imutils)

;; a function that maps (FUNC IY IX) --> single-float, representing a functional form of an image
(deftype im-func ()
  `(function (simindex simindex) single-float))

(deftype flagim-func ()
  `(function (simindex simindex) imflag))


;; a function that maps (FUNC X Y) --> single-float, representing a map from fractional  pixels
(deftype floatpix-im-func ()
  `(function (single-float single-float) single-float))


;; a generic idea of an image: a single float value, an array, or (FUNC IY IX)
(deftype generic-image ()
  `(or float image im-func))

(deftype generic-flag-image ()
  `(or imflag flag-image flagim-func))


(declaim (inline generic-imref))
(declaim (inline generic-flg-imref))

(defun generic-imref (im iy ix)
  "Access a GENERIC-IMAGE at IY,IX"
  (declare (type generic-image im)
	   (type simindex iy ix)
	   (optimize speed))
  (cond ((floatp im)  im)  ;; this is reasonably fast - could find no further optimizations
	((arrayp im)  (aref    im iy ix))
	(t            (funcall im iy ix))))

(defun generic-flag-imref (fim iy ix)
  "Access a GENERIC-FLAG-IMAGE at IY,IX"
  (declare (type generic-flag-image fim)
	   (type simindex iy ix)
	   (optimize speed))
  (cond ((integerp fim)  fim)  ;; this is reasonably fast - could find no further optimizations
	((arrayp fim)    (aref    fim iy ix))
	(t               (funcall fim iy ix))))



(defun image-to-im-func (im &key xmin ymin xmax ymax outside-val)
  "Convert an image to an (IM-FUNC IX IY).

If XMIN,YMIN,XMAX,YMAX are specfied then return an offset image with
0,0 mapping to XMIN,YMIN are in original.   Any of *MIN,*MAX not specified
default to the original array dimensions.

If OUTSIDE-VAL is specified then this value is given for points outside the range.

If RETURN-DIMS are specified then calling as (FUNC x x :NROWS)
or (FUNC x x :NCOLS) returns the number of rows or cols."

  (declare (type image im)
	   (type (or null imindex) xmin ymin xmax ymax)
	   (type (or null single-float) outside-val))

  (let ((x0 (or xmin 0))
	(x1 (1- (or xmax (array-dimension im 1))))
	(y0 (or ymin 0))
	(y1 (1- (or ymax (array-dimension im 0)))))

    (when (or (>= x1 (array-dimension im 1))
	      (>= y1 (array-dimension im 0))
	      (< x1 x0)
	      (< y1 y0))
      (error "Requested bounds XMIN=~A XMAX=~A, YMIN=~A YMAX=~A not valid for array of dimension ~A"
	     xmin xmax ymin ymax (array-dimensions im)))      
    
    (cond
      ;; simple version
      ((not (or xmin xmax ymin ymax outside-val))
       (lambda (iy ix)
	 (declare (type imindex iy ix)
		  (optimize speed)) 
	 (aref im iy ix)))
      ;;
      ;; offset array version, or having OUTSIDE-VAL
      (t
       (lambda (iy ix)
	 (declare (type simindex iy ix)
		  (optimize speed))
	 (let ((jx (+ ix xmin))  ;; indices in original image
	       (jy (+ ix ymin)))
	   ;; when out bounds, return outside-val if present, or throw error
	   (cond ((not (and (<= x0 jx x1)
			    (<= y0 jy y1)))
		  (or outside-val
		      (error "Out of bounds in offset array: Offset indices IY=~A IX=~A map to original JY=~A IY=~A in array that has array-dimensions ~A" iy ix jy jx (array-dimensions im))))
		 ;;
		 (t
		  (locally
		      (declare (optimize (speed 3) (safety 0)) );; already checked bounds
		    (aref im iy ix)))))))
       )))		  



(defun flag-image-to-flag-im-func (fim &key xmin ymin xmax ymax outside-val)
  "Convert a flag image to an (FLAG-IM-FUNC IX IY).

If XMIN,YMIN,XMAX,YMAX are specfied then return an offset image with
0,0 mapping to XMIN,YMIN are in original.   Any of *MIN,*MAX not specified
default to the original array dimensions.

If OUTSIDE-VAL is specified then this value is given for points outside the range.

If RETURN-DIMS are specified then calling as (FUNC x x :NROWS)
or (FUNC x x :NCOLS) returns the number of rows or cols."

  (declare (type flag-image fim)
	   (type (or null imindex) xmin ymin xmax ymax)
	   (type (or null imflag) outside-val))

  (let ((x0 (or xmin 0))
	(x1 (1- (or xmax (array-dimension fim 1))))
	(y0 (or ymin 0))
	(y1 (1- (or ymax (array-dimension fim 0)))))

    (when (or (>= x1 (array-dimension fim 1))
	      (>= y1 (array-dimension fim 0))
	      (< x1 x0)
	      (< y1 y0))
      (error "Requested bounds XMIN=~A XMAX=~A, YMIN=~A YMAX=~A not valid for array of dimension ~A"
	     xmin xmax ymin ymax (array-dimensions fim)))      
    
    (cond
      ;; simple version
      ((not (or xmin xmax ymin ymax outside-val))
       (lambda (iy ix)
	 (declare (type imindex iy ix)
		  (optimize speed)) 
	 (aref fim iy ix)))
      ;;
      ;; offset array version, or having OUTSIDE-VAL
      (t
       (lambda (iy ix)
	 (declare (type simindex iy ix)
		  (optimize speed))
	 (let ((jx (+ ix xmin))  ;; indices in original image
	       (jy (+ ix ymin)))
	   ;; when out bounds, return outside-val if present, or throw error
	   (cond ((not (and (<= x0 jx x1)
			    (<= y0 jy y1)))
		  (or outside-val
		      (error "Out of bounds in offset array: Offset indices IY=~A IX=~A map to original JY=~A IY=~A in array that has array-dimensions ~A" iy ix jy jx (array-dimensions fim))))
		 ;;
		 (t
		  (locally
		      (declare (optimize (speed 3) (safety 0)) );; already checked bounds
		    (aref fim iy ix)))))))
       )))		  


