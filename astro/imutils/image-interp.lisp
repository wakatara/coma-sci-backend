#|

Image interpolation - returns (values interpolated-value inside?)

  (nearest-pixel-interpolate-image data x y)
  (linear-interpolate-image data x y)
  (lanczos2-interpolate-image data x y)    
  (lanczos3-interpolate-image data x y)    

There are also versions that LOGIOR combine a 32 bit flag image in parallel,
and return (values interpolated-value logior-of-flags inside?).


  (nearest-pixel-interpolate-image-with-flags 
     data flag-image-or-null x y)
  (linear-interpolate-image-with-flags 
     data flag-image-or-null x y)
  (lanczos2-interpolate-image-with-flags 
     data flag-image-or-null x y)
  (lanczos3-interpolate-image-with-flags 
     data flag-image-or-null x y)
|#




(in-package imutils)



#|

Note that these routines compute (ARRAY-DIMENSION DATA N) on every
call.  It might seem faster to pass the dimensions from the caller,
where they could be computed just once, but in practice ARRAY-DIMENSION
is a tiny bit faster.  It is also simpler.

|#

;; a type for allowed interp methods
(deftype interp-method-type ()
  `(member :nearest :linear :lanczos2 :lanczos3 :lanczos4))

(declaim (inline nearest-pixel-interpolate-image-with-flags)
	 (ftype (function (image (or null flag-image) single-float single-float)
			  (values single-float (or NULL T) imflag))
		nearest-pixel-interpolate-image-with-flags))

(defun nearest-pixel-interpolate-image-with-flags (data flag x y)
  "Nearest-pixel interpolate an image at pixel position DATA[Y,X],
returning (LOGIOR FLAGS) as 2nd value and T if x,y are inside image as
third value

Valid image data exists between X=[-0.5,NX-0.5] and Y=[-0.5:NY-0.5]
."

  (declare (type image data)
	   (type (or null flag-image) flag)
	   (type (single-float -1e6 1e6) x y)
	   (optimize speed))
  (let* ((nx (array-dimension data 1))
	 (ny (array-dimension data 0))
	 (ix (round x))
	 (iy (round y))
	 (retval 0.0)
	 (flagval 0)
	 (inside? nil))
    (declare (type (signed-byte 24) nx ny ix iy))
    ;; a bit convoluted to prevent boxing :(
    (when (and (>= ix 0) (< ix nx)
	       (>= iy 0) (< iy ny))
	   (setf retval (aref data iy ix))
	   (when flag (setf flagval (aref flag iy ix)))
	   (setf inside? t))
    ;;
    (values retval inside? flagval)))


(declaim (inline nearest-pixel-interpolate-image)
	 (ftype (function (image single-float single-float)
			  (values single-float (or NULL T) imflag))
		nearest-pixel-interpolate-image))

(defun nearest-pixel-interpolate-image (data x y)
  "Nearest-pixel interpolate an image at pixel position DATA[Y,X],
returning T as 2nd value if x,y are inside image, otherwise returning
NIL."
  (declare (type image data)
	   (type single-float x y))
  (nearest-pixel-interpolate-image-with-flags data nil x y))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 

(declaim (inline linear-interpolate-image-with-flags)
	 (ftype (function (image (or null flag-image) single-float single-float)
			  (values single-float (or NULL T) imflag))
		linear-interpolate-image-with-flags))	

(defun linear-interpolate-image-with-flags (data flags x y)
  "Linearly interpolate the pixel value in an image DATA at pixel
position DATA[Y,X], returning (LOGIOR FLAGS) as as 2nd value and T as
third value if x,y are inside image. The convention is that x=0 refers
to the CENTER of the first pixel. The edge cases are handled by
continuing the image at its edge pixel value by one pixel: eg
DATA[Y,X=-1,y]=DATA[Y,0].  Thus valid image data exist between
X=[-0.5,NX-0.5] and Y=[-0.5,NY-0.5]."
  (declare (type image data)
	   (type (or null flag-image) flags)
	   (type (single-float -1e6 1e6) x y)
	   (optimize speed))
  (let* ((nx (array-dimension data 1))
	 (ny (array-dimension data 0))
	 (ix 0) (iy 0) (ix+1 0) (iy+1 0) 
	 (retval 0.0) (flagval 0)
	 (inside? ;; x,y are pixel centers so image extends from [-0.5,nx-0.5]
	   (and (>= x -0.5) (<= x (- nx 0.5))
		(>= y -0.5) (<= y (- ny 0.5))))
	 (fx 0.0) (fy 0.0))
    
    (declare (type (signed-byte 24) nx ny ix iy ix+1 iy+1)
	     (type imflag flagval)
	     (type (float 0.0 1.0) fx fy)
	     (type float retval))
    ;;
    (multiple-value-setq (ix fx) (the (signed-byte 28) (floor x)))
    (setf ix+1 (1+ ix))
    (multiple-value-setq (iy fy)  (the (signed-byte 28) (floor y)))
    (setf iy+1 (1+ iy))

    ;; this isn't necessary because we already checked
    ;; -0.5 < x <nx-0.5 but let's be paranoid about some
    ;; float roundoff effect and double check
    (when (or (< ix -1) (> ix (- nx 1))
	      (< iy -1) (> iy (- ny 1)))
      (setf retval 0.0)
      (setf inside? nil))
      

    ;; get values to interpolate, including continuation of edge pixels
    
    (when inside?
      (cond ((and (>= ix 0) (<= ix (- nx 2)))
	     t) 
	    ((= ix -1)
	     (setf ix 0))
	    ((= ix (- nx 1))
	     (setf ix+1 (- nx 1))))

      (cond ((and (>= iy 0) (<= iy (- ny 2)))
	     t) 
	    ((= iy -1)
	     (setf iy 0))
	    ((= iy (- ny 1))
	     (setf iy+1 (- ny 1))))
      
      (setf retval
	      (+
	       (* (aref data iy ix)       (- 1.0 fx)  (- 1.0 fy))
	       (* (aref data iy+1 ix)     (- 1.0 fx)  fy)
	       (* (aref data iy ix+1)     fx          (- 1.0 fy))
	       (* (aref data iy+1  ix+1)  fx          fy)))
	;;
	(when flags
	  (setf flagval
		(logior 
		 (aref flags iy ix) 
		 (aref flags iy+1 ix) 
		 (aref flags iy ix+1)
		 (aref flags iy+1  ix+1) ))))
      ;;
      (values retval inside? flagval))) ;; return 0 as imflag for consistency



(declaim (inline linear-interpolate-image)
	 (ftype (function (image single-float single-float)
			  (values single-float (or NULL T) imflag))
		linear-interpolate-image))


(defun linear-interpolate-image (data x y)
  "Linearly interpolate the pixel value in an image DATA at pixel
position DATA[Y,X], returning T as 2nd value if inside, otherwise
returning NIL. The convention is that x=0 refers to the CENTER of the
first pixel. The edge cases are handled by continuing the image at its
edge pixel value by one pixel: eg DATA[Y,X=-1,y]=DATA[Y,0].  Thus
valid image data exists between X=[-0.5,NX-0.5] and Y=[-0.5,NY-0.5]."
  (declare (type image data)
	   (type single-float x y))
  (linear-interpolate-image-with-flags data nil x y))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    
(declaim (inline %make-lanczos2-kernel))
;;
(defun %make-lanczos2-kernel (pos) ;; cribbed from swarp/interpolate.c
  (declare (type (single-float -1.0 1.0) pos))
  ;; swarp assumes pos is in [0,1] so generalize to [-1,1]
  (when (minusp pos) (setf pos (+ 1.0 pos)))
  (cond ((<  pos 1e-5) 
	 (values 0.0 1.0 0.0 0.0))
	(t
	 (let* ((pi/2 #.(float (/ pi 2) 1.0))
		(x (* (- pi/2) (+ 1.0 pos)))
		(sinx1 (sin x))
		(cosx1 (cos x))
		(c1 0.0) (c2 0.0) (c3 0.0) (c4 0.0) ;; kernel values
		(val 0.0))
	   ;;
	   (declare (type single-float pi/2 x sinx1 cosx1 c1 c2 c3 c4 val))
	   ;;
	   (setf c1 (/ sinx1 (* x x)))
	   (incf val c1)
	   (incf x pi/2)
	   ;;
	   (setf c2 (/ (- cosx1) (* x x)))
	   (incf val c2)
	   (incf x pi/2)
	   ;;
	   (setf c3 (/ (- sinx1) (* x x)))
	   (incf val c3)
	   (incf x pi/2)
	   ;;
	   (setf c4 (/ cosx1 (* x x)))
	   (incf val c4)
	   ;;
	   (setf val (/ 1.0 val))
	   (setf c4 (* c4 val))
	   (setf c3 (* c3 val))
	   (setf c2 (* c2 val))
	   (setf c1 (* c1 val))
	   ;;
	   (values c1 c2 c3 c4))))) 
		
 

(declaim (inline %interp-lanczos2-across))
;;
(defun %interp-lanczos2-across (data c1 c2 c3 c4 
				iy ix-1 ix ix+1 ix+2)
  (declare (type image data)
	   (type single-float c1 c2 c3 c4)
	   (type (unsigned-byte 24) iy ix-1 ix ix+1 ix+2))
  (+ (* c1 (aref data iy ix-1))
     (* c2 (aref data iy   ix))
     (* c3 (aref data iy ix+1))
     (* c4 (aref data iy ix+2))))
	   
	
  


(declaim (inline %flag-lanczos2-across))
;;
(defun %flag-lanczos2-across (flags iy ix-1 ix ix+1 ix+2)
  (declare (type flag-image flags)
	   (type (unsigned-byte 24) iy ix-1 ix ix+1 ix+2))
  (logior (aref flags iy ix-1)
	  (aref flags iy   ix)
	  (aref flags iy ix+1)
	  (aref flags iy ix+2)))


(declaim (inline lanczos2-interpolate-image-with-flags)
	 (ftype (function (image (or null flag-image) single-float single-float)
			  (values single-float (or NULL T) imflag))
		lanczos2-interpolate-image-with-flags))

(defun lanczos2-interpolate-image-with-flags (data flags x y)
  "Lanczos2 interpolate the pixel value in an image DATA at pixel
position DATA[Y,X], returning (LOGIOR FLAGS) 2nd value and T as third
value if x,y are inside image. The convention is that x=0 refers to
the CENTER of the first pixel. The edge cases are handled by
continuing the image at its edge pixel value the number of pixels in
the kernel. eg DATA[Y,X=-N,y]=DATA[Y,X=0].  Thus valid image data
exists between X=[-0.5,NX-0.5] and Y=[-0.5,NY-0.5]

If FLAGS is NIL then it is ignored and final flag value is zero.

Returns (VALUES INTERPOLATED-VALUE FLAG-VALUE IS-INSIDE-IMAGE)"
  (declare (type image data)
	   (type (or null flag-image) flags)
	   (type (single-float -1e6 1e6) x y)
	   (optimize speed))
  (let* ((nx (array-dimension data 1))
	 (ny (array-dimension data 0))
	 (ix 0) (ix-1 0) (ix+1 0) (ix+2 0)
	 (iy 0) (iy-1 0) (iy+1 0) (iy+2 0)
	 (fx 0.0) (fy 0.0)
	 (retval 0.0) 
	 (flagval 0)
	 (inside? nil)) 

    (declare (type (signed-byte 24) 
		   nx ny
		   ix ix-1 ix+1 ix+2
		   iy iy-1 iy+1 iy+2)
	     (type (float -1.0 1.0) fx fy)
	     (type float retval)
	     (type imflag flagval))
	     
    ;;
    (tagbody ;; use GOTOS
       ;; x,y are pixel centers so image extends from [-0.5,nx-0.5]
       (when (or (< x -0.5) (> x (- nx 0.5))
		 (< y -0.5) (> y (- ny 0.5)))
	 (go done))
       ;;
       (multiple-value-setq (ix fx) (floor x))
       (multiple-value-setq (iy fy) (floor y))
       ;;
       ;; adjust indices to extend edges beyond last pixel to allow
       ;; interpolation in full image; note that ix,iy can be -1
      
       (cond ((< ix (- nx 2))
	      (setf ix+1 (+ ix 1))
	      (setf ix+2 (+ ix 2)))
	     ((= ix (- nx 2))
	      (setf ix+1 (+ ix 1))
	      (setf ix+2 ix+1))
	     ((= ix (- nx 1))
	      (setf ix+1 ix)
	      (setf ix+2 ix))) 
       (cond ((> ix 0)
	      (setf ix-1 (- ix 1)))
	     ((= ix 0) 
	      (setf ix-1 0))
	     ((= ix -1)      ;; Note this case
	      (setf ix 0)    ;;   adjust IX itself
	      (setf ix-1 0)))
       ;;
       (cond ((< iy (- ny 2))
	      (setf iy+1 (+ iy 1))
	      (setf iy+2 (+ iy 2)))
	     ((= iy (- ny 2))
	      (setf iy+1 (+ iy 1))
	      (setf iy+2 iy+1))
	     ((= iy (- ny 1))
	      (setf iy+1 iy)
	      (setf iy+2 iy))) 
       (cond ((> iy 0)
	      (setf iy-1 (- iy 1)))
	     ((= iy 0) 
	      (setf iy-1 0))
	     ((= iy -1)      ;; Note this case
	      (setf iy 0)    ;;   adjust IY itself
	      (setf iy-1 0)))
       
       
       (setf inside? t) ;; we know we're inside

       ;; get kern weight in side-to-side direction
       (multiple-value-bind (c1 c2 c3 c4)
	 (%make-lanczos2-kernel fx)
	 (declare (type single-float c1 c3 c3 c4))
	 ;; now go DOWN image in y direction, accumulating 4 y values
	 (let ((y1 (%interp-lanczos2-across data c1 c2 c3 c4 iy-1 ix-1 ix ix+1 ix+2))
	       (y2 (%interp-lanczos2-across data c1 c2 c3 c4 iy   ix-1 ix ix+1 ix+2))
	       (y3 (%interp-lanczos2-across data c1 c2 c3 c4 iy+1 ix-1 ix ix+1 ix+2))
	       (y4 (%interp-lanczos2-across data c1 c2 c3 c4 iy+2 ix-1 ix ix+1 ix+2)))
	   (declare (type single-float y1 y3 y3 y4))
	   ;;
	   ;; and interpolate these 4 y values
	   (multiple-value-bind (d1 d2 d3 d4)
	       (%make-lanczos2-kernel fy)
	     (declare (type single-float d1 d3 d3 d4))
	     (setf retval (+ (* d1 y1) (* d2 y2) (* d3 y3) (* d4 y4))))))
       ;;
       (when flags
	 (setf flagval (logior
			(%flag-lanczos2-across flags iy-1 ix-1 ix ix+1 ix+2)
			(%flag-lanczos2-across flags iy   ix-1 ix ix+1 ix+2)
			(%flag-lanczos2-across flags iy+1 ix-1 ix ix+1 ix+2)			       
			(%flag-lanczos2-across flags iy+2 ix-1 ix ix+1 ix+2))))
       ;;
     done) ;; tagbody end
    (values retval inside? flagval)))



(declaim (inline lanczos2-interpolate-image)
	 (ftype (function (image single-float single-float)
			  (values single-float (or NULL T) imflag))
		lanczos2-interpolate-image))

(defun lanczos2-interpolate-image (data x y)
       "Lanczos2 interpolate the pixel value in an image DATA at pixel
position DATA[Y,X]. The convention is that x=0 refers to the CENTER of
the first pixel. The edge cases are handled by continuing the image at
its edge pixel value the number of pixels in the kernel. eg
DATA[Y,X=-N,y]=DATA[Y,X=0].  Thus valid image data exists between
X=[-0.5,NX-0.5] and Y=[-0.5,NY-0.5]

Returns (VALUES INTERPOLATED-VALUE IS-INSIDE-IMAGE)"
  (declare (type image data)
	   (type single-float x y))
  (lanczos2-interpolate-image-with-flags data nil x y))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline %make-lanczos3-kernel))
;;
(defun %make-lanczos3-kernel (pos) ;; cribbed from swarp/interpolate.c and validated using
  (declare (type (single-float -1.0 1.0) pos))

  ;; swarp assumes pos is in [0,1] so generalize to [-1,1]
  (when (minusp pos) (setf pos (+ 1.0 pos)))
  
  (cond ((< (abs pos) 1e-5)
	 (values 0.0 0.0 1.0 0.0 0.0 0.0))
	(t
	 (let* ((pi/3 #.(float (/ pi 3) 1.0))
		(x (* (- pi/3) (+ 2.0 pos)))
		(sinx1 (sin x))
		(cosx1 (cos x))
		(sinx2 0.0)
		(sinx3 0.0)
		(c1 0.0) (c2 0.0) (c3 0.0) (c4 0.0) (c5 0.0) (c6 0.0);; kernel values
		(val 0.0))
	   ;;
	   (declare (type single-float pi/3 x sinx1 cosx1 sinx2 sinx3 c1 c2 c3 c4 c5 c6 val))
			  ;;
	   (setf c1 (/ sinx1 (* x x)))
	   (setf val c1)
	   (incf x pi/3)
	   ;;
	   (setf sinx2 (- (* -0.5 sinx1) (* 0.866025403785 cosx1)))
	   (setf c2 (/ sinx2 (* x x)))
	   (incf val c2)
	   (incf x pi/3)
	   ;;
	   (setf sinx3  (+ (* -0.5 sinx1) (* 0.866025403785 cosx1)))
	   (setf c3 (/ sinx3 (* x x)))
	   (incf val c3)
	   (incf x pi/3)
	   ;;
	   (setf c4 (/ sinx1 (* x x)))
	   (incf val c4)
	   (incf x pi/3)
	   ;;
	   (setf c5 (/ sinx2 (* x x)))
	   (incf val c5)
	   (incf x pi/3)
	   ;;
	   (setf c6 (/ sinx3 (* x x)))
	   (incf val c6)
	   ;;
	   (setf val (/ 1.0 val))
	   (setf c6 (* c6 val))
	   (setf c5 (* c5 val))
	   (setf c4 (* c4 val))
	   (setf c3 (* c3 val))
	   (setf c2 (* c2 val))
	   (setf c1 (* c1 val))
	   ;;
	   (values c1 c2 c3 c4 c5 c6)))))
		
 

(declaim (inline %interp-lanczos3-across))
;;
(defun %interp-lanczos3-across (data c1 c2 c3 c4 c5 c6
				iy ix-2 ix-1 ix ix+1 ix+2 ix+3)
  (declare (type image data)
	   (type single-float c1 c2 c3 c4)
	   (type (unsigned-byte 24) iy ix-1 ix ix+1 ix+2))
  (+ (* c1 (aref data iy ix-2))
     (* c2 (aref data iy ix-1))
     (* c3 (aref data iy ix))
     (* c4 (aref data iy ix+1))
     (* c5 (aref data iy ix+2))
     (* c6 (aref data iy ix+3))))
	   
	




(declaim (inline %flag-lanczos3-across))

(defun %flag-lanczos3-across (flags iy ix-2 ix-1 ix ix+1 ix+2 ix+3)
  (declare (type flag-image flags)
	   (type (unsigned-byte 24) iy ix-1 ix ix+1 ix+2))
  (logior (aref flags iy ix-2)
	  (aref flags iy ix-1)
	  (aref flags iy ix)
	  (aref flags iy ix+1)
	  (aref flags iy ix+2)
	  (aref flags iy ix+3)))
	   

(declaim (inline lanczos3-interpolate-image-with-flags)
	 (ftype (function (image (or null flag-image) single-float single-float)
			  (values single-float (or NULL T) imflag))
		lanczos3-interpolate-image-with-flags))

(defun lanczos3-interpolate-image-with-flags (data flags x y)
   "Lanczos3 interpolate the pixel value in an image DATA at pixel
position DATA[Y,X], returning (LOGIOR FLAGS) 2nd value and T as third
value if x,y are inside image. The convention is that x=0 refers to
the CENTER of the first pixel. The edge cases are handled by
continuing the image at its edge pixel value the number of pixels in
the kernel. eg DATA[Y,X=-N,y]=DATA[Y,X=0].  Thus valid image data
exists between X=[-0.5,NX-0.5] and Y=[-0.5,NY-0.5]

If FLAGS is NIL then it is ignored and final flag value is zero.

Returns (VALUES INTERPOLATED-VALUE FLAG-VALUE IS-INSIDE-IMAGE)"
  
  (declare (type image data)
	   (type (or null flag-image) flags)
	   (type (single-float -1e6 1e6) x y)
	   (optimize speed))
  (let* ((nx (array-dimension data 1))
	 (ny (array-dimension data 0))
	 (ix 0) (ix-2 0) (ix-1 0) (ix+1 0) (ix+2 0) (ix+3 0)
	 (iy 0) (iy-2 0) (iy-1 0) (iy+1 0) (iy+2 0) (iy+3 0)
	 (fx 0.0) (fy 0.0)
	 (retval 0.0) (flagval 0) (inside? nil)) 

    (declare (type (signed-byte 24) 
		   nx ny
		   ix ix-2 ix-1 ix+1 ix+2 ix+3
		   iy iy-2 iy-1 ix+1 iy+2 ix+3)
	     (type (float -1.0 1.0) fx fy)
	     (type float retval)
	     (type imflag flagval))
    ;;
    (tagbody ;; use GOTOS
       ;; x,y are pixel centers so image extends from [-0.5,nx-0.5]
       (when (or (< x -0.5) (> x (- nx 0.5))
		 (< y -0.5) (> y (- ny 0.5)))
	 (go done))
       ;;
       (multiple-value-setq (ix fx) (floor x))
       (multiple-value-setq (iy fy) (floor y))
       ;;
       ;; adjust indices to extend edges beyond last pixel to allow
       ;; interpolation in full image; note that ix,iy can be -1
       (cond ((< ix (- nx 3))
	      (setf ix+1 (+ ix 1))
	      (setf ix+2 (+ ix 2))
	      (setf ix+3 (+ ix 3))) 
	     ((= ix (- nx 3))
	      (setf ix+1 (+ ix 1))
	      (setf ix+2 (+ ix 2))
	      (setf ix+3 ix+2))
	     (t ;; both ix=nx-2 and ix=nx-1 are the same
	      (setf ix+1 (- nx 1))
	      (setf ix+2 ix+1)
	      (setf ix+3 ix+1))) 
       (cond ((> ix 1)
	      (setf ix-1 (- ix 1))
	      (setf ix-2 (- ix 2)))
	     (t ;; ix=-1,0,1
	      (if (= ix -1) (setf ix 0))
	      (setf ix-1 0)
	      (setf ix-2 0)))
       ;;
       (cond ((< iy (- ny 3))
	      (setf iy+1 (+ iy 1))
	      (setf iy+2 (+ iy 2))
	      (setf iy+3 (+ iy 3)))
	     ((= iy (- ny 3))
	      (setf iy+1 (+ iy 1))
	      (setf iy+2 (+ iy 2))
	      (setf iy+3 iy+2))
	     (t ;; both iy=ny-2 and iy=ny-1 are the same
	      (setf iy+1 (- ny 1))
	      (setf iy+2 iy+1)
	      (setf iy+3 iy+1)))
       (cond ((> iy 1)
	      (setf iy-1 (- iy 1))
	      (setf iy-2 (- iy 2)))
	     (t  ;; iy=-1,0,1
	      (if (= iy -1) (setf iy 0))
	      (setf iy-1 0)
	      (setf iy-2 0)))
      
       
       
       (setf inside? t) ;; we know we're inside

       ;; get kern weight in side-to-side direction
       (multiple-value-bind (c1 c2 c3 c4 c5 c6)
	 (%make-lanczos3-kernel fx)
	 (declare (type single-float c1 c2 c3 c4 c5 c6))
	 ;; now go DOWN image in y direction, accumulating 6 y values
	 (let ((y1 (%interp-lanczos3-across 
		    data c1 c2 c3 c4 c5 c6 iy-2  ix-2 ix-1 ix ix+1 ix+2 ix+3))
	       (y2 (%interp-lanczos3-across 
		    data c1 c2 c3 c4 c5 c6 iy-1  ix-2 ix-1 ix ix+1 ix+2 ix+3))
	       (y3 (%interp-lanczos3-across 
		    data c1 c2 c3 c4 c5 c6 iy    ix-2 ix-1 ix ix+1 ix+2 ix+3))
	       (y4 (%interp-lanczos3-across 
		    data c1 c2 c3 c4 c5 c6 iy+1  ix-2 ix-1 ix ix+1 ix+2 ix+3))
	       (y5 (%interp-lanczos3-across 
		    data c1 c2 c3 c4 c5 c6 iy+2  ix-2 ix-1 ix ix+1 ix+2 ix+3))
	       (y6 (%interp-lanczos3-across 
		    data c1 c2 c3 c4 c5 c6 iy+3  ix-2 ix-1 ix ix+1 ix+2 ix+3)))
	   (declare (type single-float y1 y3 y3 y4 y5 y6))
	   ;;
	   ;; and interpolate these 6 y values
	   (multiple-value-bind (d1 d2 d3 d4 d5 d6)
	       (%make-lanczos3-kernel fy)
	     (declare (type single-float d1 d2 d3 d4 d5 d6))
	     (setf retval (+ (* d1 y1) (* d2 y2) (* d3 y3) (* d4 y4) (* d5 y5) (* d6 y6))))))
       (when flags
	 (setf flagval
	       (logior
		(%flag-lanczos3-across flags  iy-2  ix-2 ix-1 ix ix+1 ix+2 ix+3)
		(%flag-lanczos3-across flags  iy-1  ix-2 ix-1 ix ix+1 ix+2 ix+3)
		(%flag-lanczos3-across flags  iy    ix-2 ix-1 ix ix+1 ix+2 ix+3)
		(%flag-lanczos3-across flags  iy+1  ix-2 ix-1 ix ix+1 ix+2 ix+3)
		(%flag-lanczos3-across flags  iy+2  ix-2 ix-1 ix ix+1 ix+2 ix+3)
		(%flag-lanczos3-across flags  iy+3  ix-2 ix-1 ix ix+1 ix+2 ix+3))))
       ;;
     done) ;; tagbody end
    (values retval inside? flagval))) 


;; the non-flagging version uses the flagging version - inline compilation
;; should remove any references to flagging
(declaim (inline lanczos3-interpolate-image)
	 (ftype (function (image single-float single-float)
			  (values single-float (or NULL T) imflag))
		lanczos3-interpolate-image))

(defun lanczos3-interpolate-image (data x y)
     "Lanczos3 interpolate the pixel value in an image DATA at pixel
position DATA[Y,X]. The convention is that x=0 refers to the CENTER of
the first pixel. The edge cases are handled by continuing the image at
its edge pixel value the number of pixels in the kernel. eg
DATA[Y,X=-N,y]=DATA[Y,X=0].  Thus valid image data exists between
X=[-0.5,NX-0.5] and Y=[-0.5,NY-0.5]

Returns (VALUES INTERPOLATED-VALUE IS-INSIDE-IMAGE)"

  
  (declare (type image data)
	   (type single-float x y))
  (lanczos3-interpolate-image-with-flags data nil x y))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun %make-lanczos4-kernel (pos) 
  (declare (type (single-float -1.0 1.0) pos))

  ;; swarp assumes pos is in [0,1] so generalize to [-1,1]
  (when (minusp pos) (setf pos (+ 1.0 pos)))
  
  (cond ((< (abs pos) 1e-5)
	 (values 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0)) ;; 8 terms
	(t
	 (let* ((pi/4 #.(float (/ pi 4) 1.0))
		(x (* (- pi/4) (+ 3.0 pos)))
		(sinx1 (sin x))
		(cosx1 (cos x))
		(sinx2 0.0)
		(sinx3 0.0)
		;; kernel values
		(c1 0.0) (c2 0.0) (c3 0.0) (c4 0.0)
		(c5 0.0) (c6 0.0) (c7 0.0) (c8 0.0)
		(val 0.0))
	   ;;
	   (declare (type single-float pi/4 x sinx1 cosx1 sinx2 sinx3
			  c1 c2 c3 c4 c5 c6 c7 c8
			  val))
			  ;;
	   (setf c1 (/ sinx1 (* x x)))
	   (setf val c1)
	   (incf x pi/4)
	   ;;
	   (setf sinx2 (* 0.707106781186 (+ sinx1 cosx1)))
	   (setf c2 (- (/ sinx2 (* x x))))
	   (incf val c2)
	   (incf x pi/4)
	   ;;
	   (setf c3 (/ cosx1 (* x x)))
	   (incf val c3)
	   (incf x pi/4)
	   ;;
	   (setf sinx3  (* 0.707106781186 (- cosx1 sinx1)))
	   (setf c4 (- (/ sinx3 (* x x))))
	   (incf val c4)
	   (incf x pi/4)
	   ;;
	   (setf c5 (- (/ sinx1 (* x x))))
	   (incf val c5)
	   (incf x pi/4)
	   ;;
	   (setf c6 (/ sinx2 (* x x)))
	   (incf val c6)
	   (incf x pi/4)
	   ;;
	   (setf c7 (- (/ cosx1 (* x x))))
	   (incf val c7)
	   (incf x pi/4)
	   ;;
	   (setf c8 (/ sinx3 (* x x)))
	   (incf val c8)

	   ;;
	   (setf val (/ 1.0 val))
	   (setf c8 (* c8 val))
	   (setf c7 (* c7 val))
	   (setf c6 (* c6 val))
	   (setf c5 (* c5 val))
	   (setf c4 (* c4 val))
	   (setf c3 (* c3 val))
	   (setf c2 (* c2 val))
	   (setf c1 (* c1 val))
	   ;;
	   (values c1 c2 c3 c4 c5 c6 c7 c8)))))



(declaim (inline %interp-lanczos4-across))
;;
(defun %interp-lanczos4-across (data c1 c2 c3 c4 c5 c6 c7 c8
				iy ix-3 ix-2 ix-1 ix ix+1 ix+2 ix+3 ix+4)
  (declare (type image data)
	   (type single-float c1 c2 c3 c4)
	   (type (unsigned-byte 24) iy ix-1 ix ix+1 ix+2))
  (+ (* c1 (aref data iy ix-3))
     (* c2 (aref data iy ix-2))
     (* c3 (aref data iy ix-1))
     (* c4 (aref data iy ix))
     (* c5 (aref data iy ix+1))
     (* c6 (aref data iy ix+2))
     (* c7 (aref data iy ix+3))
     (* c8 (aref data iy ix+4))))
	   

(declaim (inline %flag-lanczos4-across))

(defun %flag-lanczos4-across (flags iy ix-3 ix-2 ix-1 ix ix+1 ix+2 ix+3 ix+4)
  (declare (type flag-image flags)
	   (type (unsigned-byte 24) iy ix-1 ix ix+1 ix+2))
  (logior (aref flags iy ix-3)
	  (aref flags iy ix-2)
	  (aref flags iy ix-1)
	  (aref flags iy ix)
	  (aref flags iy ix+1)
	  (aref flags iy ix+2)
	  (aref flags iy ix+3)
	  (aref flags iy ix+4)))


(declaim (inline lanczos4-interpolate-image-with-flags)
	 (ftype (function (image (or null flag-image) single-float single-float)
			  (values single-float (or NULL T) imflag))
		lanczos4-interpolate-image-with-flags))

(defun lanczos4-interpolate-image-with-flags (data flags x y)
   "Lanczos4 interpolate the pixel value in an image DATA at pixel
position DATA[Y,X], returning (LOGIOR FLAGS) 2nd value and T as third
value if x,y are inside image. The convention is that x=0 refers to
the CENTER of the first pixel. The edge cases are handled by
continuing the image at its edge pixel value the number of pixels in
the kernel. eg DATA[Y,X=-N,y]=DATA[Y,X=0].  Thus valid image data
exists between X=[-0.5,NX-0.5] and Y=[-0.5,NY-0.5]

If FLAGS is NIL then it is ignored and final flag value is zero.

Returns (VALUES INTERPOLATED-VALUE FLAG-VALUE IS-INSIDE-IMAGE)"
  
  (declare (type image data)
	   (type (or null flag-image) flags)
	   (type (single-float -1e6 1e6) x y)
	   (optimize speed))
  (let* ((nx (array-dimension data 1))
	 (ny (array-dimension data 0))
	 (ix 0) (ix-3 0) (ix-2 0) (ix-1 0)
	 (ix+1 0) (ix+2 0) (ix+3 0) (ix+4 0)
	 (iy 0) (iy-3 0) (iy-2 0) (iy-1 0)
	 (iy+1 0) (iy+2 0) (iy+3 0) (iy+4 0)
	 (fx 0.0) (fy 0.0)
	 (retval 0.0) (flagval 0) (inside? nil)) 

    (declare (type (signed-byte 24) 
		   nx ny
		   ix ix-3 ix-2 ix-1 ix+1 ix+2 ix+3 ix+4
		   iy iy-3 iy-2 iy-1 iy+1 iy+2 iy+3 iy+4)
	     (type (float -1.0 1.0) fx fy)
	     (type float retval)
	     (type imflag flagval))
    ;;
    (tagbody ;; use GOTOS
       ;; x,y are pixel centers so image extends from [-0.5,nx-0.5]
       (when (or (< x -0.5) (> x (- nx 0.5))
		 (< y -0.5) (> y (- ny 0.5)))
	 (go done))
       ;;
       (multiple-value-setq (ix fx) (floor x))
       (multiple-value-setq (iy fy) (floor y))
       ;;
       ;; adjust indices to extend edges beyond last pixel to allow
       ;; interpolation in full image; note that ix,iy can be -1
       (cond ((< ix (- nx 4))
	      (setf ix+1 (+ ix 1))
	      (setf ix+2 (+ ix 2))
	      (setf ix+3 (+ ix 3))
	      (setf ix+4 (+ ix 4)))
	     ((= ix (- nx 4))
	      (setf ix+1 (+ ix 1))
	      (setf ix+2 (+ ix 2))
	      (setf ix+3 (+ ix 3)) ;; nx-1
	      (setf ix+4 ix+3))
	     ((= ix (- nx 3))
	      (setf ix+1 (+ ix 1))
	      (setf ix+2 (+ ix 2)) ;; nx-1
	      (setf ix+3 ix+2) 
	      (setf ix+4 ix+2))
	     (t ;; ix=nx-2 or ix=nx-1
	      (setf ix+1 (- nx 1))
	      (setf ix+2 ix+1)     ;; nx-1 
	      (setf ix+3 ix+1)
	      (setf ix+4 ix+1)))
       ;;
       (cond ((> ix 2)
	      (setf ix-1 (- ix 1))
	      (setf ix-2 (- ix 2))
	      (setf ix-3 (- ix 3)))
	     ((= ix 2)
	      (setf ix-1 (- ix 1))
	      (setf ix-2 0)
	      (setf ix-3 0))
	     (t ;; ix=-1,0,1 
	      (if (= ix -1) (setf ix 0))
	      (setf ix-1 0)
	      (setf ix-2 0) 
	      (setf ix-3 0)))
       ;;
       (cond ((< iy (- ny 4))
	      (setf iy+1 (+ iy 1))
	      (setf iy+2 (+ iy 2))
	      (setf iy+3 (+ iy 3))
	      (setf iy+4 (+ iy 4)))
	     ((= iy (- ny 4))
	      (setf iy+1 (+ iy 1))
	      (setf iy+2 (+ iy 2))
	      (setf iy+3 (+ iy 3)) ;; ny-1
	      (setf iy+4 iy+3))
	     ((= iy (- ny 3))
	      (setf iy+1 (+ iy 1))
	      (setf iy+2 (+ iy 2)) ;; ny-1
	      (setf iy+3 iy+2) 
	      (setf iy+4 iy+2))
	     (t ;; iy=ny-2 or iy=ny-1
	      (setf iy+1 (- ny 1))
	      (setf iy+2 iy+1)     ;; ny-1 
	      (setf iy+3 iy+1)
	      (setf iy+4 iy+1)))
       ;;
       (cond ((> iy 2)
	      (setf iy-1 (- iy 1))
	      (setf iy-2 (- iy 2))
	      (setf iy-3 (- iy 3)))
	     ((= iy 2)
	      (setf iy-1 (- iy 1))
	      (setf iy-2 0)
	      (setf iy-3 0))
	     (t ;; iy=-1,0,1 
	      (if (= iy -1) (setf iy 0))
	      (setf iy-1 0)
	      (setf iy-2 0) 
	      (setf iy-3 0)))
      
       (setf inside? t) ;; we know we're inside

       ;; get kern weight in side-to-side direction
       (multiple-value-bind (c1 c2 c3 c4 c5 c6 c7 c8)
	 (%make-lanczos4-kernel fx)
	 (declare (type single-float c1 c2 c3 c4 c5 c6 c7 c8))
	 ;; now go DOWN image in y direction, accumulating 6 y values
	 (let ((y1 (%interp-lanczos4-across 
		    data c1 c2 c3 c4 c5 c6 c7 c8
		    iy-3  ix-3 ix-2 ix-1 ix ix+1 ix+2 ix+3 ix+4))
	       (y2 (%interp-lanczos4-across 
		    data c1 c2 c3 c4 c5 c6 c7 c8
		    iy-2  ix-3 ix-2 ix-1 ix ix+1 ix+2 ix+3 ix+4))
	       (y3 (%interp-lanczos4-across 
		    data c1 c2 c3 c4 c5 c6 c7 c8
		    iy-1  ix-3 ix-2 ix-1 ix ix+1 ix+2 ix+3 ix+4))
	       (y4 (%interp-lanczos4-across 
		    data c1 c2 c3 c4 c5 c6 c7 c8
		    iy    ix-3 ix-2 ix-1 ix ix+1 ix+2 ix+3 ix+4))
	       (y5 (%interp-lanczos4-across 
		    data c1 c2 c3 c4 c5 c6 c7 c8
		    iy+1  ix-3 ix-2 ix-1 ix ix+1 ix+2 ix+3 ix+4))
	       (y6 (%interp-lanczos4-across 
		    data c1 c2 c3 c4 c5 c6 c7 c8
		    iy+2  ix-3 ix-2 ix-1 ix ix+1 ix+2 ix+3 ix+4))
	       (y7 (%interp-lanczos4-across 
		    data c1 c2 c3 c4 c5 c6 c7 c8
		    iy+3  ix-3 ix-2 ix-1 ix ix+1 ix+2 ix+3 ix+4))
	       (y8 (%interp-lanczos4-across 
		    data c1 c2 c3 c4 c5 c6 c7 c8
		    iy+4  ix-3 ix-2 ix-1 ix ix+1 ix+2 ix+3 ix+4)))
	   (declare (type single-float y1 y3 y3 y4 y5 y6 y7 y8))
	   ;;
	   ;; and interpolate these 6 y values
	   (multiple-value-bind (d1 d2 d3 d4 d5 d6 d7 d8)
	       (%make-lanczos4-kernel fy)
	     (declare (type single-float d1 d2 d3 d4 d5 d6 d7 d8))
	     (setf retval (+ (* d1 y1) (* d2 y2) (* d3 y3) (* d4 y4)
			     (* d5 y5) (* d6 y6) (* d7 y7) (* d8 y8))))))
       (when flags
	 (setf flagval
	       (logior
		(%flag-lanczos4-across flags  iy-3  ix-3 ix-2 ix-1 ix ix+1 ix+2 ix+3 ix+4)
		(%flag-lanczos4-across flags  iy-2  ix-3 ix-2 ix-1 ix ix+1 ix+2 ix+3 ix+4)
		(%flag-lanczos4-across flags  iy-1  ix-3 ix-2 ix-1 ix ix+1 ix+2 ix+3 ix+4)
		(%flag-lanczos4-across flags  iy    ix-3 ix-2 ix-1 ix ix+1 ix+2 ix+3 ix+4)
		(%flag-lanczos4-across flags  iy+1  ix-3 ix-2 ix-1 ix ix+1 ix+2 ix+3 ix+4)
		(%flag-lanczos4-across flags  iy+2  ix-3 ix-2 ix-1 ix ix+1 ix+2 ix+3 ix+4)
		(%flag-lanczos4-across flags  iy+3  ix-3 ix-2 ix-1 ix ix+1 ix+2 ix+3 ix+4)
		(%flag-lanczos4-across flags  iy+4  ix-3 ix-2 ix-1 ix ix+1 ix+2 ix+3 ix+4))))
       ;;
     done) ;; tagbody end
    (values retval inside? flagval)))


(declaim (inline lanczos4-interpolate-image)
	 (ftype (function (image single-float single-float)
			  (values single-float (or NULL T) imflag))
		lanczos4-interpolate-image))

(defun lanczos4-interpolate-image (data x y)
   "Lanczos4 interpolate the pixel value in an image DATA at pixel
position DATA[Y,X] T as second value if x,y are inside image. The
convention is that x=0 refers to the CENTER of the first pixel. The
edge cases are handled by continuing the image at its edge pixel value
the number of pixels in the kernel. eg DATA[Y,X=-N,y]=DATA[Y,X=0].
Thus valid image data exists between X=[-0.5,NX-0.5] and
Y=[-0.5,NY-0.5]

Returns (VALUES INTERPOLATED-VALUE IS-INSIDE-IMAGE)"
  (declare (type image data)
	   (type single-float x y))
  (lanczos4-interpolate-image-with-flags data nil x y))


(deftype image-interp-method ()
  `(member :nearest :linear :lanczos2 :lanczos3 :lanczos4))
	 

(declaim (inline general-interpolate-image-with-flags)
	 (ftype (function (image (or null flag-image) single-float single-float
				interp-method-type)
			  (values (or null single-float) (or NULL T) imflag))
		general-interpolate-image-with-flags))

(defun general-interpolate-image-with-flags (data flag-im x y interp-method)
  "Interpolate an image using INTERP-METHOD,  one of 
     :NEAREST :LINEAR or :LANCZOS2,3,4, 
with FLAG-IM image (or NULL).

Return (VALUES INTERPOLATED-VALUE IS-INSIDE? FLAG-LOGIOR)

"
  (declare (type image data)
	   (type (or null flag-image) flag-im)
	   (type single-float x y)
	   (type image-interp-method interp-method))
  (cond ((eq interp-method :nearest)
	 (nearest-pixel-interpolate-image-with-flags data flag-im x y))
	((eq interp-method :linear)
	 (linear-interpolate-image-with-flags data flag-im x y))
	((eq interp-method :lanczos2)
	 (lanczos2-interpolate-image-with-flags data flag-im x y))
	((eq interp-method :lanczos3)
	 (lanczos3-interpolate-image-with-flags data flag-im x y))
	((eq interp-method :lanczos4)
	 (lanczos4-interpolate-image-with-flags data flag-im x y))
	(t ;; should not be needed but older SBCLs don't do enough inference
	 (error "Bad interp method ~A" interp-method))))
	
(declaim (inline general-interpolate-image)
	 (ftype (function (image single-float single-float
				interp-method-type)
			  (values (or null single-float) (or NULL T) imflag))
		general-interpolate-image))


(defun general-interpolate-image (data x y interp-method)
  "Interpolate an image using INTERP-METHOD, one of 
     :NEAREST :LINEAR or :LANCZOS2,3,4

Return (VALUES INTERPOLATED-VALUE IS-INSIDE? 0)
where the last zero is a dummy flag for cvonsistency with
GENERAL-INTERPOLATE-IMAGE-WITH-FLAGS."
  (declare (type image data)
	   (type single-float x y)
	   (type (member :nearest :linear :lanczos2 :lanczos3 :lanczos4) interp-method))
  (general-interpolate-image-with-flags data nil x y interp-method))
	




#|

Functions used to validate that this is producing valid Lanczos coefficients:

(defun lan-func (y a)
  "Generate the values of the Lanczos function of order A at y"
   (cond ((= y 0)
	  1.0)
	 ((> y (abs a))
	  0)
	 (t
	  (/ (* a 
		(sin (* pi y))
		(sin (/ (* pi y) a)))
	     (* pi pi y y)))))

;; for a discussion of normalization (as done in SWARP)
;; see https://skyview.gsfc.nasa.gov/blog/index.php/2016/02/04/lanczos-normalization-error/
(defun lan-series (x a &key (normalize t))
  "Generate the series of Lanczos coefficients"
  (let* ((sum 0d0)
	 (clist
	   (loop with fx = (floor x)
		 for i from (- fx a -1) to (+ fx a)
		 for lval = (lan-func (- x i) a)
		 do (incf sum lval)
		 collect lval)))
    (when normalize
      (setf clist (loop for x in clist
			collect (/ x sum))))
    (values clist
            sum)))

|#
