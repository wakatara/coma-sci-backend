

;; we make the following assumptions when generating simulated data
;;
;;   1) stars are distributed in magnitude as n(m)=10^(3M/5)=e^(ln(10)*3M/5)
;;   2) stars are distributed uniformly in space
;;   

(format t "NOTE: The ARTDATA package should be integrated better into IMUTILS.")

(defpackage artdata
  (:use #:cl)
  (:export
   #:gauss-fwhm-to-sigma
   #:mag-to-flux #:flux-to-mag
   #:make-gaussian-starbuf
   #:make-moffat-starbuf
   #:add-starbuf-to-array 
   #:add-trailed-starbuf-to-array
   #:pick-random-star-mag
   #:make-gaussian-star-array
   #:add-background-to-image
   #:add-noise-to-image
   ))


(in-package artdata)



(eval-when (load eval compile)
  (pushnew :artdata-tests *features*) ;; do we compile in tests?
  )


(defun gauss-fwhm-to-sigma (fwhm)
  "Given a FWHM, convert to a Gaussian sigma"
  (* 0.5d0 fwhm #.(/ (sqrt (* 2d0 (log 2d0))))))

;; default size of one dimension of a template star
(defparameter *n-star-default* 21)

;; a template star with integrated flux of 1.0
(defstruct starbuf 
  (name "un-named starbuf")
  (fwhm 0d0 :type single-float)
  (n *n-star-default* :type (unsigned-byte 28)) ;; size, an odd number
  (a (make-array (list *n-star-default* *n-star-default*)
		 :element-type 'single-float)
     :type (simple-array single-float (* *))))
  
 

(declaim (inline interpolate-starbuf-value))
(defun interpolate-starbuf-value (sb x y)
  "Use bilinear interpolation to get the value of a starbuf at
fractional pixel position x,y where 0,0 is the center pixel.

When interpolating, the value in each pixel is the center of the pixel,
so that the outermost half-pixel is set to zero, because it is beyond
the outermost value


 "
  (declare (type starbuf sb)
	   (type (single-float -1e6 1e6)  x y)
	   (optimize speed))
  (let* ((n0 (ash (starbuf-n sb) -1)) ;; center pixel
	 (n (starbuf-n sb))
	 (x0 (+ n0 x)) ;; fractional pixel position
	 (y0 (+ n0 y))
	 ;; the bounding pixels of the interpolated point
	 ;; and the fraction of the way between them
	 (nx1 0) (nx2 0) (fx 0.0)
	 (ny1 0) (ny2 0) (fy 0.0)
	 (result 0.0))
    (declare (type (signed-byte 28) n0 nx1 nx2 ny1 ny2)
	     (type (single-float 0.0 1.0) fx fy)
	     (type single-float result))
    (multiple-value-setq (nx1 fx) (floor x0))
    (multiple-value-setq (ny1 fy) (floor y0))
    ;; a small gotcha - if fx is 0.0, then we could be right at edge
    ;; of bound of array, but we don't want to return 0, so
    ;; we set nx2/ny2 to be the previous pixel, since it won't be weighted 
    (setf nx2 (if (plusp fx) (+ nx1 1) nx1))
    (setf ny2 (if (plusp fy) (+ ny1 1) ny1))


    (block calc-result
      (macrolet ((checkbounds (var) 
		   `(if (or (< ,var 0) (>= ,var n))
		     (return-from calc-result))))
	(checkbounds nx1)
	(checkbounds nx2)
	(checkbounds ny1)
	(checkbounds ny2))
      ;; and interpolate
      (let* ((a (starbuf-a sb))
	     (1-fx (- 1.0 fx))
	     (1-fy (- 1.0 fy))
	     (v11 (aref a ny1 nx1)) ;; note that dimensions an 'a' are ordered y,x as usual
	     (v12 (aref a ny2 nx1)) ;;   (ie, 2nd index, changing fastest, is x)
	     (v21 (aref a ny1 nx2))
	     (v22 (aref a ny2 nx2)))
	     ;;
	(setf result 
	      (+ (* 1-fx 1-fy v11)   ;; yes, all the coefficients add up to 1
		 (* 1-fx   fy v12)
		 (*   fx 1-fy v21)
		 (*   fx   fy v22)))))
    result))
    


(defun %normalize-starbuf (sb norm)
  (loop with a = (starbuf-a sb)
	with n = (array-total-size a)
	with scale = (float (/ norm (imutils:image-sum a)) 1.0)
	for i below n
	do (setf (row-major-aref a i)
		 (* (row-major-aref a i) scale))))
  
(defun make-gaussian-starbuf (&key (normalization 1.0)
			      (name "gaussian starbuf")
			      (n *n-star-default*)
			      (fwhm 5.0))
  "make a NxN STARBUF structure with NORMALIZATION counts in the shape of a Gaussian
of FWHM - this can then be added to arrays.  FWHM is in pixel units." 
  (let* ((fwhm (float fwhm 1.0))
	 (starbuf (make-starbuf
		   :name name
		   :fwhm fwhm
		   :n n
		   :a (make-array (list n n)
				  :element-type 'single-float)))
	 (sigma (float (gauss-fwhm-to-sigma fwhm) 1.0))
	 (n0 (ash n -1)))
    (imutils:add-gaussian-to-image 
     (starbuf-a starbuf) 
     (* (1- n) 0.5)
     (* (1- n) 0.5)
     sigma sigma 0.0 normalization :subpix 5)
    (%normalize-starbuf starbuf normalization)
    starbuf))



(defun make-moffat-starbuf (&key (normalization 1.0)
			      (name "gaussian starbuf")
			      (n *n-star-default*)
			      (fwhm 5.0)
			      (beta 2.5))
  "make a NxN STARBUF structure with NORMALIZATION counts in the shape of a
round Moffat function - this can then be added to arrays.  FWHM is in pixel units." 
  (let* ((fwhm (float fwhm 1.0))
	 (starbuf (make-starbuf
		   :name name
		   :fwhm fwhm
		   :n n
		   :a (make-array (list n n)
				  :element-type 'single-float)))
	 (ab (/ fwhm (* 2 (sqrt (- (expt 2 (/ 1.0 beta)) 1)))))
	 (n0 (ash n -1)))
    (imutils:add-moffat-to-image 
     (starbuf-a starbuf) 
     (* (1- n) 0.5)
     (* (1- n) 0.5)
     ab ab beta 0.0 normalization :subpix 1)
    (%normalize-starbuf starbuf normalization)
    starbuf))
		  
    

		 
(defun add-starbuf-to-array/roundpix (a x0 y0 starbuf norm)
  "add a starbuf to an array a at index position X0,Y0 - (Y is
 first dimension) we use integer roundoff of the pixel position
 to avoid interpolating fractional pixels"
  (declare (type imutils:image a)
	   (type (single-float -1e7 1e7) x0 y0)
	   (type single-float norm)
	   (starbuf starbuf)
	   (optimize speed))
  (loop
   with nxmax =  (array-dimension a 1)
   with nymax =  (array-dimension a 0)
   with as = (starbuf-a starbuf)
   with n = (starbuf-n starbuf)
   with n/2 = (ash n -1)
   with nx0 of-type (signed-byte 28) = (floor (+ 0.5 x0))
   with ny0 of-type (signed-byte 28) = (floor (+ 0.5 y0))
   for ix  of-type (signed-byte 28) from (- nx0 n/2) 
   for jx  of-type (unsigned-byte 28) below n
   do
   (loop
    for iy  of-type (signed-byte 28) from (- ny0 n/2)
    for jy  of-type (unsigned-byte 28) below n
    do
    (when (and (>= ix 0) (< ix nxmax)
	       (>= iy 0) (< iy nymax))
      (incf (aref a iy ix) (* norm (aref as jy jx)))))))






(defun add-starbuf-to-array (a x0 y0 starbuf norm)
  "add a starbuf to an array a at index position X0,Y0 - (Y is first dimension)
Uses interpolation of the starbuf array, so that fractional pixel positions
are allowed."
  (declare (type imutils:image a)
	   (type (single-float -1e7 1e7) x0 y0)
	   (type single-float norm)
	   (type starbuf starbuf)
	   (optimize speed))

  ;; fx0 and fy0 are the fractional pixel displacement between A and starbuf
  (let ((nx0 0) (fx0 0.0) (ny0 0) (fy0 0.0))
    (declare (type (signed-byte 28) nx0 ny0)
	     (type single-float fx0 fy0))
    ;;
    (multiple-value-setq (nx0 fx0) (floor (+ 0.5 x0)))
    (multiple-value-setq (ny0 fy0) (floor (+ 0.5 y0)))
    
    (loop
     with nxmax =  (array-dimension a 1)
     with nymax =  (array-dimension a 0)
     with n = (starbuf-n starbuf)
     with n/2 = (ash n -1)
     ;;
     for ix  of-type (signed-byte 28) from (- nx0 n/2) to  (+ nx0 n/2) 
     ;; CHECKME - THE -0.5 + FX0 MATH NEEDS TO BE CHECKED, BUT IMAGES LOOK OK
     for x of-type single-float = (+ 0.5 (- fx0) (float (- n/2))) then (+ 1.0 x)
     do
     (loop
      for iy  of-type (signed-byte 28) from (- ny0 n/2) to  (+ ny0 n/2)
      for y of-type single-float = (+ 0.5 (- fy0) (float (- n/2))) then (+ 1.0 y)
      do
      ;;    (print (list x y (interpolate-starbuf-value starbuf x y)))
      (when (and (>= ix 0) (< ix nxmax)
		 (>= iy 0) (< iy nymax))
	(incf (aref a iy ix) 
	      (* norm (interpolate-starbuf-value starbuf x y))))))))




(defun add-trailed-starbuf-to-array (a x0 y0 x1 y1 starbuf norm
				     &key (brightness-func nil) (n-steps nil))
  "add a trailed starbuf as N-STEPS discrete steps, with a total
normalization NORM.  BRIGHTNESS-FUNC is an optional function of a
parameter from 0 to 1 from x0,y0 to x1,y1 that tells how bright each
step should be, subject to the total normalization NORM.  If N-STEPS
is unspecified, use step of size 1/2 pix."
  (declare (type imutils:image a)
	   (type (single-float -1e7 1e7) x0 y0 x1 y1)
	   (type single-float norm)
	   (type starbuf starbuf)
	   (type (or null (function (single-float) single-float)) brightness-func)
	   (type (or null (unsigned-byte 20)) n-steps))
  (let* ((dx (- x1 x0))
	 (dy (- y1 y0))
	 (dr (sqrt (+ (expt dx 2) (expt dy 2))))
	 (n-steps (max 1 (or n-steps (ceiling (* 2 dr)))))
	 (bfunc (or brightness-func (lambda (x) 1.0)))
	 (norm-bfunc 0.0))
    (declare (type single-float dx dy dr norm-bfunc)
	     (type (unsigned-byte 20) n-steps))

    ;; compute total normalization arising from function, and 
    ;; place into norm-bfunc
    (loop with ns-1 = (1- n-steps)
	  for i below n-steps
	  for tau of-type single-float = (* i (/ 1.0 ns-1))
	  do (incf norm-bfunc (funcall bfunc tau)))
    ;;
    (loop 
     with ns-1 = (1- n-steps)
     for i below n-steps
     ;; = x0 then ... avoid divide-by-zero for ns=1
     for x of-type single-float =  x0 then (+ x (/ dx ns-1))
     for y of-type single-float =  y0 then (+ y (/ dy ns-1))
     ;; parameter along the line
     for tau of-type single-float = (* i (/ 1.0 ns-1))
     for this-norm of-type single-float = (* (/ norm norm-bfunc) (funcall bfunc tau))
     do
     (add-starbuf-to-array a x y starbuf this-norm))))
	  
    
  
  
;; convert a magnitude to a flux, such that a mag of MAG0 has flux=1
(defun mag-to-flux (mag &key (mag0 0d0)  (exptime 1d0))
  "convert a magnitude to a flux, such that a zero point mag of MAG0 has
flux=1 in an EXPTIME of 1.0

The formula is   FLUX=EXPTIME*10^{-0.4*(mag-mag0))"
  (float (* exptime (expt 10 (/ (- mag mag0) -2.5d0))) 1.0))




(defun flux-to-mag (flux &key  (mag0 0d0) (exptime 1d0))
  "convert a flux to a magnitude, such that a zero point mag of MAG0 has
flux=1 in an EXPTIME of 1.0

The formula is MAG=MAG0 - 2.5 log10 (flux/exptime)"
  (float (+ mag0 (* -2.5d0 (log (/ flux exptime) 10d0))) 1.0))

;; generate a magnitude from a power law distribution. The default 3/5 power law
;; is just the one results from a space-filling distribution of stars
(defun pick-random-star-mag (min-mag max-mag &key (mag-power-law #.(/ 3d0 5d0)))
  (random:exponential (float (* mag-power-law #.(log 10d0)) 1d0)
		      (float min-mag 1d0) (float max-mag 1d0)))

(defun generate-star-flux (flux-min flux-max &key (mag-power-law #.(/ 3d0 5d0)))
  "generate a random stellar flux between FLUX-MIN and FLUX-MAX, such that
the differential distribution of magnitudes is N(m)~10^(bm) where b is the
parameter MAG-POWER-LAW"
  (let* ((mag-max (flux-to-mag flux-min))
	 (mag-min (flux-to-mag flux-max))
	 (mag (pick-random-star-mag mag-min mag-max :mag-power-law mag-power-law))
	 (flux (mag-to-flux mag)))
    ;;(print (list mag-max mag-min mag flux))
    flux))
    
	 
(defun make-gaussian-star-array
    (&key (nstars 1000) (nx 512) (ny 512)
       (fwhm 5.0) (min-flux 1000.0) (max-flux 1e5)
       (mag-power-law 0.6)
       (add-gaussian-noise nil)
       (background-flux 0.0)
       ;; if NOISE-ARRAY T, then turn initial flux result into a
       ;; Gaussian noise image by replacing each element by its square
       ;; root times a Gaussian random number
       (noise-array nil)) 
  "make an 2d single float array filled with stars"
  (let* ((a (make-array (list ny nx) :element-type 'single-float
			:initial-element background-flux))
	 (nsb (+ 1 (ceiling (* 6 (round fwhm)))))
	 (sb (make-gaussian-starbuf :n nsb :fwhm fwhm))
	 ;; a vector of Gaussian random numbers that we reuse, to
	 ;; speed up computation, and to assure that we use the same Gaussian
	 ;; random numbers each time for repeatability
	 (grstate (random:make-gaussian-random-state)))
		 
    (loop for i below nstars
	  for flux = (float (generate-star-flux 
			     min-flux max-flux 
			     :mag-power-law mag-power-law)
			    1.0)
	  do (add-starbuf-to-array a
				   (float (random ny) 1.0) 
				   (float (random nx) 1.0) sb flux))


    (loop for i below (array-total-size a)
	  do (when (< (row-major-aref a i) 0)
	       (error "negative element in flux array")))

    (when add-gaussian-noise
      (loop for ia of-type fixnum below (array-total-size a)
	     for x of-type (single-float 0.0) = (row-major-aref a ia)
	     for grand = (float (random:gaussian grstate) 1.0)
	     for xx of-type single-float  = (+ x (* grand (sqrt x)))
	     do (setf (row-major-aref a ia) xx)))
	     

    ;;
    ;;
    ;; if a should be a noise-array rather than a flux-array, turn
    ;; each element into square root of itself times a random
    ;; variable.
    (when noise-array
      (loop 
       for ia of-type fixnum below (array-total-size a)
       for grand = (float (random:gaussian grstate) 1.0)
       do
       (setf (row-major-aref a ia)
	     (* (sqrt (row-major-aref a ia)) grand))))
    ;;
    a))





(defun add-background-to-image (a backd)
  "Add a random background to image, of level BACKD.  No noise is added."
  (declare (type imutils:image a)
	   (type real backd))
  (loop with b = (float backd 1.0)
	for i below (array-total-size a)
	do (incf (row-major-aref a i) b)))

(defun add-noise-to-image (a &key (gain 1.0))
  "Add gaussian random noise to an image assuming that there are GAIN electrons
for each image value."
  (declare (type imutils:image a)
	   (type real gain))
  (loop with gfac of-type single-float = (float (/ 1.0 (sqrt gain)) 1.0)
	for i below (array-total-size a)
	for noise = (float (* (random:gaussian) (* gfac (sqrt (row-major-aref a i))))
			   1.0)
	do (incf (row-major-aref a i) noise)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST FUNCTIONS FOLLOW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+artdata-tests
(defun test-show-array (&key (nstars 1000) (noise-array nil)) 
  (let ((p (pgplot:open-device :x11)))
    (pgplot:image p
		  (make-gaussian-star-array :nstars nstars :noise-array noise-array)
		  :type :gray)
    (pgplot:box p :x-num-labels-bottom nil
		:y-num-labels-left nil)))
	 
#+artdata-tests
(defun test-show-centering () 
  (let* ((n 21)
	 (a (make-array (list n n) :element-type 'single-float))
	 (nsb 5)
	 (sb (make-gaussian-starbuf :n nsb :fwhm 5.0)))
    (add-starbuf-to-array a 0.0 0.0 sb 1.0)
    (add-starbuf-to-array a 10.0 10.0 sb 1.0)
    (add-starbuf-to-array a 20.0 20.0 sb 1.0)
    
    (let ((p (pgplot:open-device :x11)))
      (pgplot:toplabel p "starbuf")
      (pgplot:image p  (starbuf-a sb) :type :gray)
       (pgplot:box p :x-num-labels-bottom nil
		   :y-num-labels-left nil))


  (let ((p (pgplot:open-device :x11)))
    (pgplot:toplabel p "Image")
    (pgplot:image p
		  a
		  :type :gray)
    (pgplot:box p :x-num-labels-bottom nil
		:y-num-labels-left nil))))    


#+artdata-tests
(defun test-show-trail (&key (n 255) (x0 50.0) (y0 50.0) (x1 200.0) (y1 200.0)
			(fwhm 5.0)
			(n-steps nil)
			(norm 1000.0))
  (let ((p (pgplot:open-device :x11))
	(a (make-array (list n n) :element-type 'single-float))
	(sb (make-gaussian-starbuf :fwhm fwhm)))

    (add-trailed-starbuf-to-array a x0 y0 x1 y1 sb norm :n-steps n-steps)
    
    (pgplot:image p
		  a
		  :type :gray)
    (pgplot:box p :x-num-labels-bottom nil
		:y-num-labels-left nil)))


#+artdata-tests
(defun test-interp-starbuf  (&key (nsb 7) (fwhm 4.0))
  (let ((p (pgplot:open-device :x11))
	(sb (make-gaussian-starbuf :fwhm fwhm :n nsb)))
    (pgplot:set-window p -8 8  -0.02 0.1)
    (pgplot:box p)
    (loop 
     with y = 0.0
     for x from -8.0 to 8.0  by 0.01
     for val = (interpolate-starbuf-value sb x y)
     do
     (pgplot:points p x val :point))))

