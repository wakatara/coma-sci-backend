
(in-package imutils)


(defun histogram-equalize-image (image &key (frac-low 0.0) (frac-high 1.0) (x-low 0.0) (x-high 1.0)
					(nsample 100000)
					(n-map-vec 1024))
  "Take the non-NaN, non-InF pixels of IMAGE, and map the image by cumulative
fraction so that pixel fraction FRAC-HIGH maps to X-LOW, and fraction FRAC-HIGH
maps to X-HIGH.  Return the mapped image, and a pair of vactors that maps
original values to output values, of length N-MAP-VEC
 (VALUES IMAGE-OUT ORIG-VEC OUT-VEC)" 

  (declare (type image image)
	   (type (real 0) frac-low frac-high x-low x-high)
	   (type (integer 2 #.(expt 2 20)) n-map-vec))

  (let* ((frac-low (float frac-low 1.0))
	 (frac-high (float frac-high 1.0))
	 (x-low (float x-low 1.0))
	 (x-high (float x-high 1.0))
	 (ntot   (array-total-size image))
	 (ngood (loop for i of-type fixnum below ntot
		      when (not (float-utils:single-float-nan-or-infinity-p
				 (row-major-aref image i)))
			count 1))
	 (nsamp (min ngood nsample))
	 (orig-vec (make-array n-map-vec :element-type 'single-float))
	 (out-vec (make-array n-map-vec :element-type 'single-float))
	 (image-out (copy-image image))
	 (scratch-vec (make-array nsamp :element-type 'single-float))
	 (fscratch-vec (make-array nsamp :element-type 'single-float))
	 (n-low 0)
	 (n-high 0)
	 (im-high 0.0)
	 (im-low 0.0)
	 (linterp-struct nil)
	 (linterp-struct-inv nil))
    (locally
	(declare (type fixnum ntot ngood nsamp n-low n-high )
		 (type (float 0.0 1.0) frac-low frac-high)
		 (type single-float x-low x-high  im-high im-low)
		 (type (simple-array single-float (*)) scratch-vec fscratch-vec)
		 (optimize speed))

      (get-random-pixel-sample image nsamp :scratch-vec scratch-vec :allow-nan nil)
      (setf scratch-vec (sort scratch-vec #'<))


      ;; set the bounds in scratch-vec that define the mapping range
      (setf n-low (round (* frac-low nsamp)))
      (setf im-low  (aref scratch-vec n-low))
      (setf n-high (round (* frac-high (1- nsamp))))
      (setf im-high (aref scratch-vec n-high))

      ;; now fill fscratch-vec with the fraction that each value corresponds to,
      ;; so the pairing scratch-vec and fscratch-vec providing a mapping between
      ;; original and histogram-remapped image values
      (loop with dn = (- n-high n-low)
	    with dx of-type single-float = (- x-high x-low)
	    for i of-type fixnum below nsamp 
	    for f of-type single-float = (cond ((< i n-low) x-low)
					       ((> i n-high) x-high)
					       (t
						(+ x-low (* dx (/ (float (- i n-low) 1.0) dn)))))
	    do (setf (aref fscratch-vec i) f))


      ;; necessary to make interpolation work
      (%ensure-sorted-vector-is-monotonically-increasing scratch-vec)
      
      ;; linear interpolator from image values to histogram valus
      (setf linterp-struct
	    (linterpf:make-linterp
	     scratch-vec fscratch-vec :x-min-val 0.0 :x-max-val 1.0))
      ;; and the inverse interpolator
      (setf linterp-struct-inv
	    (linterpf:make-linterp
	     fscratch-vec scratch-vec :x-min-val nil :x-max-val nil)))
	  
    
	  
    (loop for i of-type fixnum below ntot
	  for x of-type single-float = (row-major-aref image i)
	  if (float-utils:single-float-nan-or-infinity-p x) ;; make all bad pix NaN
	    do (setf (row-major-aref image-out i) float-utils:*single-float-nan*)
	  else
	    do
	       (setf (row-major-aref image-out i)
		     (cond ((<= x im-low)  x-low)
			   ((>= x im-high) x-high)
			   (t
			    (linterpf:linterp x linterp-struct)))))

    ;; now create the mapping vectors
    (loop for i of-type fixnum below n-map-vec
	  for f of-type single-float = (/ (float i) (1- n-map-vec))
	  do (setf (aref out-vec i) f)
	     (setf (aref orig-vec i)
		   (linterpf:linterp f linterp-struct-inv)))

    (values image-out orig-vec out-vec)))


;; given a vector that is sorted by >, modify it to be monotically increasing
(defun %ensure-sorted-vector-is-monotonically-increasing (vec)
   (declare (type (simple-array single-float (*)) vec)
	    (optimize speed))
  (flet ((minimally-increase (x) ;; function to increase x as little as possible
	   (declare (type single-float x)) ;; but still create a different float
	   (cond ((= x 0.0) least-positive-single-float) ;; special case
		 (t  ;; try a growth factor of 1e-7 and double until we get a different float
		  (loop for fac of-type single-float = 1e-7 then (* 2 fac)
			for i from 0
			for xnew = (+ x (* (abs x) fac))
			when (not (= x xnew))
			  do (return xnew)
			when (> i 10)
			  do
			     (error "Strange error in %ensure-vector-is-monotonically-increasing - could not grow float"))))))
		    
    (loop for i from 1 below (length vec)
	  when (not (< (aref vec (1- i)) (aref vec i)))
	    ;; use the element vec[1-i] rather than vec[i] because our process could have
	    ;; made vec[1-i]>vec[i]
	    do (setf (aref vec i) (minimally-increase (aref vec (1- i)))))
    vec))
	   
	  
