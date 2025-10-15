
;; modelled on ccdproc

(in-package imred)


(defun %fringecor-image-median-function  (im median-nsample statsec)
  (compute-image-median-in-statsec im median-nsample statsec))



(defun fringecor-one-extension (ff-out ff-fringe extdesc
				    saturation-val
				    invalid-func
				output-null-val)
  (declare (type (function (single-float) t) invalid-func))
  (let* ((read-type :float)
         ;(write-type :float)
         (saturation-val (float saturation-val 1.0))
         (output-null-val (float output-null-val 1.0))
         (imsec
          (progn
            (cf:move-to-extension ff-out (extdesc-n-ext extdesc))
            (cf:read-image-section  ff-out  :type read-type)))
         (imsec-fringe
	   (progn
	     (cf:move-to-extension ff-fringe (extdesc-n-ext extdesc))
	     (cf:read-image-section  ff-fringe  :type read-type)))
	 (data (cf:image-section-data imsec))
	 (ndat (array-total-size data))
	 (data-copy (imutils:copy-image data))
	 (diff-img (imutils:copy-image data))
	 (data-fringe (cf:image-section-data imsec-fringe))
	 (rough-median
	   (%fringecor-image-median-function data 30000 (extdesc-statsec extdesc)))
	 ;; use small fraction of pixels to estimate median
	 (i-med-sparse 100)
	 (nmed (floor ndat i-med-sparse))
	 ;; vector of indices of pixels used to sample median
	 (pvec (make-array nmed :element-type 'fixnum))
	 (mvec (make-array nmed :element-type 'single-float))) ;; scratch
	       
	 
    

    (declare (type imutils:image data data-copy data-fringe diff-img)
	     (type single-float rough-median)
             (type fixnum ndat))

    ;; get rid of all dubious values and replace with median
    (loop for i below ndat
	  for x of-type single-float = (row-major-aref data i)
	  when (or (float-utils:single-float-nan-or-infinity-p x)
		   (> x saturation-val)
		   (funcall invalid-func x))
	    do (setf (row-major-aref data-copy i) rough-median))
    
    ;; replace data-fringe bad values with zero
    (loop for i below ndat
	  for x of-type single-float = (row-major-aref data-fringe i)
	  when (or (float-utils:single-float-nan-or-infinity-p x)
		   (> x saturation-val)
		   (funcall invalid-func x))
	    do (setf (row-major-aref data-fringe i) 0.0))


    
    ;; populate the vector of indices we use to measure median
    ;; quasi-uniformly
    (loop with i of-type fixnum = 0
	  for ipix = (* i (random i-med-sparse))
	  until (= i nmed)
	  when (< ipix ndat)
	    do (setf (aref pvec i) ipix)
	       (incf i))
    (labels
	 ;; compute the median of diff-img using only those indices
	 ;; in pvec
	((compute-median-of-diff-img ()
	   (declare (optimize speed))
	   (loop for i below nmed
		 do (setf (aref mvec i)
			  (row-major-aref diff-img (aref pvec i))))
	   #+nil ;; delete me
	   (loop for i below (array-total-size mvec)
		 when (float-utils:float-nan-or-infinity-p (aref mvec i))
		   do (format t "Element ~A is ~A~%" i (aref mvec i)))
	   
	   (fastmedian:fast-single-float-1d-array-median mvec))
	 ;; 
	 ;; define A=(data_copy - cf * fringe)  and return
	 ;; the total absolute deviation of A - median(A).
	 (compute-median-abs-fringe-dev (cf)
	   (declare (type single-float cf)
		    (optimize speed))
	   (loop for i of-type (unsigned-byte 32) below ndat 
		 for x = (row-major-aref data-copy i)
		 for f = (row-major-aref data-fringe i)
		 for y =  (- x (* cf f))
		 when (or (float-utils:float-nan-or-infinity-p x)
			  (float-utils:float-nan-or-infinity-p f)
			  (float-utils:float-nan-or-infinity-p y))
		   do (format t "Element ~A x=~A f=~A y=~A~%" i x f y)
		 do (setf (row-major-aref diff-img i) y))
	   (*
	    (/ 1.0 ndat) ;; just to have the right scaling
	    (loop with med of-type single-float = (compute-median-of-diff-img)
		  for i of-type (unsigned-byte 32) below ndat 
		  sum (abs (- (row-major-aref diff-img i) med))
		    of-type single-float)))
	 ;; 
	 ;; make a pass from cf1 to cf2 rescaling by r, returning the best
	 ;; value of cf
	 (make-cf-pass (cf1 cf2 r)
	   (declare (type single-float cf1 cf2 r))
	   (loop
	     with cf-best = 0d0 and dev-best = 1d100
	     for cf = cf1 then (* r cf)
	     until (> cf cf2)
	     for dev = (compute-median-abs-fringe-dev cf)
	     when (< dev dev-best)
	       do (setf dev-best dev)
		  (setf cf-best cf)
	     finally (return (values cf-best dev-best)))))

      (let (cfp1 dp1 cfp2 dp2 cfp3 dp3)
	;; make a rough pass, incrementing by 1.15
	(multiple-value-setq (cfp1 dp1)
	  (make-cf-pass 1.0 1e5 1.15))
	;; make a fine pass starting one notch the best value,
	;; incrementing by 1.01
	(multiple-value-setq (cfp2 dp2)
	  (make-cf-pass (/ cfp1 1.15) (* cfp1 1.15) 1.01))
	;; now a final pass with 1.001 steps, for 0.1% precision
	(multiple-value-setq (cfp3 dp3)
	  (make-cf-pass (/ cfp2 1.01) (* cfp2 1.01) 1.001))
	;; now cfp2 has the best (to 1%) fringe correction
	;;
	;; apply it and write back to file
	(loop for i below ndat
	      for x of-type single-float = (row-major-aref data i)
	      for fcor of-type single-float
		= (* cfp3 (row-major-aref data-fringe i))
	      if (not (or (float-utils:float-nan-or-infinity-p x)
			  (> x saturation-val)
			  (funcall invalid-func x)))
		do (setf (row-major-aref data i)
			 (- x fcor))
	      else
		do (setf (row-major-aref data i)
		    output-null-val))
	;;
	(cf:write-back-image-section imsec)
	(cf:write-fits-header ff-out "IMRED.FRINGECOR" t)
	(cf:write-fits-header ff-out "IMRED.FRINGEVAL" cfp3)))))
			      




(defun fringecor (fits fits-out fringe-fits
		  &key 
		    (reduction-plan *default-reduction-plan*)
		    (if-exists :error))

  "De-fringe FITS using FRINGE-FITS, placing output into FITS-OUT."

  (declare (type (member :error :supersede) if-exists))

  (when (probe-file fits-out)
    (if (eq if-exists :error)
        (error "Output file ~A exists" fits-out)
        (delete-file fits-out)))


  (when (not (probe-file fits)) (error "input file ~A does not exist" fits))
  (when  (not (probe-file fringe-fits))
    (error "Fringe file ~A does not exist" fringe-fits))


  (let ((fits (fullfile fits))
        (fits-out (fullfile fits-out :create t)) 
        (fringe (fullfile fringe-fits)))

    (when (equal fits fits-out)
      (error "output file ~A would overwrite input file ~A" fits-out fits))

    (copy-image fits fits-out :type :float :if-exists :supersede)
    
  ;; now fits-out (not fits) must be compatible with bias and flat
  (when (not (all-images-compatible-p
              (remove nil (list fits fringe))
              :reduction-plan reduction-plan))
    (error "FITS ~A and FRINGE ~A are not compatible" fits fringe))


    (cf:with-open-fits-file (fringe ff-fringe :mode :input)
      (cf:with-open-fits-file (fits-out ff-out :mode :io) 
	(loop 
	  with extdesc-list = (build-extdesc-list-for-fits 
			       fits :reduction-plan reduction-plan)
	  ;;for ihdu from 1 to (cf:fits-file-num-hdus ff-out)
	  for extdesc in extdesc-list
	  for ihdu = (extdesc-n-ext extdesc)
	  when (not (cf:read-fits-header ff-out "IMRED.FRINGECOR" :extension ihdu))
	  do 
	     (cf:move-to-extension ff-out ihdu)
	     (when (extdesc-reduce-p extdesc)
	       (fringecor-one-extension
		ff-out ff-fringe
		extdesc 
		(reduction-plan-saturation-value reduction-plan) 
		(reduction-plan-invalid-pixel-function reduction-plan)
		(reduction-plan-output-null-pixel-value reduction-plan))))))))
  
