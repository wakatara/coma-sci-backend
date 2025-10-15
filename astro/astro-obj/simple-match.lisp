
#|

Simple matching code that takes vectors of ra1 dec1 ra2 dec2 and matches them by indices



|#


(in-package astro-obj)





(defun match-ra-dec-by-index (ra1-vec dec1-vec ra2-vec dec2-vec &key (tol/arcsec 1.0)
				      (nx 200) (ny 200))
  "Match objects in RA1-VEC,DEC1-VEC and RA2-VEC,DEC2-VEC using grids, returning
a vector of indices of (LENGTH RA1-VEC) pointing to objects in RA2-VEC,DEC2-VEC
or -1 when no matching object is found."
  (let (ra0 dec0 radius
	ixyobj1-list ixyobj2-list
	grid2
	(out-index-array (make-array (length ra1-vec) :element-type 'fixnum)))
    (multiple-value-setq (ra0 dec0 radius)
      (find-bounding-radec-circle-for-ra-dec-vecs ra1-vec dec1-vec))
    
    ;; convert the input ra,dec to IXYOBJ objects where the ID is the
    ;; array index
    (flet ((to-ixyobj-list (ravec decvec)
	     (loop 
	       with out-obj
	       for ra across ravec and dec across decvec
	       for i from 0
	       do (multiple-value-bind (x y)
		      (sky-project:tan-project 
		       (float ra 1d0) (float dec 1d0) ra0 dec0 :units :arcsec)
		    (setf out-obj
			  (make-ixyobj :id i :x x :y y)))
	       collect out-obj)))
      (setf ixyobj1-list (to-ixyobj-list ra1-vec dec1-vec))
      (setf ixyobj2-list (to-ixyobj-list ra2-vec dec2-vec))

      ;; create bins of the 2nd object
      (setf grid2 (bin-objects ixyobj2-list :nx nx :ny ny))
      
      ;; match objects in list1 to grid2, putting index in out-index-array
      (loop for i below (length ra1-vec)
	    for ixyobj in ixyobj1-list
	    for nearest-obj2 = (aobj:get-nearest-xyobject ixyobj grid2 (float tol/arcsec 1d0))
	    do (setf (aref out-index-array i)
		     (if nearest-obj2 
			 (ixyobj-id nearest-obj2)
			 -1)))
      ;; 
      out-index-array)))
			 


(defun pairwise-match-astro-objects (aobj1-seq aobj2-seq &key (tol/arcsec 1.0) (nx 200) (ny 200))
  "Pairwise match astro-objects in AOBJ1-SEQ, AOBJ2-SEQ using their
RA,DEC, returning a list of pairs ((AOBJ1 AOBJ2) ...)"
  (let* ((aobj1vec (coerce aobj1-seq 'vector))
	 (aobj2vec (coerce aobj2-seq 'vector))
	 (ra1vec (map '(simple-array double-float (*)) 'obj-alpha aobj1-seq))
	 (dec1vec (map '(simple-array double-float (*)) 'obj-delta aobj1-seq))
	 (ra2vec (map '(simple-array double-float (*)) 'obj-alpha aobj2-seq))
	 (dec2vec (map '(simple-array double-float (*)) 'obj-delta aobj2-seq))
	 (match-indices (match-ra-dec-by-index ra1vec dec1vec  ra2vec dec2vec 
					       :nx nx :ny ny :tol/arcsec tol/arcsec)))
    (loop for k across match-indices
	  for obj1 across aobj1vec
	  when (not (minusp k)) ;; -1 means 'no match'
	    collect (list obj1 (aref aobj2vec k)))))
	

#|
;; simple test of match-ra-dec-by-index - matches vectors of ra,dec
;; and reversed vectors 
;;
 (defun test-match-by-index (&key (n 100))
  (let ((ra1v (make-array n :element-type 'double-float))
	(dec1v (make-array n :element-type 'double-float))
	(ra2v (make-array n :element-type 'double-float))
	(dec2v (make-array n :element-type 'double-float)))
    (loop with ra0 = 100d0 and dec0 = -20d0
	  for i below n
	  for ra = (+ ra0 (random 0.5d0))
	  for dec = (+ dec0 (random 0.5d0))
	  do (setf (aref ra1v i) ra
		   (aref ra2v i) ra
		   (aref dec1v i) dec
		   (aref dec2v i) dec))
    (incf (aref dec2v 1) 0.1d0) ;; mess up one point
    (match-ra-dec-by-index ra1v dec1v (reverse ra2v) (reverse dec2v))))


 (defun test-pairwise-match (&key (n 100))
  (let ((aobj1-list nil)
	(aobj2-list nil))
    (loop with ra0 = 100d0 and dec0 = -20d0
	  for i below n
	  for ra = (+ ra0 (random 0.5d0))
	  for dec = (+ dec0 (random 0.5d0))
	  do (push (make-obj :alpha ra :delta dec :id i) aobj1-list)
	     (push (make-obj :alpha ra :delta dec :id (+ i 1000)) aobj2-list))
    ;;
    (pairwise-match-astro-objects aobj1-list (reverse aobj2-list))))
	     



|#


