
(in-package astro-catalog)



;; merge SAME types of catalogs
(defun merge-catalogs-around-ra-dec (catalog-list ra0 dec0 r/deg)
  "Given a set of catalogs of the same class, merge them based on
uniqeness of catalog object ID, keeping all objects that are within
R/DEG of RA0,DEC0"

  (let ((id-hash (make-hash-table :test 'equalp))
	(%ra0 (float ra0 1d0))
	(%dec0 (float dec0 1d0))
	(%r/deg (float r/deg 1d0))
	(cat-class (type-of (first catalog-list))))

    (when (zerop (length catalog-list)) (error "No catalogs in CATALOG-LIST"))
    (when (not (every (lambda (c)
			(and (typep c 'astro-catalog)
			     (eq (type-of c) cat-class)))
		      catalog-list))
      (error "Every item in CATALOG-LIST is not an astro-catlog of same type"))
    
    
    ;; fill the hash with unique IDs
    (loop
      for cat in catalog-list
      for ra-vec =  (astro-catalog:get-astro-catalog-vector cat :ra)
      for dec-vec = (astro-catalog:get-astro-catalog-vector cat :dec)
      for id-vec  = (astro-catalog:get-astro-catalog-vector cat :id)
      do (loop 
	   for ra across ra-vec and dec across dec-vec
	   for id across id-vec
	   when (<= (astro-coords:sky-angle
		     (float ra 1d0) (float dec 1d0) %ra0 %dec0 :units :degrees)
		    %r/deg)
	     do
		(setf (gethash id id-hash) t)))
    ;;
    (let* ((n (hash-table-count id-hash))
	   (cat1 (first catalog-list))
	   (data (map 'vector
		      (lambda (v)
			(make-array n :element-type (array-element-type v)))
		      (astro-catalog:astro-catalog-data cat1)))	   
	   (cat-out (make-instance
		     cat-class
		     :n n
		     :data data
		     :ra-center %ra0
		     :dec-center %dec0
		     :fields (astro-catalog-fields cat1)
		     :available-mags (available-mags cat1)
		     :%map (%map cat1)
		     :radius-deg %r/deg)))
      ;; copy the data over
      (loop
	with k = 0
	for cat-src in catalog-list
	for data-src  = (astro-catalog-data cat-src)
	for id-vec  = (astro-catalog:get-astro-catalog-vector cat-src :id)
	do (loop for id across id-vec
		 for j from 0
		 when (gethash id id-hash)
		   do (remhash id id-hash) ;; use this object only once
		      (loop for v across data
			    for v-src across data-src
			    do (setf (aref v k) (aref v-src j)))
		      (incf k)))
      ;;
      cat-out)))
			    
	
	    
			
