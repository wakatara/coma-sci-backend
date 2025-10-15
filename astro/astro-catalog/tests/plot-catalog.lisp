

(in-package astro-catalog-tests)

(defun plot-catalog (astro-catalog &key ra0 dec0 size  pgplot (color :default))
  (let ((p (or pgplot (pgplot:open-device :x11)))
	(ra0 (or ra0 (ra-center astro-catalog)))	
	(dec0 (or dec0 (dec-center astro-catalog)))
	(size (or size (* 2 (radius-deg astro-catalog)))))
    (when (not pgplot)
      (pgplot:set-window p (- (* 0.5 size)) (+ (* 0.5 size))
			 (- (* 0.5 size)) (+ (* 0.5 size)))
      (pgplot:box p))

    (loop for ra across (astro-catalog:get-astro-catalog-vector
			 astro-catalog :ra)
	  for dec across (astro-catalog:get-astro-catalog-vector
			  astro-catalog :dec)
	  do (multiple-value-bind (x y)
		 (sky-project:tan-project ra dec  (* 1d0 ra0) (* 1d0 dec0)
					  :units :degrees)
	       (pgplot:points p x y :filled-circle :size 0.3 :color color)))))
	  
    
;; plot tiling made using %generate-catalog-tiling
(defun plot-tiling (field-diam cat-diam &key (field-shape :circle))
	   (let ((p (pgplot:open-device :x11))
		 (tiling (astro-catalog::%generate-catalog-tiling 
			  field-diam cat-diam :field-shape field-shape)))
	     (pgplot::set-window p 
				 (* -0.6 field-diam) (* 0.6 field-diam) 
				 (* -0.6 field-diam) (* 0.6 field-diam))
	     (pgplot:box p)
	     (pgplot:circle p 0 0 (* 0.5 field-diam) :line-style :dotted)
	     (loop for pair in tiling 
		   do (pgplot:circle p (first pair) (second pair) 
				     (* 0.5 cat-diam)))))
