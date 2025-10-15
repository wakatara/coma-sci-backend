
#|

Some diagnostics:

   plot-calibration-match-file - plot a match file that is output by various calibration routines

   catalog-vs-catalog-comparison - compare two catalogs against each other
 

|#


(in-package phot-calib)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun plot-calibration-match-file
    (match-csv-file &key (device :x11) (filename nil)
		      (zp-mag nil) ;; if given plot it
		      (dmag-span 0.25) (mag-min nil) (mag-max nil)
		      (toplabel nil))
  "Plot a file with fields 
 OBJ_ID OBJ_MAG OBJ_MAG_ERR   CAT_ID CAT_MAG CAT_MAG_ERR DMAG DMAG_ERR  RA DEC"
  (let* ((h (csv-read:read-csv-headers/columns-from-file match-csv-file
							 :comment-char #\#))
	 (catmag (gethash "CatMag" h)) 
	 (dmag   (gethash "Dmag" h)) 
	 (dmag-err (gethash "DmagErr" h)) 
	 (dmag-med (stats:median-of-elements dmag))
	 (mag-min (or mag-min (+ -0.5 (stats:min-of-elements catmag))))
	 (mag-max (or mag-max (+ +0.5 (stats:max-of-elements catmag))))
	 (p nil))

    (unwind-protect ;; if this bombs make sure we close device,
		    ;; because this might be run in an automated loop
	 (progn (setf p (pgplot:open-device device :filename filename))
		;; top hi-res pane
		(flet ((make-plot (mspan iy label &key (labels-bottom t))
			 (pgplot:set-current-pane p  1 1 iy 2)
			 (pgplot:set-window p mag-min mag-max (- dmag-med mspan) (+ dmag-med mspan))
			 (pgplot:box p :size 1.5 :x-num-labels-bottom labels-bottom)
			 
			 (pgplot:with-window (p 0 1 0 1)
			   (pgplot:write-text p label 0.8 0.9 :character-height 1.2)
			   ;;
			   (when labels-bottom ;; only bottom panel
			     (pgplot:draw-plot-legend
			      p 0.6 0.2
			      `((,(format nil "Median Mag ~,3F" dmag-med)
				 :line :line-style :dotted :color :light-gray)
				(,(format nil "ZP Mag ~,3F" (or zp-mag "NA"))
				 :line :line-style :dotted :color :red))
			      :draw-box t :character-height 0.9)))
			 
			 (when labels-bottom (pgplot:xlabel p "Reference Magnitude"))
			 (pgplot:ylabel p "M\\dref\\u-M\\dimage\\u" :x-offset -0.03)
			 (when toplabel (pgplot:toplabel p toplabel))
			 (pgplot:points p catmag dmag :filled-circle)
			 (pgplot:errorbars p catmag dmag dmag-err :direction :y)
			 
			 (when zp-mag 
			   (pgplot:connect p (vector mag-min mag-max) (vector zp-mag zp-mag) 
					   :line-style :dotted :color :red))
			 (pgplot:connect p (vector mag-min mag-max) (vector dmag-med dmag-med) 
					 :line-style :dotted :color :light-gray)))
		  ;;
		  (make-plot  dmag-span       1  "Zoom view"   :labels-bottom t)
		  (make-plot  (* 7 dmag-span) 2  "Broad view"  :labels-bottom nil)))

      (progn
	(when filename (pgplot:close-device p))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; catalog vs catalog comparison
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun catalog-vs-catalog-comparison (catalog-ref catalog filter
				      &key (matches-file nil)
				      (extra-error 0.01d0)
				      ;; parameters of bayes-outlie
				      (f 0.1d0) (s 0.15d0))
  "Convert CATALOG to CATALOG-REF's filter, and compute the offset"
  (let* ((cat1 catalog-ref)
	 (cat2 catalog)
	 (match-indices ;; in cat2
	   (aobj:match-ra-dec-by-index  
	    (astro-catalog:get-astro-catalog-vector cat1 :ra)
	    (astro-catalog:get-astro-catalog-vector cat1 :dec)
	    (astro-catalog:get-astro-catalog-vector cat2 :ra)
	    (astro-catalog:get-astro-catalog-vector cat2 :dec)))
	 (transfunc1 ;; translation function from cat1 to filter 
	   (get-mag-trans-func-for-catalog cat1 filter))
	 (transfunc2 ;; translation function from cat1 to filter 
	   (get-mag-trans-func-for-catalog cat2 filter))
	 (ngood
	   (loop for i1 from 0
		 for i2 across match-indices
		 when (and (plusp i2)
			   (funcall transfunc1 cat1 i1)
			   (funcall transfunc2 cat2 i2))
		   count 1))
	 (ra-vec (make-array ngood))
	 (dec-vec (make-array ngood))
	 (id1-vec (make-array ngood))
	 (mag1-vec (make-array ngood))
	 (magerr1-vec (make-array ngood))
	 (id2-vec (make-array ngood))
	 (mag2-vec (make-array ngood))
	 (magerr2-vec (make-array ngood))
	 (dmag-vec (make-array ngood))
	 (dmagerr-vec (make-array ngood)))

    (loop with k = 0
	  for ra across (astro-catalog:get-astro-catalog-vector cat1 :ra)
	  for dec across (astro-catalog:get-astro-catalog-vector cat1 :dec)
	  for i1 from 0
	  for i2 across match-indices
	  when (plusp i2)
	    do
	       (multiple-value-bind (m1 dm1)
		   (funcall transfunc1 cat1 i1)
		 (when m1
		   (multiple-value-bind (m2 dm2) 
		       (funcall transfunc2 cat2 i2)
		     (when m2 
		       (setf (aref ra-vec      k) ra)
		       (setf (aref dec-vec     k) dec)
		       (setf (aref id1-vec     k) (astro-catalog:get-value cat1 :id i1))
		       (setf (aref id2-vec     k) (astro-catalog:get-value cat2 :id i2))
		       (setf (aref mag1-vec    k) m1
			  (aref magerr1-vec k) dm1
			  (aref mag2-vec    k) m2
			  (aref magerr2-vec  k) dm2)
		       (setf (aref dmag-vec  k) (- m1 m2))
		       (setf (aref dmagerr-vec k) 
			     (sqrt (+ (expt extra-error 2) (expt dm1 2) (expt dm2 2))))
		       (incf k))))))

    (when matches-file
      (with-open-file (sout matches-file 
			    :direction :output 
			    :if-does-not-exist :create
			    :if-exists :supersede)
	(format sout "# Matches from catalog/catalog calib
# 1=~A; 2=~A;   Dmag=Mag2-Mag1
#
"
		(type-of catalog) (type-of catalog-ref))
	(format 
	 sout
"# CatID1                CatMag1  CatMagErr1      CatID2                  CatMag2  CatMagErr2     Dmag     DmagErr   RA       DEC~%")
	(loop for ra across ra-vec and dec across dec-vec 
	      for id1 across id1-vec and id2 across id2-vec
	      for m1 across mag1-vec for merr1 across magerr1-vec
	      for m2 across mag2-vec for merr2 across magerr2-vec
	      for dm across dmag-vec for dmerr across dmagerr-vec
	      do (format 
		  sout 
 "~20A   ~7,3F   ~7,3F         ~20A   ~7,3F   ~7,3F      ~7,3F   ~7,3F ~9,5F ~9,5F~%"
   id2 m2 merr2 ;; cat2 is the one one being calibrated
   id1 m1 merr1
   dm dmerr
   ra dec))))

    (let* ((outlier-result (bayes-outlier:bayes-outlier-estimate-mean 
			    dmag-vec dmagerr-vec :f f :s s))
	   (dmag (bayes-outlier:outlier-result-xm outlier-result))
	   (dmag-err (bayes-outlier:outlier-result-xm-err outlier-result)))


      (values dmag dmag-err ngood outlier-result))))
	     

    
