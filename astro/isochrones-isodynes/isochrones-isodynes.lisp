(in-package isochrones-isodynes)

#|

Make isochrones and isodynes

IN PROGRESS

|#

(defstruct iso ;; isochrone or isodyone
  type     ;; :ISOCHRONE or :ISODYNE
  mjd      ;; mjd of observation
  ra0 dec0 ;; coords of center at mjd
  ;;
  ;; Each of the following vectors has one element per dust particle.
  ;;   For an isochrone, DT-VEC has the same values, the time in sec
  ;;     relative to MJD0 the dust was emitted.
  ;;   For an isodyne, BETA-VEC has the same values, and DT-VEC varies.
  dt-vec 
  beta-vec
  ;;
  ra-vec
  dec-vec
  xsky-vec ;; arcseconds, in tan projected coords
  ysky-vec
  )


;; advance a PV vector by DT.  Units are km and s
(defun kepler-advance-pv-vector (pv beta dt)
  (declare (type (simple-array double-float (6)) pv)
	   (type double-float beta dt))
  ;; mu is effective mass times G in km units
  (let ((mu (* (- 1d0 beta) units:cgs-g units:cgs-msun 1d-15)))
    (declare (type double-float mu))
    (kepler-orbit:advance-orbit mu pv dt)))


;; given comet elements of a comet, an initial time mjd0, a relative
;; vector of emission times (mjd0+dt/(24*3600)), and a vector of dust betas,
;; and return
;;
;;  (VALUES RA-COMET  DEC-COMET
;;          RA-DUST-VEC
;;          DEC-DUST-VEC
;;          DRA-DUST-VEC (distance from nucleus at mjd0, arcsec)
;;          DDEC-DUST-VEC (distance from nucleus, arcsec)
;;  )
(defun advance-dust-from-comet-pv (elem mjd0 dt-vec  beta-vec
				   &key (observatory "MKO"))
  (declare (type orbital-elements:comet-elem elem)
	   (type double-float mjd0)
	   (type (simple-array double-float (*)) dt-vec beta-vec))
  (let* ((n (length dt-vec))
	 (ra-comet 0d0)
	 (dec-comet 0d0)
	 (ra-dust-vec   (make-array n :element-type 'double-float))
	 (dec-dust-vec  (make-array n :element-type 'double-float))
	 (dra-dust-vec  (make-array n :element-type 'double-float))
	 (ddec-dust-vec  (make-array n :element-type 'double-float))
	 (pv-comet (make-array 6 :element-type 'double-float))
	 (pv-dust  (make-array 6 :element-type 'double-float))
	 (obs (or (observatories:get-observatory observatory)
		  (error "Could not get observatory ~A" observatory)))
	 (obs-elong (- (observatories:observatory-wlongitude obs)))
	 (obs-lat   (observatories:observatory-latitude obs))
	 (obs-altitude  (observatories:observatory-altitude obs))
	 (elem-pert
	   (slalib-ephem:perturb-comet-elem mjd0 elem)))

    (multiple-value-setq (ra-comet dec-comet)
      (slalib-ephem:compute-radecr-from-comet-elem
       elem-pert mjd0 obs-elong obs-lat  
       :perturb nil
       :altitude obs-altitude))

    (loop for i below n
	  for dt-last of-type double-float = 0d0 then dt
	  for dt across dt-vec
	  for beta across beta-vec
	  for mjd-emit = (+ mjd0 (/ dt 24 3600))
	  do
	     ;; recompute initial comet location if changed
	     (when (or (= i 0) ;; first time
		       (not (= dt-last dt))) ;; different start time of emission
	       (slalib-ephem:compute-pv-from-comet-elem
		elem-pert mjd-emit
		:pv pv-comet :units :km
		:correct-mjd-to-tt t))
	     ;;
	     ;; copy comet pv to dust pv
	     (loop for i below 6
		   do (setf (aref pv-dust i) (aref pv-comet i)))
	     ;; advance the dust's orbit to MJD0 from MJD-EMIT
	     (kepler-advance-pv-vector pv-dust  beta (- dt))
	     ;; compute the ra,dec at the final mjd
	     (multiple-value-bind (ra-dust dec-dust)
	       (slalib-ephem:compute-radecr-from-pv
		mjd0 obs-elong obs-lat pv-dust
		:units :km :altitude obs-altitude)
	       (setf (aref ra-dust-vec i) ra-dust)
	       (setf (aref dec-dust-vec i) dec-dust)
	       (multiple-value-bind (x y)
		   (sky-project:tan-project ra-dust dec-dust ra-comet dec-comet
					    :units :arcsec)
		 (setf (aref dra-dust-vec i) x)
		 (setf (aref ddec-dust-vec i) y))))

    (values ra-comet dec-comet ra-dust-vec dec-dust-vec
	    dra-dust-vec ddec-dust-vec)))

(defun to-dbl-vec (v)
  (if (typep v '(simple-array double-float (*)))
      v
      (map '(simple-array double-float (*)) (lambda (x) (float x 1d0)) v)))

(defun build-isochrone (elem mjd dt beta-vec &key (observatory "geocenter"))
  "Build an isochone as an ISO structure, for comet elements ELEM, at time MJD,
emitted at time DT (seconds) relative to MJD - ie, DT should be negative.
BETA-VEC is the vectors of dust beta to be considered."
  (declare (type orbital-elements:comet-elem elem)
	   (type double-float mjd)
	   (type real dt)
	   (type vector beta-vec))
  (let ((dt-vec (make-array (length beta-vec) :element-type 'double-float
					      :initial-element (float dt 1d0)))
	(beta-vec (to-dbl-vec beta-vec)))
    (multiple-value-bind
	  (ra-comet dec-comet ra-dust-vec dec-dust-vec
	   dra-dust-vec ddec-dust-vec)
	(advance-dust-from-comet-pv elem mjd dt-vec beta-vec
				    :observatory observatory)
      (make-iso
       :type :isochrone
       :mjd (float mjd 1d0)
       :ra0 ra-comet :dec0 dec-comet	   
       :dt-vec dt-vec
       :beta-vec beta-vec
       :ra-vec ra-dust-vec
       :dec-vec dec-dust-vec
       :xsky-vec dra-dust-vec
       :ysky-vec ddec-dust-vec))))


(defun build-isodyne (elem mjd dt-vec beta &key (observatory "geocenter"))
    "Build an isodyne as an ISO structure, for comet elements ELEM, at time MJD,
emitted at times in DT-VEC (seconds) relative to MJD - ie, DT should be negative.
BETA is the one value of dust beta to be considered."
  (declare (type orbital-elements:comet-elem elem)
	   (type double-float mjd)
	   (type vector dt-vec)
	   (type real beta))
  (let ((beta-vec (make-array (length dt-vec) :element-type 'double-float
					      :initial-element (float beta 1d0)))
	(dt-vec (to-dbl-vec dt-vec)))
    (multiple-value-bind
	  (ra-comet dec-comet ra-dust-vec dec-dust-vec
	   dra-dust-vec ddec-dust-vec)
	(advance-dust-from-comet-pv elem mjd dt-vec beta-vec
				    :observatory observatory)
      (make-iso
       :type :isodyne
       :mjd (float mjd 1d0)
       :ra0 ra-comet :dec0 dec-comet	   
       :dt-vec dt-vec
       :beta-vec beta-vec
       :ra-vec ra-dust-vec
       :dec-vec dec-dust-vec
       :xsky-vec dra-dust-vec
       :ysky-vec ddec-dust-vec))))




#+nil ;; routine to test the advance function on just the orbit
(defun testit (elem mjd0 dmjd &key (observatory "MKO") (perturb t))
  (let* ((elem-use
	   (if perturb (slalib-ephem:perturb-comet-elem mjd0 elem) elem))
	 (pv0 (slalib-ephem:compute-pv-from-comet-elem
	       elem-use mjd0 
	       :units :km 
	       :correct-mjd-to-tt t))
	 (pv1 (slalib-ephem:compute-pv-from-comet-elem
	       elem-use (- mjd0 dmjd)
	       :units :km
	       :correct-mjd-to-tt t))
	 (pv2 (copy-seq pv1))
	 (obs (observatories:get-observatory observatory))
	 (obs-elong (- (observatories:observatory-wlongitude obs)))
	 (obs-lat   (observatories:observatory-latitude obs))
	 (obs-altitude  (observatories:observatory-altitude obs)))
    
    (kepler-advance-pv-vector
     pv2 0d0 (* dmjd 24 3600))
    
    ;;    (print (list pv0 pv1 pv2))
   
    (multiple-value-bind (ra-comet dec-comet)
      (slalib-ephem:compute-radecr-from-comet-elem
       elem-use mjd0 obs-elong obs-lat  
       :perturb nil 
       :altitude obs-altitude)
      (format t "Normal method: RA=~,6F   DEC=~,6F~%" ra-comet dec-comet))
      
    (terpri)
    (flet ((print-radec (pv where)
	     (multiple-value-bind (ra dec)
		 (slalib-ephem:compute-radecr-from-pv
		  mjd0 obs-elong obs-lat pv
		  :units :km :altitude obs-altitude)
	       (format t "~A: RA=~,6F DEC=~,6F~%" where ra dec))))
      (print-radec pv0 "Original PV")
      (print-radec pv2 "Kepler PV"))))

		
	       
		  
	 
					
