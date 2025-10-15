;; coordinate math


(in-package small-body-identify)

;; coordinate computer struct, with a quaternion for converting
;; geocentric XYZ into RA2000,DEC2000 system
(defstruct ccomp
  (mjdtt 0d0  :type double-float)
  (r-earth nil :type (or null (simple-array double-float (3))))
  (v-earth nil :type (or null (simple-array double-float (3))))
  (quaternion nil :type (or null quaternion:quaternion)))  

;; compute the ra,dec and out3vec for in3vec, which is a 3vector
;; of relative geocentric position to the earth
(defun %compute-radec-of-gcvector (mjdtt rearth in3vec &key (observatory "MKO"))
  ;; h6v is the heliocentric pv vector, formed by adding rearth to in3vec
  (let* ((h6v (make-array 6 :element-type 'double-float :initial-element 0d0))
	 (obs (observatories:get-observatory observatory))
	 (height (observatories:observatory-altitude obs))
	 (elong (* (/ pi 180)
		   (- (observatories:observatory-wlongitude obs))))
	 (lat  (* (/ pi 180)
		  (observatories:observatory-latitude obs))))
	
    (dotimes (i 3) (setf (aref h6v i) (+ (aref in3vec i) (aref rearth i))))
    ;; convert h6v to an ra,dec (at this mjdtt)
    (multiple-value-bind (ra/radians dec/radians)
	(slalib:pv-to-radecr mjdtt elong lat height h6v)

      (multiple-value-bind (ra2000/radians dec2000/radians)
	(slalib:sla-amp ra/radians dec/radians mjdtt 2000d0)
	
	(let ((out3vec 
		(3vec:3vector-from-spherical-coordinates
		 dec2000/radians ra2000/radians 1d0)))
	  (values out3vec
		  (* (/ 180 pi) ra2000/radians)
		  (* (/ 180 pi) dec2000/radians)))))))


(defun compute-conversion-quaternion (mjdtt rearth  &key (observatory "MKO"))
  ;; v1,v2 are the 'far away' vectors used to compute ra,dec of
  ;; directions from earth, and n2,n2 are the unit vectors
  (let* ((v1 (3vec:make-3vec 0d0 10000d0  0d0))
	 (v2 (3vec:make-3vec 0d0  0d0 10000d0))
	 (n1 (3vec:make-3vec 0d0  1d0  0d0))
	 (n2 (3vec:make-3vec 0d0  0d0  1d0))
	 (u1 (%compute-radec-of-gcvector mjdtt rearth v1
					 :observatory observatory))
	 (u2 (%compute-radec-of-gcvector mjdtt rearth v2
					 :observatory observatory)))

    ;; u1,u2 will not be perfectly orthgonal, but close enough
    #+nil
    (format t "u1=~A~%u2=~A~%dot: ~A~%"
	    u1 u2 (3vec:dot-product u1 u2))
    
    (quaternion:compute-quaternion-struct-for-transform
     n1 n2 u1 u2)))



;; this seems to improve the residuals a teeny but, but is not main source
;; of errors
(defun %precess-and-nutate-observatory (mjdtt pvobs)
  (let ((rmat (make-array 9 :element-type 'double-float))
	(3vec1 (make-array 3 :element-type 'double-float))
	(3vec2 (make-array 3 :element-type 'double-float)))
    (slalib:sla-prenut 2000d0 mjdtt rmat) ;; make precession/nutation matrix
    ;; rotate position component
    (dotimes (i 3) (setf (aref 3vec1 i) (aref pvobs i)))
    (slalib:sla-dmxv rmat 3vec1 3vec2)
    (dotimes (i 3) (setf (aref pvobs i) (aref 3vec2 i)))
    ;; rotate velocity component
    (dotimes (i 3) (setf (aref 3vec1 i) (aref pvobs (+ 3 i))))
    (slalib:sla-dmxv rmat 3vec1 3vec2)
    (dotimes (i 3) (setf (aref pvobs (+ 3 i)) (aref 3vec2 i)))))
    



;; this adjustment is missing UT1-UTC adjustment, but we're not so exact
(defun %adjust-rearth-for-observatory (mjdtt rearth &key (observatory "MKO"))
					     
  (let* ((obs (observatories:get-observatory observatory))
	 (elong (* (/ pi -180) (observatories:observatory-wlongitude obs)))
	 (lat   (* (/ pi  180) (observatories:observatory-latitude obs)))
	 (height (observatories:observatory-altitude obs))
	 (st/radians
	   (* (/ (* 2 pi) 24)
	      (slalib:compute-local-sidereal-time mjdtt elong)))
	 ;; PV of observatory 
	 (pvobs (slalib:sla-pvobs lat height st/radians)))
    (%precess-and-nutate-observatory mjdtt pvobs)
    ;; use DECF it matches fortran code (and gives MUCH smaller residuals)
    ;; maybe pvobs is FROM observatory, TO geocenter?
    (loop for i below 3
	  do (decf (aref rearth i) (aref pvobs i)))))

	
;; For an MJDTT, generate a CCOMP structure for converting geocentric
;; vectors to ra,dec system, using the CCOMP-QUATERNION
(defun make-ccomp-for-mjdtt (mjdtt &key  (observatory "MKO"))
  (declare (type double-float mjdtt))
  (multiple-value-bind (veb reb ve re) ;; au/sec and au
      (slalib:sla-epv mjdtt) ;; high precision routine
    (declare (ignore veb reb)) ;; want helio, not bary
    (%adjust-rearth-for-observatory mjdtt re)
    (make-ccomp
     :mjdtt mjdtt
     :v-earth ve :r-earth re
     :quaternion (compute-conversion-quaternion 
		 mjdtt re
		 :observatory observatory))))


(defun compute-ra-dec-delta-using-ccomp (ccomp comet-elem
				   &key
				     mjdtt
				     ;; optional scratch vectors to reduce consing
				     3vec-scratch
				     pv-scratch)
  "Estimate the RA,DEC,DELTA for COMET-ELEM, using MJDTT if given, otherwise using
MJDTT in CCOMP (the MJD for which the CCOMP was made)." 
						      
  (declare (type ccomp ccomp)
	   (type slalib-ephem:comet-elem comet-elem)
	   (type (or null double-float) mjdtt)
	   (type (or null (simple-array double-float (3))) 3vec-scratch)
	   (type (or null (simple-array double-float (6))) pv-scratch)
	   (optimize (speed 3) (safety 1)))
  (let ((pv (slalib-ephem:compute-pv-from-comet-elem    
	     comet-elem 
	     ;; WARNING - not using ccomp-mjdtt is inaccurate because earth is at
	     ;; wrong spot, but might be OK for short intervals, and large Delta
	     (or mjdtt (ccomp-mjdtt ccomp))
	     :pv pv-scratch
	     :correct-mjd-to-tt nil))
	;; the geocentric radius vector of object 
	(geo-robj (or 3vec-scratch (3vec:make-3vec)))
	(re (ccomp-r-earth ccomp)))
    ;;
    (declare (type (simple-array double-float (6)) pv))
    ;;
    (dotimes (i 3)   
      (setf (aref geo-robj i)
	    (- (aref pv i) (aref re i))))
    
    ;; first guess of distance
    (let* ((delta (3vec:3vec-norm geo-robj))
	   (1/clight #.(/ 1d0 0.0020039888042339624d0)) ;; 1/speed of light in au/sec
	   (tlight (* delta 1/clight))) ;; light travel time
      (declare (type double-float delta 1/clight tlight))
      ;; adjust the geo-robj vector to where pv WAS tlight ago
      (dotimes (i 3)
	(decf (aref geo-robj i)
	      (* tlight (aref pv (+ 3 i))))))
    ;;
    (let ((u (quaternion:quaternion-struct-rotate
	      geo-robj (ccomp-quaternion ccomp))))
      (multiple-value-bind (dec/rad ra/rad delta)
	  (3vec:spherical-coordinates-from-3vector u)
	(when (minusp ra/rad) (incf ra/rad (* 2 pi)))
	(values ;; return non-boxed single floats - close enough for this routine
	 (float (* (/ 180 pi) ra/rad) 1.0) 
	 (float (* (/ 180 pi) dec/rad) 1.0)
	 (float delta 1.0)
	 pv)))))

 
