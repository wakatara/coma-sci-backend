
(in-package cfitsio)



(defun compute-pa-direction-for-wcs (wcs)
  "Given a cfitsio WCS of type RADEC-TAN, return 
 (VALUES PA-ANGLE PA-SENSE) 

If PA=(PAx,PAy) is the vector on the chip that points north, then
PA-ANGLE=(180/PI)*ATAN(PAx,PAy) and PA-SENSE the direction
that the PA vector moves as the PA increases. 

PA-ANGLE is 0 if the PA is north.  PA-SENSE is +1 if the position
angle is counterclockwise (normal) and 1 if it is clockwise.

A North up, East left image should have PA-ANGLE=0, PA-SENSE=+1."
  (declare (type wcs-radec-tan wcs))
  (let (pa-angle pa-sense)
    ;; solve for the xp,yp pixels that the wcs matrix maps to
    ;; (dra,ddec)=(0,1) ie, north
    (multiple-value-bind (xp yp)
	(wcs::%2x2-lin-solve-macro 
	 (wcs-radec-tan-cd1_1 wcs) 
	 (wcs-radec-tan-cd1_2 wcs) 
	 (wcs-radec-tan-cd2_1 wcs) 
	 (wcs-radec-tan-cd2_2 wcs)  
	 0d0 1d0)
      ;; note that (0,1)=>0  and  (1,0)=>90
      (setf pa-angle (* (/ 180 pi) (atan xp yp))))
    ;;
    ;; now the det of the matrix determines sense
    (let ((det (- (* (wcs-radec-tan-cd1_1 wcs) 
		     (wcs-radec-tan-cd2_2 wcs))
		  (*
		   (wcs-radec-tan-cd1_2 wcs) 
		   (wcs-radec-tan-cd2_1 wcs) ))))
      (setf pa-sense (if (minusp det) +1 -1)))
    ;;
    (values pa-angle pa-sense)))



(defun wcs-pixel-direction-vector-for-PA (wcs pa)
  "Return (VALUES XHAT YHAT) for a position angle PA in degrees, and a
wcs-radec-tan WCS, representing the unit vector in pixel space that
points in direction PA."
  (declare (type wcs-radec-tan wcs)
	   (type real pa))
  (let* ((parad (* pa (/ pi 180)))
	 (cospa (cos parad))
	 (sinpa (sin parad))
	 ;; the unit vector in ra,dec space - note that small positive
	 ;; PA turns points +north, +east
	 (dra sinpa)
	 (ddec cospa))
    ;; the pixel deltas required to create dra,ddec
    (multiple-value-bind (dx dy)
	(wcs::%2x2-lin-solve-macro 
	 (wcs-radec-tan-cd1_1 wcs) 
	 (wcs-radec-tan-cd1_2 wcs) 
	 (wcs-radec-tan-cd2_1 wcs) 
	 (wcs-radec-tan-cd2_2 wcs)  
	 dra ddec)
      ;; normalize
      (let* ((r (sqrt (+ (expt dx 2) (expt dy 2))))
	     (xhat (/ dx r))
	     (yhat (/ dy r)))
	(values xhat yhat)))))
	
		    
  
  