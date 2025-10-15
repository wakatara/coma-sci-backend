

(in-package imutils)



(defun find-hottest-block (data nb &key ix0 ix1 iy0 iy1)
"Find the location of the hottest integer-boundary block of NBxNB
pixels, where NB is odd.  IX0,IX1,IY0,IY1 are the inclusive bounding
indices of the region to search.  

Returns (VALUES IX IY AVERAGE-IN-BLOCK)"
  (declare (type (simple-array single-float (* *)) data)
	   (type (unsigned-byte 20) nb)
	   (type (or null (unsigned-byte 20)) ix0 ix1 iy0 iy1)
	   (optimize speed))
  (when (not (oddp nb))
    (error "Block size NB=~A should be odd" nb))
  (let* ((nb/2 (ash nb -1))
	 (nx (1- (array-dimension data 1)))
	 (ny (1- (array-dimension data 0)))
	 (ix0 (or ix0 0))
	 (ix1 (or ix1 nx))
	 (iy0 (or iy0 0))
	 (iy1 (or iy1 ny)))
    ;;
    (when (< ix1 ix0) (rotatef ix0 ix1))
    (when (< iy1 iy0) (rotatef iy0 iy1))
    (when (or (< ix0 0) (> ix1 nx) (< iy0 0) (> iy1 ny))
      (error "bounds IX0=~A IX1=~A IY0=~A IY`=~A outaide valid array indices IX=~A...~A IY=~A...~A"
	     ix0 ix1 iy0 iy1 0 nx 0 ny))
    ;;
    (flet ((get-val-at-block (mx my)
	     (loop 
	      with nused of-type (unsigned-byte 20) = 0
	      with sum of-type single-float = 0.0
	      for ix of-type (signed-byte 20) from (max 0 (- mx nb/2)) to (min nx (+ mx nb/2))
	      do
	      (loop 
	       for iy of-type (signed-byte 20) from (max 0 (- my nb/2)) to (min ny (+ my nb/2))
	       do
	       (incf sum (aref data iy ix))
	       (incf nused 1))
	      finally
	      (return (/ sum nused)))))
      ;;
      (loop
       with ixbest = 0 and iybest = 0 
       with bestval of-type single-float = most-negative-single-float
       for ix of-type (signed-byte 20) from ix0 to ix1
       do
       (loop 
	for iy of-type (signed-byte 20) from iy0 to iy1
	for val of-type single-float = (get-val-at-block ix iy)
	when (> val bestval)
	do
	(setf bestval val
	      ixbest ix
	      iybest iy))
       finally
       (return (values ixbest iybest bestval))))))
	
    
    


(defun find-peak (data x0 y0 &key (search-distance 5.0) 
		  (nb 5) (nq 5) (errors :const))
  "Given an image DATA and a position X0,Y0, first find the minimum
using the hottest block of size B within +/-SEARCH-DISTANCE +/-RADIUS
using smoothing block of size NB.  Then perform a local quadratic fit
of region size NB at this point.  ERRORS are the same as in FIT-QUADRATIC.

Return (values XFIT YFIT ERRORCODE NXH NYH) 
where ERRORCODE is NIL or the logical OR of

     1 if the final peak is not negative definite, meaning a legitimate peak.  
     2 if the intermediate peak is at edge of search region
     4 if the quadratic peak is more than 1 pix from intermediate peak

NXH,NYH are the intemediate hottest block peak."
  (declare (type image data)
	   (type real x0 y0 search-distance)
	   (type (integer 1 100) nb nq))
  (let ((x0 (float x0 1.0))
	(y0 (float y0 1.0))
	(ix0 (floor   (- x0 search-distance)))
	(ix1 (floor   (+ x0 search-distance)))
	(iy0 (ceiling (- y0 search-distance)))
	(iy1 (ceiling (+ y0 search-distance)))
	(search-distance (float search-distance 1.0)))
    (multiple-value-bind (nxh nyh) ;; hottest block position
	(find-hottest-block data nb 
			    :ix0 ix0 :ix1 ix1
			    :iy0 iy0 :iy1 iy1)
      (multiple-value-bind (xfit yfit sign params)
	  (fit-quadratic data nxh nyh nq :errors errors) 
	(let* ((errorcode
		(logior
		 (if (not (minusp sign)) 1 0)
		 (if (not (and (< ix0 nxh ix1)
			       (< iy0 nyh iy1)))
		     2 0)
		 (if (or (> (abs (- xfit nxh)) 1)
			 (> (abs (- yfit nyh)) 1))
		     4 0))))
	  ;;
	  (values (float xfit 1.0) (float yfit 1.0)
		  (if (zerop errorcode) nil errorcode)
		  nxh nyh))))))
      
