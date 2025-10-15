

(in-package imutils)

(defun transform-image-to-wavelet-space (image &key (ndaub 4))
  "transform an image to wavelet space, creating a copy padded to size 2^n - this
operation is NON-DESTRUCTIVE because it involves copying the image to a power of 2.
NDAUB is the Daubechies number, 4, 12, or 20."
  (declare (type image image)
	   (type (member 4 12 20) ndaub))
  (let* ((impadded (nrwavelets:pad-2d-sngl-array-2^n image))
	 (imw (nrwavelets:wavelet-transform-single-float impadded ndaub)))
    imw))


(defun transform-image-from-wavelet-space (imagew &key (original-image nil) (destructive nil) (ndaub 4))
  "Transform wavelet-space image IMAGEW back into real space. If ORIGINAL-IMAGE is an
image, then trim to its size.  If DESTRUCTIVE is T, then do the wavelet transform in-place
on IMAGEW. NDAUB is the Daubechies number, 4, 12, or 20."
  (declare (type image imagew)
	   (type (or null image) original-image)
	   (type (member 4 12 20) ndaub))
  ;;
  (let ((imagew-use (if destructive imagew 
			(copy-image imagew))))

    (nrwavelets:wavelet-transform-single-float imagew-use ndaub nil)
    ;; now trim if necessary and return answer
    (if (not original-image)
	imagew-use ;; no trimming
	(nrwavelets:trim-2d-sngl-array 
	 imagew-use 
	 (array-dimension original-image 0) (array-dimension original-image 1)))))



#+nil ;; plot 1d wavelets so we think we know how they're laid out
(defun wavelet-plot (&key  (n 16) (ndaub 4))
  (let ((v (make-array n :element-type 'single-float ))
	(xvec (make-array n :initial-contents (loop for i below n collect i)))
	(p (pgplot:open-device :x11)))
    (loop for i below n
	  do
	  (pgplot:set-current-pane p 1 1 (1+ i) n  :y-separation 0.02)
	  (pgplot:box p :x-num-labels-bottom nil :y-num-labels-left nil
		      :x-major-ticks nil :x-minor-ticks nil :y-major-ticks nil :y-minor-ticks nil)
	  (pgplot:set-window p 0 (1- n) -0.5 0.5)
	  (fill v 0.0) 
	  (setf (aref v i) 1.0)
	  (nrwavelets:wavelet-transform-single-float v ndaub  nil)
	  (pgplot:connect p xvec v))))
	  


;; wavelets seem to be laid out as 
;; - first 2 indices are widest wavelet - really the 'mother function coefficients'
;; - next  2 indices are 2nd widest
;; - next  4 indices are 3rd widest
;; - next  8 indices are 4th widest
;; - etc 


;; ugly integer log base 2, returning FLOOR of log2(n), useful for getting
;; number of wavelet orders in a transformed array of size 2^n
(defun floor-ilog2 (n)  
  (declare (type (unsigned-byte 28) n)
	   (optimize speed))
  (loop 
   with i of-type (signed-byte 28) = -1
   for j of-type (unsigned-byte 28) = n then (ash j -1)
   while (plusp j) do (incf i) finally (return i)))





(defun compute-index-range-for-wavelet-order (nord)
  "when NORD is the number of a wavelet order, starting at 1, 
return (VALUES I-START I-END), the inclusive array indices containing 
this order"
  (declare (type (unsigned-byte 14) nord)) ;; so that (expt 2 nord) is a fixnum
  (cond ((= nord 1)   ;; first 2 orders are special
	 (values 0 1))
	((= nord 2)
	 (values 2 3))
	(t
	 (values (expt 2 (1- nord))
		 (1- (expt 2 nord))))))
	


(defun compute-starting-index-for-wavelet-order (nord)
  "when NORD is the number of a wavelet order, starting at 1, return the first index
of the wavelet order"
  (nth-value 0 (compute-index-range-for-wavelet-order nord)))

(defun compute-ending-index-for-wavelet-order (nord)
  "when NORD is the number of a wavelet order, starting at 1, return the last index
of the wavelet order"
  (nth-value 1 (compute-index-range-for-wavelet-order nord)))

(defun compute-index-range-for-wavelet-order-range (nord1 nord2)
  "Compute the indices containing wavelet orders NORD1 to NORD2, where NORD1<NORD2"
  (assert (< nord1 nord2))
  (values
   (compute-starting-index-for-wavelet-order nord1)
   (compute-ending-index-for-wavelet-order nord2)))


(declaim (inline compute-wavelet-order-for-index))
(defun compute-wavelet-order-for-index (i)
"Compute the wavelet order (starting at 1) for an index I.  This is ungainly,
but should be fast, requiring 9 comparisons worst case and more typically about 4 -
this is complex but has been verified vs the inverse routine."
  (declare (type fixnum i)
	   (optimize speed))
  (cond
    ((< i #.(expt 2 14))
     (cond ((< i #.(expt 2 7))
	    (cond 
	      ((< i 2)   1)
	      ((< i 4)   2)
	      ((< i 8)   3)
	      ((< i 16)  4)
	      ((< i 32)  5)
	      ((< i 64)  6)
	      ((< i 128) 7)))
	   (t ;; > 2^7 but < 2^14
	    (cond
	      ((< i 256)             8)
	      ((< i 512)             9)
	      ((< i 1024)            10)
	      ((< i #.(expt 2 11))   11)
	      ((< i #.(expt 2 12))   12)
	      ((< i #.(expt 2 13))   13)
	      ((< i #.(expt 2 14))   14)))))
    (t ;; i>= 2^14
     (cond ((< i #.(expt 2 21))
	    (cond
	      ((< i #.(expt 2 15))  15)
	      ((< i #.(expt 2 16))  16)
	      ((< i #.(expt 2 17))  17)
	      ((< i #.(expt 2 18))  18)
	      ((< i #.(expt 2 19))  19)
	      ((< i #.(expt 2 20))  20)
	      ((< i #.(expt 2 21))  21)))
	   ;; else i>=2^23
	   (t
	    (cond	     
	      ((< i #.(expt 2 22))   22)
	      ((< i #.(expt 2 23))   23)
	      ((< i #.(expt 2 24))   24)
	      ((< i #.(expt 2 25))   25)
	      ((< i #.(expt 2 26))   26)
	      ((< i #.(expt 2 27))   27)
	      ((< i #.(expt 2 28))   28)
	      (t (error "Index too big"))))))))


(defun wavelet-hipass-filter (imagew nxkill nykill)
  "High-pass filter a wavelet-space image IMAGEW, deleting the NXKILL,NYKILL
largest scale X,Y components.  An image of dimension Nx has log2(Nx) x-scales"
  (declare (type image imagew)
	   (type (unsigned-byte 28) nxkill nykill))
  (let* ((nx (array-dimension imagew 1))	
	 (ny (array-dimension imagew 0))
	 ;; ixkeep, iykeep are the starting indices of the next highest order after nxkill
	 (ixkeep (compute-starting-index-for-wavelet-order (1+ nxkill)))
	 (iykeep (compute-starting-index-for-wavelet-order (1+ nykill))))
    (declare (type (unsigned-byte 28) nx ny ixkeep iykeep ))
    ;;(format t "nx=~A ixkeep=~A ny=~A iykeep=~A~%" nx ixkeep ny iykeep)
    (locally
	(declare (optimize speed))
      (loop for iy below ny
	    do
	    (loop for ix below nx
		  do
		  (when (not (and (>= ix ixkeep) (>= iy iykeep)))
		    (setf (aref imagew iy ix) 0.0)))))))
	 
	 

(defun zero-selected-wavelet-scales (imagew nordx1 nordx2 nordy1 nordy2 &key (operator :and))
  "Zero out wavelet orders (starting at 1) such that
     (NORDX1<=NORDX<=NORDX2)  OPERATOR  (NORDY1<=NORDY<=NORDY2)
where OPERATOR is either :AND or :OR."
  (declare (type image imagew)
	   (type (unsigned-byte 28) nordx1 nordx1 nordy1 nordy2)
	   (type (member :and :or) operator))
  (assert (> nordx2 nordx1))
  (assert (> nordy2 nordy1))
  (let* ((nx (array-dimension imagew 1))	
	 (ny (array-dimension imagew 0))
	 ;;
	 (ix1  (compute-starting-index-for-wavelet-order nordx1))
	 (ix2  (compute-ending-index-for-wavelet-order nordx2))
	 (iy1  (compute-starting-index-for-wavelet-order nordy1))
	 (iy2  (compute-ending-index-for-wavelet-order nordy2)))
	 ;;
    (declare (type (unsigned-byte 28) nx ny ix1 ix2 iy1 iy2))
    ;;
    (locally
	(declare (optimize speed))
      (cond
	;;
	((eq operator :and)
	 (loop for iy below ny
	       do
	       (loop for ix below nx
		     when
		     (and 
		      (and (>= ix ix1) (<= ix ix2))
		      (and (>= iy iy1) (<= iy iy2)))
		     do
		     (setf (aref imagew iy ix) 0.0))))
	((eq operator :or)
	 (loop for iy below ny
	       do
	       (loop for ix below nx
		     when
		     (or
		      (and (>= ix ix1) (<= ix ix2))
		      (and (>= iy iy1) (<= iy iy2)))
		     do
		     (setf (aref imagew iy ix) 0.0))))))))
    
	 
(defun wavelet-filter-by-function-of-scales (imagew func-of-iy-ix)
  "Scale coefficients in wavelet image IMAGEW according to float
function FUNC-OF-IX-IY, which takes wavelet orders IY,IX as arguments,
and returns a float scaling factor"
  (declare (type image imagew)
	   (type (or function symbol) func-of-iy-ix))
  (let* ((nx (array-dimension imagew 1))	
	 (ny (array-dimension imagew 0))
	 (func (if (symbolp func-of-iy-ix)
		   (symbol-function func-of-iy-ix)
		   func-of-iy-ix)))
	 ;;
    (declare (type (unsigned-byte 28) nx ny)
	     (type function func))
    ;;
    (locally
	(declare (optimize speed))
      (loop 
	 for iy below ny
	 for nordy = (compute-wavelet-order-for-index iy)  
	 do
	   (loop 
	      for ix below nx
	      for nordx = (compute-wavelet-order-for-index ix)  
	      for scale of-type single-float = (funcall func nordy nordx)
	      do
	      (setf (aref imagew iy ix)
		    (* scale (aref imagew iy ix))))))))
	 
	 	 
	 
	 


 

    
  



	  
	   