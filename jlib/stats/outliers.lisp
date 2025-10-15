

(in-package stats)

(defun z-score-clip-data (data-seq &key (cutoff 3.5))
  "Perform a Z-SCORE clip of data, rejecting data that ar
      0.6745(xi-median(x))/MAD   > CUTOFF 
where MAD=Median Absolute Deviation.

CUTOFF is 3.5 by default (see source below)

Return (VALUES KEPT-POINTS-VECTOR KEEP-VECTOR)
where KEEP-VECTOR is of same size as data seq
and has T where the point was kept.

See http://www.itl.nist.gov/div898/handbook/eda/section3/eda35h.htm
"
  (let* ((v (seq-to-dbl-vec data-seq))
	 (cutoff (float cutoff 1d0))
	 (n (length v))
	 (dev-vec (make-array n :element-type 'double-float))
	 (median (fast-double-float-vec-median v))
	 (keep-vec (make-array n :initial-element nil))
	 (mad 0d0)
	 (ngood 0)
	 (out-vec nil))
    ;;
    ;; compute abs deviation from median into dev-vec
    (loop for x across v
	  for i from 0
	  do (setf (aref dev-vec i) (abs (- x median))))
    ;;
    (setf mad (fast-double-float-vec-median dev-vec))
    ;;
    ;; when  0.6745(xi-median(x))/MAD < 3.5 flag it in keep-vec
    (loop for dev across dev-vec
	  for i from 0
	  when (< (/ (* 0.6745d0 dev) mad) cutoff)
	  do (setf (aref keep-vec i) t)
	     (incf ngood))
    ;;
    ;; where keep-vec is T, collect data from input vector
    (setf out-vec (make-array ngood :element-type 'double-float))
    (loop with j = 0
	  for good across keep-vec
	  for i from 0
	  when good
	    do (setf (aref out-vec j) (aref v i))
	       (incf j))
    ;;
    (values out-vec keep-vec median mad)))
    
    
   
