
;; routines for bootstrap resampling

;; note that is is a bit SLOW because the SBCL random number 
;; generator conses a lot.

(defpackage #:bootstrap
  (:use #:common-lisp)
  (:export
   #:resample-double-float-vector
   #:resample-single-float-vector
   #:resample-median-bounds/double-float
   #:resample-median-bounds/single-float
   #:resample-one-sided-median-dev/double-float
   #:resample-one-sided-median-dev/single-float
   #:resample-one-sided-mean-dev/double-float
   #:resample-one-sided-mean-dev/single-float
   #:compute-significance-of-deviation-from-x0 ;; only double-float version
   ))


(in-package bootstrap)

(defun resample-double-float-vector (v &key target n)
  "Produce a random resampling of double-float vector V.  If TARGET
is given, it must be a double-float vector, and it is used as the
destination.  If N is given and TARGET is not given, then the output
vector produced has length N, rather than the default value of (length
V) - values in V in the resampled output may be repeated."
  (declare (type (simple-array double-float (*)) v)
	   (type (or null (simple-array double-float (*))) target)
	   (type (or null (unsigned-byte 28)) n)
	   (optimize speed))
  (loop
     with target of-type (simple-array double-float (*))
       = (or target
	     (when n (make-array n :element-type 'double-float))
	     (copy-seq v))
     with n of-type (unsigned-byte 28) = (length target)
     for i of-type (unsigned-byte 28) below n
     do
       (setf (aref target i) (aref v (random n)))
     finally
       (return target)))


(defun resample-single-float-vector (v &key target n)
  "Produce a random resampling of double-float vector V.  If TARGET
is given, it must be a single-float vector, and it is used as the
destination.  If N is given and TARGET is not given, then the output
vector produced has length N, rather than the default value of (length
V) - values in V in the resampled output may be repeated."
  (declare (type (simple-array single-float (*)) v)
	   (type (or null (simple-array single-float (*))) target)
	   (type (or null (unsigned-byte 28)) n)
	   (optimize speed))
  (loop
     with target of-type (simple-array single-float (*))
       = (or target
	     (when n (make-array n :element-type 'single-float))
	     (copy-seq v))
     with n of-type (unsigned-byte 28) = (length target)
     for i of-type (unsigned-byte 28) below n
     do
       (setf (aref target i) (aref v (random n)))
     finally
       (return target))) 




 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resample-median-bounds/double-float (v nresamp
			       &key
			       (fbot 0.15865525393145696d0)
			       (ftop 0.15865525393145696d0))
  "Resample the values in double-float vector V a total of NRESAMP
times.  Each time, accumulate the median, and finally return the three
values OBSERVED-MEDIAN BOTTOM-MEDIAN-BOUND TOP-MEDIAN-BOUND, such that
the resampled median is less than BOTTOM-MEDIAN-BOUND fraction FBOT of
occurrences, and greater than TOP-MEDIAN-BOUND fraction FTOP of
occurrences.  The default values of FTOP and FBOT are 0.1587, or one
sigma.  V is not modified."
  (declare (type (simple-array double-float (*)) v))
  (loop
     with vmed = (make-array nresamp :element-type 'double-float)
     with vv = (make-array (length v) :element-type 'double-float)
     for i of-type (unsigned-byte 28) below nresamp
     do (setf (aref vmed i)
	      (fastmedian:fast-double-float-1d-array-median
	       (resample-double-float-vector v :target vv)))
     finally
     ;; copy v to vv to take v's median without rearranging it
       (loop for i of-type (unsigned-byte 28) below (length v)
	  do (setf (aref vv i) (aref v i)))
       (return
	 (values
	  (fastmedian:fast-double-float-1d-array-median vv)
	  (fastmedian:fast-double-float-1d-array-fraction 
	   vmed (float fbot 1d0))
	  (fastmedian:fast-double-float-1d-array-fraction 
	   vmed (- 1d0 (float ftop 1d0)))))))

(defun resample-median-bounds/single-float (v nresamp
			       &key
			       (fbot 0.15865525393145696e0)
			       (ftop 0.15865525393145696e0))
  "Resample the values in single-float vector V a total of NRESAMP
times.  Each time, accumulate the median, and finally return the three
values OBSERVED-MEDIAN BOTTOM-MEDIAN-BOUND TOP-MEDIAN-BOUND, such that
the resampled median is less than BOTTOM-MEDIAN-BOUND fraction FBOT of
occurrences, and greater than TOP-MEDIAN-BOUND fraction FTOP of
occurrences.  The default values of FTOP and FBOT are 0.1587, or one
sigma.  V is not modified."
  (declare (type (simple-array single-float (*)) v))
  (loop
     with vmed = (make-array nresamp :element-type 'single-float)
     with vv = (make-array (length v) :element-type 'single-float)
     for i of-type (unsigned-byte 28) below nresamp
     do (setf (aref vmed i)
	      (fastmedian:fast-single-float-1d-array-median
	       (resample-single-float-vector v :target vv)))
     finally
     ;; copy v to vv to take v's median without rearranging it
       (loop for i of-type (unsigned-byte 28) below (length v)
	  do (setf (aref vv i) (aref v i)))
       (return
	 (values
	  (fastmedian:fast-single-float-1d-array-median vv)
	  (fastmedian:fast-single-float-1d-array-fraction 
	   vmed (float fbot 1e0))
	  (fastmedian:fast-single-float-1d-array-fraction 
	   vmed (- 1e0 (float ftop 1e0)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       


(defun resample-one-sided-median-dev/double-float (v nresamp &key (frac 0.5d0))
  "Resample the values in double-float vector V a total of NRESAMP
times.  Each time, accumulate the absolute difference between the
observed median of V and the median of the resampled vector.  Finally
return the two values OBSERVED-MEDIAN ABS-DEVIATION.  The deviation
returned is the value such that fraction FRAC of the resampled
deviations are smaller than the returned value."
  (declare (type (simple-array double-float (*)) v))
  (loop
     with median of-type double-float = 0d0
     with vmed = (make-array nresamp :element-type 'double-float)
     with vv = (make-array (length v) :element-type 'double-float)
     initially ;; copy v to vv and compute median
       (loop for i of-type (unsigned-byte 28) below (length v)
	  do (setf (aref vv i) (aref v i)))
       (setf median (fastmedian:fast-double-float-1d-array-median vv))
     for i of-type (unsigned-byte 28) below nresamp
     do (setf (aref vmed i)
	      (abs
	       (- median
		  (fastmedian:fast-double-float-1d-array-median
		   (resample-double-float-vector v :target vv)))))
     finally
     ;; copy v to vv to take v's median without rearranging it
       (loop for i of-type (unsigned-byte 28) below (length v)
	  do (setf (aref vv i) (aref v i)))
       (return
	 (values
	  median
	  (fastmedian:fast-double-float-1d-array-fraction
	   vmed (float frac 1d0))))))

(defun resample-one-sided-median-dev/single-float (v nresamp &key (frac 0.5e0))
  "Resample the values in single-float vector V a total of NRESAMP
times.  Each time, accumulate the absolute difference between the
observed median of V and the median of the resampled vector.  Finally
return the two values OBSERVED-MEDIAN ABS-DEVIATION.  The deviation
returned is the value such that fraction FRAC of the resampled
deviations are smaller than the returned value."
  (declare (type (simple-array single-float (*)) v))
  (loop
     with median of-type single-float = 0e0
     with vmed = (make-array nresamp :element-type 'single-float)
     with vv = (make-array (length v) :element-type 'single-float)
     initially ;; copy v to vv and compute median
       (loop for i of-type (unsigned-byte 28) below (length v)
	  do (setf (aref vv i) (aref v i)))
       (setf median (fastmedian:fast-single-float-1d-array-median vv))
     for i of-type (unsigned-byte 28) below nresamp
     do (setf (aref vmed i)
	      (abs
	       (- median
		  (fastmedian:fast-single-float-1d-array-median
		   (resample-single-float-vector v :target vv)))))
     finally
     ;; copy v to vv to take v's median without rearranging it
       (loop for i of-type (unsigned-byte 28) below (length v)
	  do (setf (aref vv i) (aref v i)))
       (return
	 (values
	  median
	  (fastmedian:fast-single-float-1d-array-fraction
	   vmed (float frac 1e0))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resample-one-sided-mean-dev/double-float (v nresamp &key (frac 0.5d0))
  "Resample the values in double-float vector V a total of NRESAMP
times.  Each time, accumulate the absolute difference between the
observed mean of V and the mean of the resampled vector.  Finally
return the two values OBSERVED-MEAN ABS-DEVIATION.  The deviation
returned is the value such that fraction FRAC of the resampled
deviations are smaller than the returned value."
  (declare (type (simple-array double-float (*)) v))
  (flet ((mean-of-vector (vec)
	   (loop with sum of-type double-float = 0d0
		 for x across vec
		 do (incf sum x)
		 finally (return (/ sum (length vec))))))
  (loop
     with mean of-type double-float = (mean-of-vector v)
     with vmean = (make-array nresamp :element-type 'double-float)
     with vv = (make-array (length v) :element-type 'double-float)
     for i of-type (unsigned-byte 28) below nresamp
     do (setf (aref vmean i)
	      (abs
	       (- mean
		  (mean-of-vector
		   (resample-double-float-vector v :target vv)))))
     finally
	;; copy v to vv to take v's mean without rearranging it
	(return
	  (values
	   mean
	   (fastmedian:fast-double-float-1d-array-fraction
	    vmean (float frac 1d0)))))))


(defun resample-one-sided-mean-dev/single-float (v nresamp &key (frac 0.5e0))
  "Resample the values in single-float vector V a total of NRESAMP
times.  Each time, accumulate the absolute difference between the
observed mean of V and the mean of the resampled vector.  Finally
return the two values OBSERVED-MEAN ABS-DEVIATION.  The deviation
returned is the value such that fraction FRAC of the resampled
deviations are smaller than the returned value."
  (declare (type (simple-array single-float (*)) v))
  (flet ((mean-of-vector (vec)
	   (loop with sum of-type single-float = 0e0
		 for x across vec
		 do (incf sum x)
		 finally (return (/ sum (length vec))))))
  (loop
     with mean of-type single-float = (mean-of-vector v)
     with vmean = (make-array nresamp :element-type 'single-float)
     with vv = (make-array (length v) :element-type 'single-float)
     for i of-type (unsigned-byte 28) below nresamp
     do (setf (aref vmean i)
	      (abs
	       (- mean
		  (mean-of-vector
		   (resample-single-float-vector v :target vv)))))
     finally
	;; copy v to vv to take v's mean without rearranging it
	(return
	  (values
	   mean
	   (fastmedian:fast-single-float-1d-array-fraction
	    vmean (float frac 1e0)))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun compute-significance-of-deviation-from-x0
    (v x0 &key (nresamp-max 10000) (ncross-wanted 25) (ncross-floor 1))
  "Use bootstrapping to answer the question 'How likely is it that
that median of V is really X0 or beyond?'

Given a double float vector V and a number X0, compute
m0=median(Vi-X0), and compute using resampling the probability that the
m0 is really on the opposite side of X0. For instance, if
X0 is zero, this computes the probability that V is significantly non-zero.
 
NRESAMP-MAX is the maximum number of resamplings to do.  NCROSS-WANTED is the number
of X0 crossings to stop at, unless NMAX is surpassed.  The fractional
error in the probability will be sqrt(NCROSS-WANTED).

NCROSS-FLOOR is a floor value to impose on NCROSS so that the value
of NCROSS is the MAX(NCROSS-SEEN, NCROSS-FLOOR) - this is so we never
report a zero probability.  It can be changed to zero.

Returns 
  (VALUES MEDIAN-OF-V  PROB-CROSS-X0  ERR-PROB-CROSS-X0 NRESAMP NCROSS)

where  
  
  PROB-CROSS-X0  = NCROSS / NRESAMP-DONE
  ERR-PROB-CROSS-X0 = sqrt(NCROSS) / NRESAMP-DONE  (beware small NCROSS)
"

  (declare (type (simple-array double-float (*)) v)
	   (type (integer 0) ncross-floor))
  
  (loop 
     with ncross of-type (unsigned-byte 28) = 0
     with nresamp of-type (unsigned-byte 28) = 0
     with vv = (make-array (length v) :element-type 'double-float)
     with med of-type double-float = 0.d0
     initially ;; copy v-X0 to vv 
       (loop for i of-type (unsigned-byte 28) below (length v)
	  do (setf (aref vv i) (- (aref v i) x0)))
       (setf med (fastmedian:fast-double-float-1d-array-median vv))
     until (or (= ncross ncross-wanted)
	       (= nresamp nresamp-max))
     do
       (resample-double-float-vector v :target vv)
       (let ((this-med (fastmedian:fast-double-float-1d-array-median vv)))
	 ;; when 0 is between med and this-med, there's a crossing
	 (when (or (and (plusp this-med) (minusp med))
		   ;; note use of (not (minusp ..), not (plusp ..)
		   (and (minusp this-med) (not (minusp med)))) 
		(incf ncross)))
       (incf nresamp)
     finally
       ;; impose a floor on ncross so we never report a zero probability (unless
       ;; user set NCROSS-FLOOR to zero)
       (setf ncross (max ncross ncross-floor))
       ;;
       (return 
	 (values
	  med 
	  (float (/ ncross nresamp) 1d0) ;; prob-cross-x0
	  (float (/ (sqrt ncross) nresamp) 1d0) ;; err-prob-cross-x0
	  nresamp
	  ncross))))
	       
	       
	       
		
       
