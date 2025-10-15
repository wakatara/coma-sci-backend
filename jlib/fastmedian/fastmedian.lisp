#|

Fast O(n) median search: an ANSI C implementation
Nicolas Devillard
citing algorithm by Niklaus Wirth

see http://ndevilla.free.fr/median/median.pdf and wirth.c

Reference:
   Author: Wirth, Niklaus
   Title: Algorithms + data structures = programs
   Publisher: Englewood Cliffs: Prentice-Hall, 1976
   Physical description: 366 p.
   Series: Prentice-Hall Series in Automatic Computation

Empirically, it runs in O(N) time, and 1 billion points takes
15sec (2010 CPU) whether in sets of 1e4 or 1e5

Curiously, it's just a bit faster on a modern 2025 x86 CPU, but
2x faster on an M2.

It is about 10x slower than a simple average.



|#

(defpackage #:fastmedian
  (:use #:common-lisp)
  (:export
   #:fast-double-float-1d-array-median
   #:fast-single-float-1d-array-median
   #:fast-double-float-2d-array-median
   #:fast-single-float-2d-array-median
   ;;
   #:fast-double-float-1d-array-fraction
   #:fast-single-float-1d-array-fraction
   #:fast-double-float-2d-array-fraction
   #:fast-single-float-2d-array-fraction
   ))

(in-package fastmedian)

;; a macro to build a specialized function to take the median
;; of an array of type array-type, containing elements of type num-type
(defmacro %make-fast-specialized-median-function 
    (func-name array-type num-type &key (docstring "Fast median; rearranges input array"))
  `(progn
    (declaim (#+sbcl sb-ext:maybe-inline 
	      #+cmu ext:maybe-inline 
	      #-(or sbcl cmu) inline
              ,func-name))
    (defun ,func-name (v &optional nv)
      ,docstring
      (declare (type ,array-type v)
	       (type (or null (unsigned-byte 28)) nv)
	       (optimize  speed))

      (flet
	  ;;
	  ((find-index-of-next-largest-or-equal-element (v k nv)
	     (declare (type ,array-type v)
		      (type (unsigned-byte 28) k)
		      (type (or null (unsigned-byte 28)) nv)
		      (optimize speed))
	     (loop
	      with n of-type (unsigned-byte 28) = (or nv 
						      (array-total-size v))
	      ;;default val - nothing bigger
	      with ibest of-type (unsigned-byte 28) = k 
	      with x of-type ,num-type = (row-major-aref v k)
	      with xbest of-type ,num-type = 
	         ,(cond ((eq num-type 'cl:double-float)
			 most-positive-double-float)
			((eq num-type 'cl:single-float)
			 most-positive-single-float)
			(t
			 (describe num-type)
			 (error 
			  "NUM_TYPE is ~A but must be double-float or single-float"
			  num-type)))
	      with xx of-type ,num-type = ,(coerce 0d0 num-type)
	      for i below n
	      when (not (= i k))
	      do
	      (setf xx (row-major-aref v i))
	      (when (and (>= xx x) (< xx xbest))
		(setf xbest xx)
		(setf ibest i))
	      finally (return ibest)))
	   ;;
	   (wirth-index-of-kth-smallest (v k &optional nv)
	     (declare (type ,array-type v)
		      (type (unsigned-byte 28) k)
		      (type (or null (unsigned-byte 28)) nv)
		      (optimize speed))
	     (let ((i 0) (j 0) (l 0) (m 0) (x ,(coerce 0d0 num-type))
		   (n (or nv (array-total-size v))))
	       (declare (type (signed-byte 28) i j l m n)
			(type ,num-type x))
	       (when (>= k n) 
		 (error "k=~D is too big; must be <~D" k n))
	       (setf m (1- n))
	       (loop
		while (< l m)
		do
		(setf x (row-major-aref v k))
		(setf i l)
		(setf j m)
		(loop
		 do
		 (loop while (< (row-major-aref v i) x) do (incf i))
		 (loop while (< x (row-major-aref v j)) do (decf j))
		 (when (<= i j)
		   (rotatef (row-major-aref v i) (row-major-aref v j))
		   (incf i)
		   (decf j))
		 while 
		 (<= i j))
		(when (< j k) (setf l i))
		(when (< k i) (setf m j)))
	       ;;
	       k)))
	   
	
	(let ((n (or nv (array-total-size v)))
	      (k1 0)
	      (k2 0))
	  (declare (type (unsigned-byte 28) k1 k2))
	  (cond ((= n 0)
		 (error "Cannot take median of a set of 0 elements"))
		((= n 1)
		 (row-major-aref v 0))
		((oddp n)
		 (row-major-aref 
		  v 
		  (wirth-index-of-kth-smallest v  (ash n -1) nv)))
		(t
		 (setf k1 (wirth-index-of-kth-smallest v (1- (ash n -1)) nv))
		 (setf k2 (find-index-of-next-largest-or-equal-element v k1 nv))
		 (/ (+ (row-major-aref v k1) 
		       (row-major-aref v k2))
		    2))))))))



 
(defmacro %make-fast-specialized-fraction-function 
    (func-name array-type num-type 
     &key (docstring "Fast array fraction using O(N) algorithm; rearranges input array"))
  (declare (type (member single-float double-float) num-type))
  (let ((%zero     (if (eq num-type 'single-float) 0e0 0d0))
	(%bigplusfloat (if (eq num-type 'single-float)
			   most-positive-single-float
			   most-positive-double-float)))
    `(progn
       (declaim (#+sbcl sb-ext:maybe-inline 
		 #+cmu ext:maybe-inline 
		 #-(or sbcl cmu) inline
		 ,func-name))
       (defun ,func-name (v frac &optional nv)
	 ,docstring
	 (declare (type ,array-type v)
		  (type (or single-float double-float) frac)
		  (type (or null (unsigned-byte 28)) nv)
		  (optimize  speed))

	 (flet
	     ;;
	     ((find-index-of-next-largest-or-equal-element (v k nv)
		(declare (type ,array-type v)
			 (type (unsigned-byte 28) k)
			 (type (or null (unsigned-byte 28)) nv)
			 (optimize speed))
		(loop
		  with n of-type (signed-byte 28) = (or nv 
							(array-total-size v))
		  ;;default val - nothing bigger
		  with ibest of-type (unsigned-byte 28) = k 
		  with x of-type ,num-type = (row-major-aref v k)
		  with xbest of-type ,num-type = ,%bigplusfloat
		  with xx of-type ,num-type = ,%zero
		  for i below n
		  when (not (= i k))
		    do
		       (setf xx (row-major-aref v i))
		       (when (and (>= xx x) (< xx xbest))
			 (setf xbest xx)
			 (setf ibest i))
		  finally (return ibest)))
	      ;;
	      (wirth-index-of-kth-smallest (v k &optional nv)
		(declare (type ,array-type v)
			 (type (unsigned-byte 28) k)
			 (type (or null (unsigned-byte 28)) nv)
			 (optimize speed))
		(let ((i 0) (j 0) (l 0) (m 0) (x ,%zero)
		      (n (or nv (array-total-size v))))
		  (declare (type (signed-byte 28) i j l m n)
			   (type ,num-type x))
		  (when (>= k n)
		    (error "k=~D is too big; must be <~D" k n))
		  (setf m (1- n))
		  (loop
		    while (< l m)
		    do
		       (setf x (row-major-aref v k))
		       (setf i l)
		       (setf j m)
		       (loop
			 do
			    (loop while (< (row-major-aref v i) x) do (incf i))
			    (loop while (< x (row-major-aref v j)) do (decf j))
			    (when (<= i j)
			      (rotatef (row-major-aref v i) (row-major-aref v j))
			      (incf i)
			      (decf j))
			 while 
			 (<= i j))
		       (when (< j k) (setf l i))
		       (when (< k i) (setf m j)) )

		  ;;
		  k)))
	   
	   (when (zerop (array-total-size v))
	     (error "Cannot take frac of array of zero elements."))
	   (let* ((n (or nv (array-total-size v)))
		  (frac (if (typep frac 'single-float) ;; convoluted to avoid compiler notes
			    (* frac 1d0)
			    frac))
		  (f (* 1.0d0 (1- n) frac)) ;; double
		  (ff 0d0)		    ;; double
		  (bot 0)
		  (k1 0)
		  (k2 0))
	     (declare (type (unsigned-byte 28) n bot k1 k2)
		      (type (double-float  0d0 1d99) f)
		      (type (double-float 0d0 1d0) frac ff))
	     (cond ((= n 0)
		    (error "Cannot take fraction of a set of 0 elements"))
		   ((= n 1)
		    (row-major-aref v 0))
		   (t
		    (multiple-value-setq (bot ff) (floor f))
		    (setf k1 (wirth-index-of-kth-smallest v bot nv))
		    (setf k2 (find-index-of-next-largest-or-equal-element v k1 nv))
		    (let ((answer
			    (+ (* (- 1.0d0 ff) (row-major-aref v k1)) 
			       (* ff (row-major-aref v k2)))))
		      (declare (type double-float answer))
		      ;; coerce answer to type of array
		      (float answer ,(if (eq num-type 'double-float) 1d0 1.0) ))))))))))
      


(%make-fast-specialized-median-function 
  fast-double-float-1d-array-median
  (simple-array double-float (*))
  cl:double-float
 :docstring 
"Quickly compute the median of a double precision 1d array V using Wirth's method -
NV is the optional length of V to use, by default (array-total-size V).
WARNING - this rearranges V")
 
(%make-fast-specialized-median-function 
  fast-single-float-1d-array-median
  (simple-array single-float (*))
  cl:single-float
 :docstring 
"Quickly compute the median of a single precision 1d array V using Wirth's method -
NV is the optional length of V to use, by default (array-total-size V).
WARNING - this rearranges V")



(%make-fast-specialized-median-function 
  fast-double-float-2d-array-median
  (simple-array double-float (* *))
  cl:double-float
 :docstring 
"Quickly compute the median of a double precision 2d array V using Wirth's method -
NV is the optional length of V to use, by default (array-total-size V).
WARNING - this rearranges V")

(%make-fast-specialized-median-function 
  fast-single-float-2d-array-median
  (simple-array single-float (* *))
  cl:single-float
 :docstring 
"Quickly compute the median of a single precision 2d array V using Wirth's method -
NV is the optional length of V to use, by default (array-total-size V).
WARNING - this rearranges V")






(%make-fast-specialized-fraction-function 
  fast-double-float-1d-array-fraction
  (simple-array double-float (*))
  cl:double-float
 :docstring 
"Quickly compute the fraction of a double precision 1d array V using Wirth's method -
NV is the optional length of V to use, by default (array-total-size V).
WARNING - this rearranges V")

(%make-fast-specialized-fraction-function 
  fast-single-float-1d-array-fraction
  (simple-array single-float (*))
  cl:single-float
 :docstring 
"Quickly compute the fraction of a single precision 1d array V using Wirth's method -
NV is the optional length of V to use, by default (array-total-size V).
WARNING - this rearranges V")



(%make-fast-specialized-fraction-function 
  fast-double-float-2d-array-fraction
  (simple-array double-float (* *))
  cl:double-float
 :docstring 
"Quickly compute the fraction of a double precision 1d array V using Wirth's method -
NV is the optional length of V to use, by default (array-total-size V).
WARNING - this rearranges V")

(%make-fast-specialized-fraction-function 
  fast-single-float-2d-array-fraction
  (simple-array single-float (* *))
  cl:single-float
 :docstring 
"Quickly compute the fraction of a single precision 2d array V using Wirth's method -
NV is the optional length of V to use, by default (array-total-size V).
WARNING - this rearranges V")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; testing

#|
(defparameter *v100*
  (loop with n = 100
       with v = (make-array n :element-type 'single-float)
       for i below n do (setf (row-major-aref v i) (float i 1.0))
       finally 
       (loop for i below n do 
	    (rotatef (aref v (random n))
		     (aref v (random n))))
       (return v)))

(defparameter *v101*
  (loop with n = 100
       with v = (make-array n :element-type 'single-float)
       for i below n do (setf (row-major-aref v i) (float i 1.0))
       finally 
       (loop for i below n do 
	    (rotatef (aref v (random n))
		     (aref v (random n))))
       (return v)))


(eval-when (load eval compile)
  (require 'stats))

(defun test-fastmedian ()
  (format t "V[0..99]: median: ~F fastmedian: ~F~%"
	  (stats:median-of-elements *v100*)
	  (fastmedian:fast-single-float-1d-array-median
	   (copy-seq *v100*)))
  (format t "V[0..99]: 11%: ~F fast-fraction: ~F~%"
	  (stats:fraction-of-elements *v100* 0.11)
	  (fastmedian:fast-single-float-1d-array-fraction
	   (copy-seq *v100*) 0.11))
  (format t "V[0..99]: 87%: ~F fast-fraction: ~F~%"
	  (stats:fraction-of-elements *v100* 0.87)
	  (fastmedian:fast-single-float-1d-array-fraction
	   (copy-seq *v100*) 0.87))
  ;;
  (format t "V[0..100]: median: ~F fastmedian: ~F~%"
	  (stats:median-of-elements *v101*)
	  (fastmedian:fast-single-float-1d-array-median
	   (copy-seq *v101*)))
  (format t "V[0..100]: 11%: ~F fast-fraction: ~F~%"
	  (stats:fraction-of-elements *v101* 0.11)
	  (fastmedian:fast-single-float-1d-array-fraction
	   (copy-seq *v101*) 0.11))
  (format t "V[0..100]: 87%: ~F fast-fraction: ~F~%"
	  (stats:fraction-of-elements *v101* 0.87)
	  (fastmedian:fast-single-float-1d-array-fraction
	   (copy-seq *v101*) 0.87)))
			      
|#
