#|

A shell sort using Tokuda's series 1,4,9,20,46,103 ...
with unknown worst case  performance, but slightly beating builtin SBCL
sort for at least some short values

If done using Pratt's interval series 1,2,3,4,6,8,9,12 with worst case
N [log(N)]^2 performance, it gives worse result than builtin SORT.

see https://en.wikipedia.org/wiki/Shellsort

This is designed as a macro taking a PREDICATE-EXPRESSION and a SWAPPER-EXPRESSION.

The macro design allows full inlining, and can sort on an inclusive
subset of indices ISTART to IEND.   It might beat native SORT when native SORT
would require a :KEY

Using Tokuda, it seems a bit faster than builtin sort, but with no consing, as long
as swap expression is efficient.

Using Pratt's series, it is 2x slower than builtin SORT.  Thus the
default is to use Tokuda.



In 

 (defmacro shellsort-macro (predicate-expression swapper-expression istart iend ivar jvar)

an example usage is 

(defun test-shellsort-macro (vec &key (istart 0) (iend nil))
  (declare (type simple-vector vec))
  (shellsort-macro (< (aref vec i) (aref vec j))        ;; predicate expression
		   (rotatef (aref vec i) (aref vec j))  ;; swapper expression
		   istart                               ;; first index
		   (or iend (1- (length vec)))          ;; last index (inclusive)
		   i j)                                 ;; variables used
  vec) 

There is also a SHELLSORT-FUNCTION, taking predicate and swapper functions instead of 
expressions, but it is about 4x slower than builtin SORT.

|#

(defpackage shellsort-jk
  (:use #:cl)
  (:export
   #:shellsort-function ;; not really interesting, because it is slower than builtin
   #:shellsort-macro
   #:index ;; type of indices, positive integer 1/2 of most-positive-fixnum
   ))

(in-package shellsort-jk)

;; 3smooth of Pratt 1971 (2^q 3^q)
#+nil
(defun generate-pratt-intervals (nmax)
  (let ((noutlist nil))
    (loop 
      for p from 0
      if (> (expt 2 p) nmax)
	do (return)
      else
      do (loop for q from 0
	       for n = (* (expt 2 p) (expt 3  q))
	       if (> n nmax)
		 do (return)
	       else
		   do (push n noutlist)))
    (sort noutlist '<)))

;; Tokuda series of 1992
(defun generate-tokuda-intervals (nmax)
  (loop for k from 1
	for n = 1 then
		  (ceiling (* 0.20
			    (- (* 9 (expt (/ 9 4.0) (- k 1))) 4)))
	until (> n nmax)
	collect n))

;; Ciura empirical series of 2001, extended by  h_k = floor(2.2*h_(k-1))
#+nil
(defun generate-ciura-intervals (nmax)
  (append '(1 4 10 23 57 132 301 701)
	  (loop for n  = (floor (* 2.25 701)) then (floor (* 2.25 n))
		until (> n nmax)
		collect n)))


;; max index 2x smaller than maximum fixnum to avoid warnigns about
;; integer math at 
(deftype index ()
  '(unsigned-byte #.(- (floor (log most-positive-fixnum 2)) 1)))

(deftype index-array ()
  '(simple-array index (*)))

 
(defparameter *max-n* (floor most-positive-fixnum  2))

;; interval sequences for shellsort


(defparameter *interval-vector*
  (map 'index-array
       'identity
       ;; Pratt is slow, but proven to be bound by N log(N)^2
       #+nil(generate-pratt-intervals *max-n*)   
       ;; Tokuda seems fastest for shortest vectors
       (generate-tokuda-intervals *max-n*)
       ;; Ciuia ostensibly fastest; purely empirical; seems to lose to
       ;; Tokuda for N~10
       #+nil(generate-ciura-intervals *max-n*)   
       ))


(declaim (type index-array *interval-vector*)
	 (type index *max-n*))

(declaim (inline find-starting-interval))

;; this vector grows so swiftly that a linear search makes sense
(defun find-starting-interval (n)
  (declare (type index n)
	   (optimize (speed 3) (safety 0)))
  (loop with v = *interval-vector*
	for i of-type index from 0
	when (> (aref v i) n)
	  do (return (1- i))
	finally (error "N too large for shellsort interval vector.")))


		   

(declaim  (inline shellsort))

(defun shellsort-function (predicate swapper istart iend)
  "Function to run shellsort using PREDICATE and SWAPPER functions, from indices
from ISTART to IEND, inclusive.

 (funcall predicate i j) tests whether i<j
 (funcall swapper   i j) swaps item at indices i,j"

  (declare (type index istart iend)
	   (optimize speed))
  (when (> iend istart)  ;; not sorting vector of length 0
    (loop
      with %predicate of-type function = (if (symbolp predicate) (fdefinition predicate) predicate)
      with %swapper of-type function  = (if (symbolp swapper) (fdefinition swapper) swapper)
      with %interval-vector of-type index-array = *interval-vector*
      with iend* of-type index =  (- iend istart)
      with kint0 of-type index = (find-starting-interval iend*)
      for kint  of-type fixnum from kint0 downto 0
      for interval of-type index
	= (aref %interval-vector kint)
      do (loop
	   for m of-type fixnum below interval
	   do (loop
		for j of-type fixnum from (+ m interval) to iend*
		by interval
		do (loop
		     with i of-type fixnum = j
		     while (and (>= i interval)
				(funcall %predicate
					 (+ istart i)
					 (+ istart (- i interval))))
		     do
			(funcall %swapper (+ istart i) (+ istart (- i interval)))
			(setf i (- i interval))))))))
				    


			  
			 
(defmacro shellsort-macro (predicate-expression swapper-expression istart iend ivar jvar)
  `(let ((%istart ,istart)
	  (%iend ,iend))
     (declare (type index %istart %iend))
     (when (> %iend %istart) ;; not sorting vector of length 0
       (let* ((%iend* (- %iend %istart))
	      (%kint0 (find-starting-interval %iend*))
	      (%interval-vector *interval-vector*))
	 (declare (type index %iend* %kint0))
	 (loop
	   for %kint  of-type fixnum from %kint0 downto 0
	   for %interval of-type index
	     = (aref %interval-vector %kint)
	   do (loop
		for %m of-type fixnum below %interval
		do (loop
		     for %j of-type fixnum from (+ %m %interval) to %iend*
		     by %interval
		     do (loop
			  with %i of-type fixnum = %j
			  for ,ivar of-type fixnum = (+ %istart %i)
			  for ,jvar of-type fixnum = (+ %istart (- %i %interval))
			  while (and (>= %i %interval)
				     ,predicate-expression)
			  do
			  ,swapper-expression
			   (setf %i (- %i %interval))))))))))

  
	
  
  

