
;; routines for manipulating matrices - emphasis is on double precision

(defpackage matrix
  (:use #:common-lisp)
  (:export
   #:matrix-p #:dbl-matrix-p
   #:square-matrix-p #:symmetric-matrix-p
   #:matrix-nrows #:matrix-ncols
   #:diag-matrix
   #:copy-array #:copy-dbl-array #:copy-matrix #:copy-dbl-matrix
   #:make-matrix-with-function
   #:rescale-array #:rescale-matrix #:rescale-dbl-matrix #:fill-dbl-array
   #:add-arrays #:add-matrices #:add-dbl-matrices
   #:copy-matrix-to-double-precision
   #:matrix-multiply
   #:matrix-multiply-dbl
   #:transpose-matrix #:transpose-matrix-dbl
   #:gauss-jordan-elimination-destructive-dbl
   #:gauss-jordan-elimination
   #:invert-matrix #:invert-matrix-destructive-dbl
   #:svdcmp #:svbksb
   #:make-random-positive-definite-matrix ;; for testing, eg, cholesky
   #:jacobi   ;; eigenvalues and eigenvectors of real symmetric matrix
   #:cholesky
   ))


(in-package matrix)


(declaim (inline matrix-p dbl-matrix-p square-matrix-p symmetric-matrix-p))


(deftype matrix () '(simple-array * (* *)))
(deftype dbl-matrix () '(simple-array double-float (* *)))
(deftype dbl-vector () '(simple-array double-float (*)))
(deftype dbl-array () '(simple-array double-float))
;; integers with which we can do fast math
(deftype nice-signed-integer () 
  (list 'integer (ash most-negative-fixnum -1)  (ash most-positive-fixnum -1)))
(deftype nice-unsigned-integer () 
  (list 'integer 0  (ash most-positive-fixnum -1)))


(defun matrix-p (a) (typep a 'matrix))


(defun dbl-matrix-p (a) (typep a 'dbl-matrix))

(defun square-matrix-p (a)
  (and
   (arrayp a)
   (= (array-rank a) 2)
   (= (array-dimension a 0) (array-dimension a 1))))

(defun symmetric-matrix-p (a)
  (and (square-matrix-p a)
       (block ret
	 (loop 
	    with nx = (array-dimension a 1)
	    with ny = (array-dimension a 0)
	    for ix below nx
	    do
	      (loop
		 for iy from (1+ ix) below ny
	         when (not (= (aref a ix iy) 
			      (aref a iy ix)))
		 do (return-from ret nil)))
	 t)))
		 

(declaim (inline matrix-nrows matrix-ncols))

(defun matrix-nrows (matrix)
  "Number of rows in matrix - (array-dimension matrix 0)"
  (array-dimension matrix 0))
(defun matrix-ncols (matrix)
  "Number of rows in matrix - (array-dimension matrix 1)"
  (array-dimension matrix 1))

(defun make-dbl-matrix (ny nx &key (initial-element 0d0))
  (make-array (list ny nx) :element-type 'double-float :initial-element initial-element))


(defun make-matrix-with-function (ny nx function &key
				  (element-type 'double-float))
  "Make an NY x NX matrix populated by (FUNCTION IY IX)"
  (loop 
     with m = (make-dbl-matrix ny nx)
     for iy below ny
     do
       (loop 
	  for ix below nx
	  do
	    (setf (aref m iy ix) 
		  (coerce (funcall function iy ix) element-type)))
       finally (return m)))
	    

(defun diag-matrix (vec &key (element-type 'double-float))
  "Return a diagonal NxN matrix given a vector of length N"
  (loop 
     with n = (length vec)
     with a = (make-dbl-matrix n n)
     for x across vec
     for i from 0
     do
       (setf (aref a i i) (coerce x element-type))
     finally (return a)))



(defun copy-array (a &key (output-type nil) (target nil))
  "copy an array to an array of the same shape and type specified
by OUTPUT-TYPE - if OUTPUT-TYPE is left NIL, use the same type as A"
  (declare (type array a)
	   (type (or null array) target))
  (when (and target
	     (not (equal (array-dimensions a) (array-dimensions target))))
    (error "Target has bad dimensions"))	
  (loop
   with output-type = (or (if target (array-element-type target))
			  output-type (array-element-type a))
   with aa of-type array =
         (or target
	     (make-dbl-matrix (array-dimension a 0) (array-dimension a 1)))
   with n = (array-total-size a)
   for i below n
   for x = (if (not output-type) ;; avoid coercing if possible
	       (row-major-aref a i)
	     (coerce (row-major-aref a i) output-type))
   do
   (setf (row-major-aref aa i) x)
   finally (return aa)))

(defun copy-matrix (a &key (output-type nil) (target nil))
  "copy a matrix to a matrix of the type specified by OUTPUT-TYPE - if
OUTPUT-TYPE is left NIL, use the same type as A - this is a trivial
wrapper for copy-array"
  (declare (type matrix a))
  (copy-array a :output-type output-type :target target))


(defun copy-dbl-array (a &key target)
  "copy a double-precision array to an array of the same shape and type -
somewhat SLOW because dimensions not known at compile time
- if keyword TARGET is specified,
use it as the target"
  (declare (type dbl-array a)
	   (type (or null dbl-array) target))
  (when (and target
	     (not (equal (array-dimensions a) (array-dimensions target))))
    (error "Target has bad dimensions"))	
  (loop
   with aa of-type dbl-array =
      (or target 
	   (make-dbl-matrix (array-dimension a 0) (array-dimension a 1)))
   with n = (array-total-size a)
   for i below n
   for x of-type double-float = (row-major-aref a i)
   do
   (setf (row-major-aref aa i) x)
   finally (return aa)))

(defun copy-dbl-matrix (a &key target)
  "copy a double precision matrix - if keyword TARGET is specified,
use it as the target"
  (declare (type dbl-matrix a)
	   (type (or null dbl-matrix) target)
	   (optimize speed))
  (when (and target
	     (not (= (array-dimension a 0) (array-dimension target 0)))
	     (not (= (array-dimension a 1) (array-dimension target 1))))
    (error "Target has bad dimensions"))	     
  (loop
   with aa of-type dbl-matrix =
      (or target
	  (make-dbl-matrix (array-dimension a 0) (array-dimension a 1)))
   with n of-type nice-unsigned-integer = (array-total-size a)
   for i below n
   for x of-type double-float = (row-major-aref a i)
   do
   (setf (row-major-aref aa i) x)
   finally (return aa)))




(defun copy-array-to-double-precision (a)
  "copy an array to a new double precision matrix"
  (declare (type array a))
  (loop
   with aa = (make-dbl-matrix (array-dimension a 0) (array-dimension a 1))
   with n = (array-total-size a)
   for i below n
   for x = (row-major-aref a i)
   do
   (when (not (realp x))
     (error "array A contains an element that is is not a real number"))
   (setf (row-major-aref aa i) (coerce x 'double-float))
   finally (return aa)))


(defun copy-matrix-to-double-precision (a)
  "copy a matrix to a double precision matrix - trivial wrapper for
copy-array-to-double-precision"
  (declare (type matrix a))
  (copy-array-to-double-precision a))


(defun rescale-array (a x)
  "destructively rescale any numerical array A by x"
  (declare (type array a)
	   (type real x))
  (loop
   with n of-type nice-unsigned-integer = (array-total-size a)
   for i of-type nice-unsigned-integer below n
   do (setf (row-major-aref a i) (* x  (row-major-aref a i)))
   finally (return a)))
  
(defun rescale-matrix (a x)
  "destructively rescale a matrix A by x - wrapper for rescale-array"
  (declare (type matrix a)
	   (type real x))
  (rescale-array a x))

(defun rescale-dbl-matrix (a x)
  "destructively rescale a double-precision matrix A by double x"
  (declare (type dbl-matrix a)
	   (type double-float x)
	   (optimize speed))
  (loop for i of-type nice-unsigned-integer below (array-dimension a 0)
	do
	(loop for j  of-type nice-unsigned-integer below (array-dimension a 1)
	      do (setf (aref a i j) (* x (aref a i j)))))
  a)
  
(defun fill-dbl-array (a val)
  (declare (type dbl-array a)
	   (type double-float val))
  (loop for i of-type nice-unsigned-integer below (array-total-size a)
     do (setf (row-major-aref a i) val)))

(defun add-arrays (a1 a2 &key (c1 1) (c2 1)
		      (output-type t) (target nil))
  ;;
  "Add arrays a1 and a2, placing result into target if
specified, else create a new array.
If keyword arguments :C1 or :C2 are supplied, these are the coefficients
that multiply A1 and A2 - output type is one of the standard Lisp
array types, unless target is specified, in which case
the result of the addition is coerced to fit into target"
  ;;
  (declare (type array a1 a2)
	   (type (or null array) target)
	   (type real c1 c2))
  (when (not (equal (array-dimensions a1) (array-dimensions a2)))
    (error "Arrays do not have the same shape"))
  (when (and target
	     (not (equal (array-dimensions a1)
			 (array-dimensions target))))
    (error "Target array has wrong dimensions"))
  (loop with aout = (or target
			(make-dbl-matrix (array-dimension a1 0)
					 (array-dimension a1 1)))
	with output-type = (if target (array-element-type target)
			     output-type)
	with n = (array-total-size a1)
	for i below n
	for x1 = (row-major-aref a1 i)
	for x2 = (row-major-aref a1 i)
	for xout = (coerce (+ (* c1 x1) (* c2 x2)) output-type)
	do (setf (row-major-aref aout i) xout)
	finally (return aout)))

(defun add-matrices (a1 a2  &key (c1 1) (c2 1)
			(output-type t) (target nil))
  "Add matrices a1 and a2, placing result into target if
specified, else create a new matrix.
If keyword arguments :C1 or :C2 are supplied, these are the coefficients
that multiply A1 and A2 - output type is one of the standard Lisp
array types, unless target is specified, in which case
the result of the addition is coerced to fit into target"
  (declare (type matrix a1 a2)
	   (type (or null matrix) target)
	   (type real c1 c2))
  (add-arrays a1 a2 :c1 c1 :c2 c2
	      :output-type output-type :target target))


(defun add-dbl-matrices  (a1 a2  &key (c1 1d0) (c2 1d0)
			     (target nil))
  (declare (type dbl-matrix a1 a2)
	   (type double-float c1 c2)
	   (type (or null dbl-matrix) target)
	   (optimize speed))
  (when (or (not (= (array-dimension a1 0) (array-dimension a2 0)))
	    (not (= (array-dimension a1 1) (array-dimension a2 1))))
    (error "Matrix dimensions of A1 and A2 do not match"))
  (when (and target
	     (or (not (= (array-dimension a1 0) (array-dimension target 0)))
		 (not (= (array-dimension a1 1) (array-dimension target 1)))))
    (error "Target has wrong dimensions"))  
  ;;
  (loop
   with aout of-type dbl-matrix
       = (or target 
	     (make-dbl-matrix (array-dimension a1 0)
			      (array-dimension a1 1)))
   for i of-type nice-unsigned-integer below (array-dimension a1 0)
   do
   (loop
    for j of-type nice-unsigned-integer below (array-dimension a1 1)
    do (setf (aref aout i j) (+ (* c1 (aref a1 i j))
				(* c2 (aref a2 i j)))))
   finally
   (return aout)))
   
   
(defun matrix-multiply (a b &key (target-matrix nil)
			  (output-type t))
  "multiply two matrices A B into a new matrix, placing result
into :TARGET-MATRIX if specified.  Array type of result matrix
is :OUTPUT-TYPE, if  :TARGET-MATRIX is unspecified"
  (declare (type matrix a b)
	   (type (or null matrix) a b))
  ;;
  (let ((ma (array-dimension a 0))
	(na (array-dimension a 1))
	(mb (array-dimension b 0))
	(nb (array-dimension b 1)))
    ;;
    (when (not (= na mb))
      (error "Bad dimensions - multiplying A[~d][~d] x B[~d][~d]" na ma nb mb))
    (when (and target-matrix
	       (or (not (= na (array-dimension a 0)))
		   (not (= mb (array-dimension a 1)))))
      (error "Bad dimensions for result -  multiplying A[~d][~d] x B[~d][~d]
into C[~d][~d]" na ma nb mb (array-dimension a 0) (array-dimension a 1)))
    ;;
    (loop
     with zero = (coerce 0 output-type)
     with result = (or target-matrix
		       (make-array (list ma nb)
				   :initial-element zero
				   :element-type output-type))
     for i of-type nice-unsigned-integer below ma
     do
     (loop
      for j of-type nice-unsigned-integer below nb
      do
      (setf (aref result i j) zero)
      (loop
       for k of-type nice-unsigned-integer below na
       ;; first sum into 'the-sum' using general arithmetic
       sum (* (aref a i k) (aref b k j)) into the-sum
       finally
       ;; and THEN coerce 'the-sum' to fit into result
       (setf (aref result i j)
		(coerce (+ (aref result i j) the-sum) output-type))))
     finally (return result))))

(defun matrix-multiply-dbl (a b &key (target-matrix nil))
  "multiply two double precision matrices A B into a new matrix
- if a key argument :TARGET-MATRIX is supplied, it is taken to
be a double-float array into which result is placed"
  (declare (type dbl-matrix a b)
	   (type (or null dbl-matrix) target-matrix)
	   (optimize speed))
  (let ((ma (array-dimension a 0))
	(na (array-dimension a 1))
	(mb (array-dimension b 0))
	(nb (array-dimension b 1)))
    (when (not (= na mb))
      (error "Bad dimensions - multiplying A[~d][~d] x B[~d][~d]" na ma nb mb))
    (when (and target-matrix
	       (or (not (= (array-dimension target-matrix 0) na))
		   (not (= (array-dimension target-matrix 1) mb))))
      (error "Target-Matrix array has wrong dimensions"))
			
    (loop
     with result of-type dbl-matrix
	 = (or target-matrix 
	       (make-dbl-matrix (array-dimension a 0) (array-dimension a 1)
				:initial-element 0d0))
     for i of-type nice-unsigned-integer below ma
     do
     (loop
      for j of-type nice-unsigned-integer below nb
      do
      (setf (aref result i j) 0d0)
      (loop
       for k of-type nice-unsigned-integer below na
       do (incf (aref result i j) (* (aref a i k) (aref b k j)))))
     finally (return result))))
 
(defun make-random-positive-definite-matrix (n)
  "Generate a random NxN positive definite matrix.  It is pos-def
beause of diagonal dominance."
  (let ((a (make-array (list n n) :element-type 'double-float)))
    (loop for i below n
	  do (loop for j from i below n
		   for dval = (if (= i j) n 0d0)
		   for val = (+ dval (random 1d0))
			 do 
			    (setf (aref a i j) val)
			    (setf (aref a j i) val)))
    a))

(defun gauss-jordan-elimination-destructive-dbl (a &optional b)
  "perform full gauss-jordan elimination for double-precision 2d arrays
a[n][n] and b[n][m], replacing a[n][n] with its inverse and replacing a set of
solution vectors in b[n][m] with their solutions - B can be NIL for
the case of no solution vectors"
  (declare (type dbl-matrix a)
	   (type (or null dbl-matrix) b)
	   (optimize speed)
	   )
  (let* ((n (array-dimension a 0))
	 (m (if b (array-dimension b 1) 0))
	 (nb (if b (array-dimension b 0) n))
	 (n-1 (1- n))
	 (indexc (make-array n :element-type 'nice-signed-integer))
	 (indexr (make-array n :element-type 'nice-signed-integer))
	 (ipiv (make-array n :element-type 'nice-signed-integer
			   :initial-element 0)))
    
    (declare (type nice-signed-integer n m nb n-1)
	     (type (simple-array nice-signed-integer (*))
		   indexc indexr ipiv))
    
    (when (not (= n (array-dimension a 1)))
      (error "A is not square - cannot invert"))
    (when (not (= n nb))
      (error "B[n][m] does not match A[n][n] in n - invalid solution vectors"))

    (locally
     (declare (optimize (speed 3) (safety 1) (debug 0)))
     ;;
     (loop
      with irow of-type nice-signed-integer = 0
      with icol of-type nice-signed-integer = 0
      with big of-type double-float = 0d0
      with pivinv of-type double-float = 0d0
      for i of-type nice-signed-integer below n
      do
      (setf big 0d0)
      (loop
       for j of-type nice-signed-integer below n
       do
       ;;
       (if (not (= (aref ipiv j) 1))
	   (loop
	    for k of-type nice-signed-integer below n
	    do
	    (if (= 0 (aref ipiv k))
		(if (>= (abs (aref a j k)) big)
		    (progn (setf big (abs (aref a j k)))
			   (setf irow j)
			   (setf icol k)))
	      (when (> (aref ipiv k) 1) (error "Singular matrix 1"))))))
      ;;
      (incf (aref ipiv icol) 1)
      ;;
      (when (not (= irow icol))
	(loop for l of-type nice-signed-integer below n
	      do (rotatef (aref a irow l) (aref a icol l)))
	(loop for l of-type nice-signed-integer below m
	      do (rotatef (aref b irow l) (aref b icol l))))
      ;;
      (setf (aref indexr i) irow)
      (setf (aref indexc i) icol)
      ;;
      (when (zerop (aref a icol icol)) (error "Singular matrix 2"))
      ;;
      (setf pivinv (/ 1d0  (aref a icol icol)))
      (setf (aref a icol icol) 1d0)
      ;;
      (loop for l of-type nice-signed-integer below n
	    do (setf (aref a icol l) (* pivinv (aref a icol l))))
      (loop for l of-type nice-signed-integer below m
	    do (setf (aref b icol l) (* pivinv (aref b icol l)))) 
      ;;
      (loop
       with dum of-type double-float = 0d0
       for ll of-type nice-signed-integer below n
       when (not (= ll icol))
       do
       (setf dum (aref a ll icol))
       (setf (aref a ll icol) 0d0)
       (loop for l of-type nice-signed-integer below n
	     do (decf (aref a ll l) (* (aref a icol l) dum)))
       (loop for l of-type nice-signed-integer below m
	     do (decf (aref b ll l) (* (aref b icol l) dum))))
      
      ) ;; end of big 'i' loop over columns
     ;;
     ;; unscramble solutions
     (loop for l of-type nice-signed-integer from n-1 downto 0
	   do
	   (when (not (= (aref indexr l) (aref indexc l)))
	     (loop for k of-type nice-signed-integer below n
		   do 
		  (rotatef (aref a k (aref indexr l))
			   (aref a k (aref indexc l))))))
     t
     )))
      

(defun transpose-matrix (a &key (target nil)
			   (output-type nil))
  "transpose a matrix so that A[i][j] --> A[j][i] - if keyword TARGET
is specified, then use this as the target - if keyword OUTPUT-TYPE
is specified, use this as the output type - default OUTPUT-TYPE NIL
means use same type as input matrix
 If TARGET is the same as A, then transposition is destructive"
  (declare (type matrix a)
	   (type (or null matrix) target))
  (loop
   with a-type = (array-element-type a)
   with output-type = (or output-type a-type)
   with m = (array-dimension a 0)
   with n = (array-dimension a 1)
   with aout = (or target (make-array (list n m) :element-type output-type))
   with a-is-aout = (eq a aout)
   ;;
   initially
   (when (and target ;; check if target given has right dimensions
	      (not (and (= (array-dimension target 0) n)
			(= (array-dimension target 1) m))))
     (error "Target matrix has the wrong dimensions"))
   ;;
   for i of-type nice-unsigned-integer below m
   do
   (cond
    (a-is-aout
     (loop  ;; simple case of (eq a aout) in which case we do only (> i j)
      for j of-type nice-unsigned-integer from (1+ i) below n
      do (rotatef (aref a i j) (aref a j i))))
    (t
     (loop ;; case of (not (eq a aout)) so look over all i,j
      for j of-type nice-unsigned-integer from 0 below n
      do (setf (aref aout j i) (coerce (aref a i j)  output-type))))) 
   finally
   (return aout)))
	    

(defun transpose-dbl-matrix (a &key (target nil))
  "transpose a double-precision matrix so that A[i][j] --> A[j][i] -
if keyword TARGET is specified, then use this as the target.
 If TARGET is the same as A, then transposition is destructive"
  (declare (type dbl-matrix a)
	   (type (or null dbl-matrix) target)
	   (optimize speed))
  (loop
   with m = (array-dimension a 0)
   with n = (array-dimension a 1)
   with aout of-type dbl-matrix =
      (or target (make-dbl-matrix n m))
   with a-is-aout = (eq a aout)
   ;;
   initially
   (when (and target ;; check if target give has right dimensions
	      (not (and (= (array-dimension target 0) n)
			(= (array-dimension target 1) m))))
     (error "Target matrix has the wrong dimensions"))
   ;;
   for i of-type nice-unsigned-integer below m
   do
   (cond
    (a-is-aout
     (loop  ;; simple case of (eq a aout) in which case we do only (> i j)
      for j of-type nice-unsigned-integer from (1+ i) below n
      do (rotatef (aref a i j) (aref a j i))))
    (t
     (loop ;; case of (not (eq a aout)) so look over all i,j
      for j of-type nice-unsigned-integer from 0 below n
      do (setf (aref aout j i) (aref a i j)))))
   finally
   (return aout)))
	     



	       
(defun gauss-jordan-elimination (a &optional b)
  "perform full gauss-jordan elimination for real array a[n][n] and
solution vectors b[n][m], returning the inverse of a[n][n] and the
solution of b[n][m] - B can be left unspecified for the case of no solution
vectors"
  (declare (type matrix a)
	   (type (or null matrix) b))
  (let ((aa (copy-matrix-to-double-precision a))
	(bb (if b (copy-matrix-to-double-precision b))))
    (gauss-jordan-elimination-destructive-dbl aa bb)
    (values aa bb)))


(defun invert-matrix (a)
  "trivial front end for gauss-jordan-elimination - return inverse
of general A[n][n] in a fresh matrix."
  (declare (type matrix a))
  (gauss-jordan-elimination a))

(defun invert-matrix-destructive-dbl (a)
  "invert a double precision matrix in place - trivial front end for
gauss-jordan-elimination-destructive-dbl"
  (declare (type dbl-matrix a))
  (gauss-jordan-elimination-destructive-dbl a))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEST ROUTINE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun matrix-invert-test (n)
  "test of matrix inversion that makes an NxN random matrix, inverts it,
multiplies by inverse, and prints maximum deviations from
Identity matrix"
  (let ((m (make-dbl-matrix n n))
	(minv nil)
	(ident nil))
    (loop
     for i below n
     do (loop for j below n
	      do
	      (setf (aref m i j) (* 1d-2 (random 100)))))
    (format t "Starting inversion~%")
    (setf minv (invert-matrix m))
    (format t "Done inversion~%")
    (setf ident (matrix-multiply-dbl minv m))
    ;;
    (loop
     with biggest-diag-dev = 0d0
     with biggest-off-diag-dev = 0d0
     for i below n
     do (loop for j below n
	      for x = (aref ident i j)
	      do
	      (cond ((= i j)
		     (setf biggest-diag-dev (max (abs (- 1d0 x))
						 biggest-diag-dev)))
		    (t
		     (setf biggest-off-diag-dev (max (abs x) biggest-diag-dev)))))
     finally (format t "Biggest-diag-deviation from 1.0: ~A
Biggest-off-diag-deviation from 0.0: ~A~%"
		     biggest-diag-dev  biggest-off-diag-dev))))
	  





;; Singular Value Decomposition


;; Singular Value Decomposition

(defmacro fref (arr &rest indices)
  `(aref ,arr ,@(mapcar #'(lambda (x) (list '1- x)) indices)))
(defmacro fset (a b) 
  `(setf (fref ,(second a) ,@(cddr a)) ,b))
(defmacro signp (a b)
  `(let ((%aa ,a)
	 (%bb ,b))
     (if (>= %bb 0d0)
	 (abs %aa)
	 (- (abs %aa)))))


(defmacro %psqrt (x)
  `(the (double-float 0d0)
     (sqrt (the (double-float 0d0) ,x))))

(defun svdcmp (array &key (destroy nil) u v w)
 "Destructively perform an SVD of MxN double float ARRAY, returning
   (values U W V) where A=U W V^T, U, and W is a diagonal matrix
expressed as a vector.  U is MxN, W is of length N, and V is NxN.
ARRAY is not modified unless DESTROY keyword is set, which saves some
allocation.  If U is given, then ARRAY is not modified, regardless of
DESTROY.

If M<N, then only the first M rows of U and the first M elements of W
are non-zero.  We don't trim out the zero rows to preserve the quality that
the input array is the product of the outputs."
 (declare (type dbl-matrix array)
	  (type (or null dbl-matrix) u v)
	  (type (or null dbl-vector) w)
	  (optimize speed))
 ;;
 ;; check dimensions of inputs
 (let ((m (array-dimension array 0))
       (n (array-dimension array 1)))
   (when (and v (or (not (= (array-dimension v 0)  n))
		    (not (= (array-dimension v 1)  n))))
     (error "Invalid input V dimensions"))
   (when (and u (or (not (= (array-dimension u 0) m))
		    (not (= (array-dimension u 1) n))))
     (error "Invalid input U dimensions"))
    (when (and w (or (not (= (array-dimension w 0) n))))
      (error "Invalid input W dimensions")))
 ;;

 ;; if u is given, copy array into it. Array will then be left alone.
 (when u
   (loop for i of-type nice-unsigned-integer below (array-total-size array)
      do (setf (row-major-aref u i) (row-major-aref array i))))

 (prog*
     ((a (or u ;; u becomes a if it was given - we have copied array into it above
	     (if destroy  ;; if destroying, then don't make fresh copy of array
		 array
		 (copy-dbl-matrix array))))
      (m (array-dimension a 0))
      (n (array-dimension a 1))
      (nm-min (min m n))
      (w (or w (make-array n :element-type 'double-float :initial-element 0d0)))
      (u a)  ;; just notation
      (v (or v (make-dbl-matrix n n)))
      (rv1 (make-array n :element-type 'double-float :initial-element 0d0))
      (g 0d0) (scale 0d0) (anorm 0d0) (s 0d0) (h 0d0) (f 0d0) (c 0d0) 
      (x 0d0) (y 0d0) (z 0d0) (l 0) (ll 0) (nm 0) (i 0))
    (declare (type fixnum l ll m n nm i nm-min))
    (declare (type double-float c anorm f g h s scale x y z))
    (declare (type dbl-vector w))
    (declare (type dbl-vector rv1))
    (declare (type dbl-matrix a u v))


    (do ((i 1 (+ i 1)))
	((> i n) t)
      (declare (type fixnum i))
      
      (setf l (+ i 1))
      (fset (fref rv1 i) (* scale g))
      (setf g 0d0)
      (setf s 0d0)
      (setf scale 0d0)
      (when (<= i m)
	(do ((k i (+ k 1)))
	    ((> k m) t)
          (declare (type fixnum k))
	  (setf scale (+ scale (abs (fref a k i)))))
	(when (not (= scale 0d0)) 
	  (do ((k i (+ k 1)))
	      ((> k m) t)
            (declare (type fixnum k))
	    (fset (fref a k i) (/ (fref a k i) scale))
	    (setf s (+ s (* (fref a k i) (fref a k i)))))
	  
	  (setf f (fref a i i))
	  (setf g (- (signp  (%psqrt s) f)))
	  (setf h (+ (* f g) (- s)))
	  (fset (fref a i i) (+ f (- g)))
	  (when (not (= i n)) 
	    (do ((j l (+ j 1)))
		((> j n) t)
              (declare (type fixnum j))
            (setf s 0d0)
	      (do ((k i (+ k 1)))
		  ((> k m) t)
                (declare (type fixnum k))
		(setf s (+ s (* (fref a k i) (fref a k j)))))
	      
	      (setf f (/ s h))
	      (do ((k i (+ k 1)))
		  ((> k m) t)
		(declare (type fixnum k))
		(fset (fref a k j) (+ (fref a k j) (* f (fref a k i)))))))
	  
	  (do ((k i (+ k 1)))
	      ((> k m) t)
            (declare (type fixnum k))
	    (fset (fref a k i) (* scale (fref a k i))))))
      ;;-------------------------------------------
      (fset (fref w i) (* scale g))
      (setf g 0d0)
      (setf s 0d0)
      (setf scale 0d0)
      (when (and (<= i m) (not (= i n))) 
	(do ((k l (+ k 1)))
	    ((> k n) t)
          (declare (type fixnum k))
	  (setf scale (+ scale (abs (fref a i k)))))
	
	(when (not (= scale 0d0)) 
	  (do ((k l (+ k 1)))
	      ((> k n) t)
            (declare (type fixnum k))
	    (fset (fref a i k) (/ (fref a i k) scale))
	    (setf s (+ s (* (fref a i k) (fref a i k)))))

	  (setf f (fref a i l))
	  (setf g (- (signp  (%psqrt s) f)))
	  (setf h (+ (* f g) (- s)))
	  (fset (fref a i l) (+ f (- g)))
	  (do ((k l (+ k 1)))
	      ((> k n) t)
            (declare (type fixnum k))
	    (fset (fref rv1 k) (/ (fref a i k) h)))
	  
	  (when (not (= i m)) 
	    (do ((j l (+ j 1)))
		((> j m) t)
              (declare (type fixnum j))
	      (setf s 0d0)
	      (do ((k l (+ k 1)))
		  ((> k n) t)
		(declare (type fixnum k))
		(setf s (+ s (* (fref a j k) (fref a i k)))))
	      
	      (do ((k l (+ k 1)))
		  ((> k n) t)
                (declare (type fixnum k))
		(fset (fref a j k) (+ (fref a j k) (* s (fref rv1 k)))))))
	  
	  (do ((k l (+ k 1)))
	      ((> k n) t)
            (declare (type fixnum k))
	    (fset (fref a i k) (* scale (fref a i k))))))
      
      
      (setf anorm (max anorm (+ (abs (fref w i)) (abs (fref rv1 i))))))
    ;;=============================================  
    
    (do ((i n (+ i (- 1))))
        ((< i 1) t)
      (declare (type fixnum i))
      (when (< i n) 
	(when (not (= g 0d0)) 
	  (do ((j l (+ j 1)))
	      ((> j n) t)
            (declare (type fixnum j))
	    (fset (fref v j i) (/ (/ (fref a i j) (fref a i l)) g)))
	  
	  (do ((j l (+ j 1)))
	      ((> j n) t)
            (declare (type fixnum j))
	    (setf s 0d0)
	    (do ((k l (+ k 1)))
		((> k n) t)
              (declare (type fixnum k))
	      (setf s (+ s (* (fref a i k) (fref v k j)))))
	    
	    (do ((k l (+ k 1)))
		((> k n) t)
              (declare (type fixnum k))
	      (fset (fref v k j) (+ (fref v k j) (* s (fref v k i)))))))
	
	(do ((j l (+ j 1)))
	    ((> j n) t)
          (declare (type fixnum j))
	  (fset (fref v i j) 0d0)
	  (fset (fref v j i) 0d0)))
      
      (fset (fref v i i) 1d0)
      (setf g (fref rv1 i))
      (setf l i))
    ;;**********************************
    (do ((i nm-min (+ i (- 1)))) ;; original code seems buggy, using n instead of nm-min
	((< i 1) t)
      (declare (type fixnum i))
      (setf l (+ i 1))
      (setf g (fref w i))
      (when (< i n) 
	(do ((j l (+ j 1)))
	    ((> j n) t)
          (declare (type fixnum j))
	  (fset (fref a i j) 0d0)))
      
      (cond ((not (= g 0d0))
	     (setf g (/ 1d0 g))
	     (when (not (= i n)) 
	       (do ((j l (+ j 1)))
		   ((> j n) t)
		 (declare (type fixnum j))
		 (setf s 0d0)
		 (do ((k l (+ k 1)))
		     ((> k m) t)
		   (declare (type fixnum k))
		   (setf s (+ s (* (fref a k i) (fref a k j)))))
		 
		 (setf f (* (/ s (fref a i i)) g))
		 (do ((k i (+ k 1)))
		     ((> k m) t)
		   (declare (type fixnum k))
		   (fset (fref a k j) (+ (fref a k j) (* f (fref a k i)))))))
	     
	     (do ((j i (+ j 1)))
		 ((> j m) t)
	       (declare (type fixnum j))
	       (fset (fref a j i) (* (fref a j i) g))))
	    
	    (t
	     (do ((j i (+ j 1)))
		 ((> j m) t)
	       (declare (type fixnum j))
	       (fset (fref a j i) 0d0))))
      
      (fset (fref a i i) (+ (fref a i i) 1d0)))
    ;;----------------------------------------------------------
    
    (do ((k n (+ k (- 1))))
	((< k 1) t)
      (declare (type fixnum k))
      (do ((its 1 (+ its 1)))
	  ((> its 30) t)
        (declare (type fixnum its))
        (do ((l k (1- l)))
            ((< l 1) t)
	  (declare (type fixnum l))
          (setf nm (1- l))
          (setq ll l)
          (when (= anorm (+ (abs (fref rv1 l)) anorm)) (go label2))
	  (when (= anorm (+ (abs (fref w nm)) anorm)) (go label1)))
	
	
       label1
        (setf c 0d0)
        (setf s 1d0)
        (do ((i ll (+ i 1)))
            ((> i k) t)
	  (declare (type fixnum i))
          (setf f (* s (fref rv1 i)))
          (fset (fref rv1 i) (* c (fref rv1 i)))
          (when (= anorm (+ (abs f) anorm)) (go label2))
          (setf g (fref w i))
          (setf h  (%psqrt (+ (* f f) (* g g))))
          (fset (fref w i) h)
          (setf h (/ 1d0 h))
          (setf c (* g h))
          (setf s (- (* f h)))
	  
          (do ((j 1 (+ j 1)))
              ((> j m) t)
	    (declare (type fixnum j))
            (setf y (fref a j nm))
            (setf z (fref a j i))
            (fset (fref a j nm) (+ (* y c) (* z s)))
            (fset (fref a j i) (+ (- (* y s)) (* z c)))))
	
       label2
	
        (setf z (fref w k))
        (when (= ll k) 
          (when (< z 0d0) 
            (fset (fref w k) (- z))
            (do ((j 1 (+ j 1)))
                ((> j n) t)
	      (declare (type fixnum j))
              (fset (fref v j k) (- (fref v j k)))))
          (go label3))
	
        (if (= its 30) (error "no convergence in 30 iterations of svdcmp"))
        (setf x (fref w ll))
        (setf nm (+ k (- 1)))
        (setf y (fref w nm))
        (setf g (fref rv1 nm))
        (setf h (fref rv1 k))
        (setf f
              (/ (+ (* (+ y (- z)) (+ y z)) (* (+ g (- h)) (+ g h)))
                 (* (* 2d0 h) y)))
        (setf g  (%psqrt (+ (* f f) 1.0d0)))
	
        (setf f
              (/
               (+ (* (+ x (- z)) (+ x z))
                  (* h (+ (/ y (+ f (signp g f))) (- h))))
               x))
        (setf c 1d0)
        (setf s 1d0)
        (do ((j ll (+ j 1)))
            ((> j nm) t)
	  (declare (type fixnum j))
          (setf i (+ j 1))
          (setf g (fref rv1 i))
          (setf y (fref w i))
          (setf h (* s g))
          (setf g (* c g))
          (setf z  (%psqrt (+ (* f f) (* h h))))
          (fset (fref rv1 j) z)
          (setf c (/ f z))
          (setf s (/ h z))
          (setf f (+ (* x c) (* g s)))
          (setf g (+ (- (* x s)) (* g c)))
          (setf h (* y s))
          (setf y (* y c))
	  
          (do ((jj 1 (+ jj 1)))
              ((> jj n) t)
	    (declare (type fixnum jj))
            (setf x (fref v jj j))
            (setf z (fref v jj i))
            (fset (fref v jj j) (+ (* x c) (* z s)))
            (fset (fref v jj i) (+ (- (* x s)) (* z c))))
	  
          (setf z  (%psqrt (+ (* f f) (* h h))))
          (fset (fref w j) z)
	  
          (when (not (= z 0d0)) 
            (setf z (/ 1d0 z))
            (setf c (* f z))
            (setf s (* h z)))
	  
          (setf f (+ (* c g) (* s y)))
          (setf x (+ (- (* s g)) (* c y)))
          (do ((jj 1 (+ jj 1)))
              ((> jj m) t)
	    (declare (type fixnum jj))
            (setf y (fref a jj j))
            (setf z (fref a jj i))
            (fset (fref a jj j) (+ (* y c) (* z s)))
            (fset (fref a jj i) (+ (- (* y s)) (* z c)))))
	(fset (fref rv1 ll) 0d0)
	(fset (fref rv1 k) f)
	(fset (fref w k) x))
     label3)
      
      ;; copy u from a
      (dotimes (i m)
	(declare (type nice-unsigned-integer i))
	(dotimes (j n)
	  (declare (type nice-unsigned-integer j))
	  (setf (aref u i j) (aref a i j))))

    (return (values u w v))))


(defun %svdtest (u w v)
  (matrix-multiply-dbl
   (matrix-multiply-dbl
    u (diag-matrix w))
   (transpose-matrix v)))

(defun svbksb (u w v b)
  "Given an SVD decomposition of some matrix A=U W V^T, and a vector b,
solve Ax=b.  No inputs are changed.  It is expected that the
undesired small elements of the vector W will have been zeroed out.  See
NR SVD discussion."
 (declare (type dbl-matrix u)) 
 (declare (type dbl-vector w)) 
 (declare (type dbl-matrix v)) 
 (declare (type dbl-vector b)) 
 (declare (optimize speed))

 (prog* (
  (n (array-dimension w 0))
  (m (array-dimension b 0))
  (x (make-array n :element-type 'double-float :initial-element 0d0))
  (tmp (make-array n :element-type 'double-float :initial-element 0d0))
  (s 0d0))

  (declare (type dbl-vector x)) 
  (declare (type dbl-vector tmp)) 
  (declare (type fixnum m n))
  (declare (type double-float s))


  (do ((j 0 (+ j 1)))
      ((> j (1- n)) t)
      (declare (type fixnum j))
    (setf s 0d0)
    (when 
     (not (= (aref w j) 0d0))
     (do ((i 0 (+ i 1)))
         ((> i (1- m)) t)
         (declare (type fixnum i))
       (setf s (+ s (* (aref u i j) (aref b i)))))

     (setf s (/ s (aref w j))))

    (setf (aref tmp j) s)) 

  (do ((j 0 (+ j 1)))
      ((> j (1- n)) t)
      (declare (type fixnum j))
    (setf s 0d0)
    (do ((jj 0 (+ jj 1)))
        ((> jj (1- n)) t)
        (declare (type fixnum jj))
      (setf s (+ s (* (aref v j jj) (aref tmp jj)))))
    (setf (aref x j) s)) 
   
  (return x)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun jacobi (a)
"For a real symmetric matrix A, return
  (VALUES D U)  where A = U D UT   where   
U's columns contain the normalized eigenvectors, 
UT is the transpose of U, and D is a diagonal array 
with the eigenvalues. 
A is overwritten."
 (declare (type dbl-matrix a))

 (when (not (symmetric-matrix-p a))
   (error "A is not a symmetric matrix in JACOBI"))

 (prog* ((nrot 0)
	 (n (array-dimension a 0))
	 (d (make-array n :element-type 'double-float :initial-element 0d0))
	 (v (make-dbl-matrix n n))
	 (b (make-array n :element-type 'double-float :initial-element 0d0))
	 (z (make-array n :element-type 'double-float :initial-element 0d0))
	 (sm 0d0) (tresh 0d0) (g 0d0) (h 0d0) (t0 0d0) 
	 (theta 0d0) (c 0d0) (s 0d0) (tau 0d0))
    
    (declare (type dbl-vector d)) 
    (declare (type dbl-matrix v)) 
    (declare (type dbl-vector b)) 
    (declare (type dbl-vector z))
    (declare (type nice-unsigned-integer n nrot))
    (declare (type double-float sm tresh g h t0 theta c s tau))
    (declare (optimize speed))
	     
    
    (do ((ip 1 (+ ip 1)))
	((> ip n) t)
      (declare (type fixnum ip))
      (do ((iq 1 (+ iq 1)))
	  ((> iq n) t)
	(declare (type fixnum iq))
	(fset (fref  v ip iq) 0d0))
      (fset (fref v ip ip) 1d0)) 
    (do ((ip 1 (+ ip 1)))
	((> ip n) t)
      (declare (type fixnum ip))
      (fset (fref b ip) (fref a ip ip))
      (fset (fref d ip) (fref b ip))
      (fset (fref z ip) 0d0)) 
    (setf nrot 0) 
      (do ((i 1 (+ i 1)))
	  ((> i 50) t)
	(declare (type fixnum i))
	(setf sm 0d0)
	(do ((ip 1 (+ ip 1)))
	    ((> ip (+ n (- 1))) t)
	  (declare (type fixnum ip))
	  (do ((iq (+ ip 1) (+ iq 1)))
	      ((> iq n) t)
	    (declare (type fixnum iq))
	    (setf sm (+ sm (abs (fref a ip iq))))))
	
	(if (= sm 0d0) (go end))
	(if (< i 4) 
	    (setf tresh (/ (* 0.2d0 sm) (expt (* 1d0 n) 2)))
	    (setf tresh 0d0))
	(do ((ip 1 (+ ip 1)))
	    ((> ip (+ n (- 1))) t)
	  (declare (type fixnum ip))
	  (do ((iq (+ ip 1) (+ iq 1)))
	      ((> iq n) t)
	    (declare (type fixnum iq))
	    (setf g (* 100d0 (abs (fref a ip iq))))
	    (cond 
	      ((and (> i 4)
		    (= (+ (abs (fref d ip)) g) (abs (fref d ip)))
		    (= (+ (abs (fref d iq)) g) (abs (fref d iq))))
	       (fset (fref a ip iq) 0d0))
	      ((> (abs (fref a ip iq)) tresh)            
	       (setf h (+ (fref d iq) (- (fref d ip))))
	       
	       (cond 
		 ((= (+ (abs h) g) (abs h))
		  (setf t0 (/ (fref a ip iq) h)))
		 (t
		  (setf theta (/ (* 0.5d0 h) (fref a ip iq)))
		  (setf t0 (/ 1d0 (+ (abs theta) (sqrt (1+ (expt theta 2))))))
		  (if (< theta 0d0) (setf t0 (- t0)))))
	       
	       (setf c (/ 1d0 (sqrt (+ 1d0 (expt t0 2))))) (setf s (* t0 c))
	       (setf tau (/ s (1+ c))) (setf h (* t0 (fref a ip iq)))
	       (fset (fref z ip) (+ (fref z ip) (- h)))
	       (fset (fref z iq) (+ (fref z iq) h))
	       (fset (fref d ip) (+ (fref d ip) (- h)))
	       (fset (fref d iq) (+ (fref d iq) h)) (fset (fref a ip iq) 0d0)
	       (do ((j 1 (+ j 1)))
		   ((> j (+ ip (- 1))) t)
		 (declare (type fixnum j))
		 (setf g (fref a j ip))
		 (setf h (fref a j iq))
		 (fset (fref a j ip) (+ g (* (- s) (+ h (* g tau)))))
		 (fset (fref a j iq) (+ h (* s (+ g (* (- h) tau))))))
	       (do ((j (+ ip 1) (+ j 1)))
		   ((> j (+ iq (- 1))) t)
		 (declare (type fixnum j))
		 (setf g (fref a ip j))
		 (setf h (fref a j iq))
		 (fset (fref a ip j) (+ g (* (- s) (+ h (* g tau)))))
		 (fset (fref a j iq) (+ h (* s (+ g (* (- h) tau))))))
	       (do ((j (+ iq 1) (+ j 1)))
		   ((> j n) t)
		 (declare (type fixnum j))
		 (setf g (fref a ip j))
		 (setf h (fref a iq j))
		 (fset (fref a ip j) (+ g (* (- s) (+ h (* g tau)))))
		 (fset (fref a iq j) (+ h (* s (+ g (* (- h) tau))))))
	       (do ((j 1 (+ j 1)))
		   ((> j n) t)
		 (declare (type fixnum j))
		 (setf g (fref v j ip))
		 (setf h (fref v j iq))
		 (fset (fref v j ip) (+ g (* (- s) (+ h (* g tau)))))
		 (fset (fref v j iq) (+ h (* s (+ g (* (- h) tau))))))
	       (setf nrot (+ nrot 1))))))
	(do ((ip 1 (+ ip 1)))
	    ((> ip n) t)
	  (declare (type fixnum ip))
	  (fset (fref b ip) (+ (fref b ip) (fref z ip)))
	  (fset (fref d ip) (fref b ip))
	  (fset (fref z ip) 0d0))) 
      (error "jacobi should not reach this point") 
      end
      (return (values d v nrot))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cholesky (a &key (l nil))
  "For a positive definite matrix A, compute the Cholesky decomposition
L such that A=LL^T.    Keyword argument :L is an optional output matrix,
which is otherwise allocated."
  (declare (type dbl-matrix a)
	   (type (or dbl-matrix null) l)
	   (optimize speed))

  (when (not (symmetric-matrix-p a))
   (error "A is not a symmetric matrix in CHOLESKY"))

  (when (and l (not (and (= (array-dimension a 0)
			    (array-dimension l 0))
			 (= (array-dimension a 1)
			    (array-dimension l 1)))))
    (error "Output matrix L was given, but is of wrong dimensions: A is ~Ax~A but L is ~Dx~D"
	   (array-dimension a 1)
	   (array-dimension a 0)
	   (array-dimension a 1)
	   (array-dimension a 0)))
  
  (let* ((n (array-dimension A 0))
	 (lout
	   (or l
	       (make-array `(,n ,n) :element-type 'double-float 
				    :initial-element 0d0))))
    (declare (type (unsigned-byte 28) n)
	     (type dbl-matrix lout))

    (flet ((%pos-sqrt (x)
	   (if (minusp x)
	       (error "Matrix in CHOLESKY encoutered SQRT(x<0).  The matrix is probably not positive definite")
	       (sqrt (the (double-float 0d0)  x)))))
      (loop
	for k below n
	do
           ;; diagonal elements lout_kk.
           (setf (aref lout k k)
		 (%pos-sqrt
		  (- (aref A k k)
		     (do* ((j 0 (1+ j))
			   (sum (expt (aref lout k j) 2) 
				(incf sum (expt (aref lout k j) 2))))
			  ((> j (- k 1)) sum)
		       (declare (type double-float sum)
				(type fixnum j))))))
	   
           ;; elements below a diagonal element, lout_ik, i=k+1..n.
	   (loop
	     for i from (1+ k)
	     until (> i (- n 1))
	     do
		(setf (aref lout i k)
		      (/ (- (aref A i k)
			    (do* ((j 0 (1+ j))
				  (sum (* (aref lout i j) (aref lout k j))
				       (incf sum (* (aref lout i j)
						    (aref lout k j)))))
				 ((> j (- k 1)) sum)
			      (declare (type double-float sum )
				       (type fixnum j)))) 
			 (aref lout k k)))))
      
      lout)))
