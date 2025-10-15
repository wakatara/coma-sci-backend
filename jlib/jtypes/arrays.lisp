

(in-package jtypes)

(deftype agglomeration ()
  '(or array list))

(deftype fixnum-vector ()
  '(simple-array fixnum (*)))
(deftype fixnumvec () 'fixnum-vector)

(deftype single-float-vector ()
  '(simple-array single-float (*)))
(deftype floatvec () 'double-float-vector)

(deftype double-float-vector ()
  '(simple-array double-float (*)))
(deftype dblvec () 'double-float-vector)

(deftype fixnum-matrix ()
  '(simple-array fixnum (* *)))
(deftype fixnummat () 'fixnum-matrix)

(deftype single-float-matrix ()
  '(simple-array single-float (* *)))
(deftype floatmat () 'single-float-matrix)

(deftype double-float-matrix ()
  '(simple-array double-float (* *)))
(deftype dblmat () 'double-float-matrix)



(defun make-fixnum-vector (n &key
			   (generator-function nil)
			   (initial-contents nil)
			   (initial-element nil))
  "Generate a (SIMPLE-ARRAY-FIXNUM n (*)).  If INITIAL-ELEMENT and/or
INITIAL-CONTENTS are set, they work like MAKE-ARRAY.  If GENERATOR-FUNCTION
is set, then it is a function of the index used to fill the array.  All fill
values are coerced to the correct type."
  (declare (type fixnum n))
  (let ((v (make-array n :element-type 'fixnum)))
    (when initial-element
      (fill v (round initial-element)))
    (when initial-contents
      (when (not (and (listp initial-contents)
		      (= (length initial-contents) n)))
	(error "INITIAL-CONTENTS is not a list of length ~A" n)))
      (loop 
	 for x in initial-contents 
	 for i from 0
	 do (setf (aref v i) (round x)))
      (when generator-function 
	(loop 
	   for i of-type fixnum below n
	   for x = (round (funcall generator-function i))
	   do (setf (aref v i) x)))
      ;;
      v))


(defun make-single-float-vector (n &key
				 (generator-function nil)
				 (initial-contents nil)
				 (initial-element nil))
  "Generate a (SIMPLE-ARRAY-SINGLE-FLOAT n (*)).  If INITIAL-ELEMENT and/or
INITIAL-CONTENTS are set, they work like MAKE-ARRAY.  If GENERATOR-FUNCTION
is set, then it is a function of the index used to fill the array.  All fill
values are coerced to the correct type."
  (declare (type fixnum n))
  (let ((v (make-array n :element-type 'single-float)))
    (when initial-element
      (fill v (float initial-element 1e0)))
    (when initial-contents
      (when (not (and (listp initial-contents)
		      (= (length initial-contents) n)))
	(error "INITIAL-CONTENTS is not a list of length ~A" n)))
      (loop 
	 for x in initial-contents 
	 for i from 0
	 do (setf (aref v i) (float x 1e0)))
      (when generator-function 
	(loop 
	   for i below n
	   for x = (float (funcall generator-function i) 1e0)
	   do (setf (aref v i) x)))
      ;;
      v))



(defun make-double-float-vector (n &key
				 (generator-function nil)
				 (initial-contents nil)
				 (initial-element nil))
  "Generate a (SIMPLE-ARRAY-DOUBLE-FLOAT n (*)).  If INITIAL-ELEMENT and/or
INITIAL-CONTENTS are set, they work like MAKE-ARRAY.  If GENERATOR-FUNCTION
is set, then it is a function of the index used to fill the array.  All fill
values are coerced to the correct type."
  (declare (type fixnum n))
  (let ((v (make-array n :element-type 'double-float)))
    (when initial-element
      (fill v (float initial-element 1d0)))
    (when initial-contents
      (when (not (and (listp initial-contents)
		      (= (length initial-contents) n)))
	(error "INITIAL-CONTENTS is not a list of length ~A" n)))
      (loop 
	 for x in initial-contents 
	 for i from 0
	 do (setf (aref v i) (float x 1d0)))
      (when generator-function 
	(loop 
	   for i below n
	   for x = (float (funcall generator-function i) 1d0)
	   do (setf (aref v i) x)))
      ;;
      v))




(defun make-fixnum-matrix (m n &key
			   (generator-function nil)
			   (initial-element nil))
  "Generate a (SIMPLE-ARRAY-FIXNUM (m n) (* *)).  If
INITIAL-ELEMENT is set, it works like MAKE-ARRAY.  If
GENERATOR-FUNCTION is set, then it is a function of the indices used
to fill the array.  All fill values are coerced to the correct type."
  (declare (type fixnum m n))
  (let ((arr (if initial-element
	       (make-array (list m n) 
			   :element-type 'fixnum
			   :initial-element (round initial-element))
	       (make-array (list m n)
			   :element-type 'fixnum))))
    (when generator-function 
      (loop 
	 for i of-type fixnum below m
	 do
	   (loop 
	      for j of-type fixnum below n
	      for x = (round (funcall generator-function i j))
	      do (setf (aref arr i j) x))))
    ;;
    arr))
	   
(defun make-single-float-matrix (m n &key
				 (generator-function nil)
				 (initial-element nil))
  "Generate a (SIMPLE-ARRAY-SINGLE-FLOAT (m n) (* *)).  If
INITIAL-ELEMENT is set, it works like MAKE-ARRAY.  If
GENERATOR-FUNCTION is set, then it is a function of the indices used
to fill the array.  All fill values are coerced to the correct type."
  (declare (type fixnum m n))
  (let ((arr (if initial-element
	       (make-array (list m n) 
			   :element-type 'single-float
			   :initial-element (float initial-element 1e0))
	       (make-array (list m n)
			   :element-type 'single-float))))
    (when generator-function 
      (loop 
	 for i of-type fixnum below m
	 do
	   (loop 
	      for j of-type fixnum below n
	      for x = (float (funcall generator-function i j) 1e0)
	      do (setf (aref arr i j) x))))
    ;;
    arr))
	   


(defun make-double-float-matrix (m n &key
				 (generator-function nil)
				 (initial-element nil))
  "Generate a (SIMPLE-ARRAY-DOUBLE-FLOAT (m n) (* *)).  If
INITIAL-ELEMENT is set, it works like MAKE-ARRAY.  If
GENERATOR-FUNCTION is set, then it is a function of the indices used
to fill the array.  All fill values are coerced to the correct type."
  (declare (type fixnum m n))
  (let ((arr (if initial-element
	       (make-array (list m n) 
			   :element-type 'double-float
			   :initial-element (float initial-element 1d0))
	       (make-array (list m n)
			   :element-type 'double-float))))
    (when generator-function 
      (loop 
	 for i of-type fixnum below m
	 do
	   (loop 
	      for j of-type fixnum below n
	      for x = (float (funcall generator-function i j) 1d0)
	      do (setf (aref arr i j) x))))
    ;;
    arr))
	   




(defun coerce-agglomeration-to-double-float-vector (agglom &key (force-copy t))
  "Coerce an agglomeration AGGLOM, a list or an array, into a double
float vector, unrolling the dimensions for a multidimensional array. If
FORCE-COPY is T, then force a copy even if AGGLOM is of the correct type."
  (declare (type agglomeration agglom))
  (cond 
    ;; already of correct type - maybe copy it
    ((typep agglom 'double-float-vector)
     (if force-copy
	 (copy-seq agglom)
	 agglom))
    ;; convert a list
    ((listp agglom)
     (make-double-float-vector (length agglom) :initial-contents agglom))
    ;; convert a general array
    ((arrayp agglom)
     (loop 
	with n = (array-total-size agglom)
	with v = (make-double-float-vector n)
	for i below n
	do (setf (aref v i) (float (row-major-aref agglom i) 1d0))
	finally (return v)))))
	      

(defun coerce-agglomeration-to-single-float-vector (agglom &key (force-copy t))
  "Coerce an agglomeration AGGLOM, a list or an array, into a single
float vector, unrolling the dimensions for a multidimensional array. If
FORCE-COPY is T, then force a copy even if AGGLOM is of the correct type."
  (declare (type agglomeration agglom))
  (cond 
    ;; already of correct type - maybe copy it
    ((typep agglom 'single-float-vector)
     (if force-copy
	 (copy-seq agglom)
	 agglom))
    ;; convert a list
    ((listp agglom)
     (make-single-float-vector (length agglom) :initial-contents agglom))
    ;; convert a general array
    ((arrayp agglom)
     (loop 
	with n = (array-total-size agglom)
	with v = (make-single-float-vector n)
	for i below n
	do (setf (aref v i) (float (row-major-aref agglom i) 1e0))
	finally (return v)))))
	      
  