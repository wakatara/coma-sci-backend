
#|


write and read specialized arrays to keep their type information

Use notation that  
  (make-array  '(2 2)  :element-type 'double-float 
       :initial-contents '((1d0 2d0) (3d0 4d0)))
writes to 
  #Z(double-float (2 2) :row-major-contents 1.0d0 2.0d0 3.0d0 4.0d0)

One problem seems to be that PRINT-OBJECT can't be redefined
for standard classes like arrays.  Hence our writer is never
called.

We bypass that by requiring *print-pretty* to be T, and 
using SET-PPRINT-DISPATCH.

The standard usage is 

  (WITH-READABLE-ARRAYS ()
     (STUFF-THAT-READS-AND-WRITES-ARRAYS ...))
  
which temporarily modifies *READTABLE* and *PRINT-PPRINT-DISPATCH*
and sets *PRINT-PRETTY* to T.

|#

(defpackage readable-arrays
  (:use #:common-lisp)
  (:export
   #:install-readable-arrays
   #:with-readable-arrays))

(in-package readable-arrays)

(defvar *array-dispatch-char* #\Z)

(defun install-readable-arrays (&key (readtable *readtable*)
				(array-dispatch-char *array-dispatch-char*)
				(pprint-dispatch-table *print-pprint-dispatch*))
  "Install readable array capability into READTABLE"
    (set-dispatch-macro-character #\#  array-dispatch-char
				  'array-read-function  readtable)
    (set-pprint-dispatch 
     '(and simple-array (not string) (not (simple-array T (*))))
     'print-array 0  pprint-dispatch-table ))


(defmacro with-readable-arrays ((&key (readtable *readtable*)
				      (array-dispatch-char *array-dispatch-char*)
				      (pprint-dispatch-table *print-pprint-dispatch*))
				&body body)
"Evaluable BODY with readable and writable specialized arrays available"
  `(let ((*print-pprint-dispatch* (copy-pprint-dispatch ,pprint-dispatch-table))
	 (*readtable* (copy-readtable ,readtable))
	 (*print-pretty* t))
     (install-readable-arrays :array-dispatch-char ,array-dispatch-char
			      :readtable *readtable*
			      :pprint-dispatch-table *print-pprint-dispatch*)
     ,@body))
     

(defmacro with-readable-arrays (&body body)
  `(let ((*print-pprint-dispatch* (copy-pprint-dispatch))
	 (*readtable* (copy-readtable)))
     (install-readable-arrays :array-dispatch-char *array-dispatch-char*)
     ,@body))
      

;; the array read function is designed to be memory-efficient, creating the array
;; then reading and filling element by element.  No intermediate storage 
;; is used.
(defun array-read-function (stream sub-char dispatch-integer)
  (when dispatch-integer 
    (error "Readable array #~A(...): Did not expect a dispatch-integer" 
	   sub-char))
  (when (not (eql sub-char *array-dispatch-char*))
    (error "Char ~A is not ~A" sub-char *array-dispatch-char*))
  ;; grab the leading (
  (when (not (eql (read-char stream nil nil) #\())
    (error "Cound not find leading paren when reading specialized array"))
  ;;
  (let* ((type (read stream))
	 (dimensions (read stream))
	 (rmkeyword (read stream)))
    ;;
    (when (not (eq rmkeyword :row-major-contents))
      (error ":ROW-MAJOR-CONTENTS expected when reading array"))
    ;;
    ;; read elements one by one
    (let ((a (make-array dimensions :element-type type)))
      (loop 
	 for i of-type fixnum below (array-total-size a)
	 for x = (read stream)
	 do (setf (row-major-aref a i) x))
      
      ;; consume final closing paren - no comments may appear before it
      (loop for c = (read-char stream nil nil)
	 until (eql c #\))
	   when (not (find c #(#\space #\tab #\cr #\lf)))
	   do (error "Unexpected character <~S> at end of specialized array" c))

      a))) 




;; this seems horrid, but it is the best we can do
;; we really don't understand pprint in all its majestic ghastliness
(defun print-array  (stream array)
  (declare (type stream stream)
	   (type simple-array array)
	   (optimize speed))
  (let ((prefix (format nil "#~A(~S ~S ~S ~%" 
			*array-dispatch-char*
			(array-element-type array) (array-dimensions array)
			:ROW-MAJOR-CONTENTS)))
    (pprint-newline :linear stream)
    (pprint-logical-block (stream nil :prefix prefix :suffix (format nil ")"))
      (pprint-indent :block 2 stream)
      ;(pprint-newline :mandatory stream)
      (loop 
	 for i of-type fixnum below (array-total-size array)
	 do 
	   (pprint-newline :fill stream)
	   (pprint-tab :section-relative 1 1 stream)
	   (when (zerop i) (write-string "  "  stream))
	   (write (row-major-aref array i)  :stream stream)
	   (pprint-pop)))
    (pprint-newline :mandatory stream)))



