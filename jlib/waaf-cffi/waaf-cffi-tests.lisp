
#|

Tests to test WITH-ARRAY-AS-FOREIGN-POINTER. In particular, check correctness of
array copying to and from foreign space.


Run as:
     (WAAF-CFFI-TESTS:RUN-ALL-TESTS)

or sub-tests

     (WAAF-CFFI-TESTS:RUN-ALL-ARRAY-TESTS)
     (WAAF-CFFI-TESTS:RUN--WITH-LISP-VALUES-AS-FOREIGN-POINTERS--TEST)

|#


(defpackage waaf-cffi-tests
  (:use :cl :waaf)
  (:export
   #:run-all-tests
   #:run-all-array-tests
   #:run--with-lisp-values-as-foreign-pointers--test
   ))

(in-package waaf-cffi-tests)


(eval-when (:load-toplevel :compile-toplevel)
  
  (defun %make-test-name (lisp-array-type ffi-type complex)
    (intern
     (string-upcase
      (concatenate 'string "%waaf-test--" (waaf::%make-function-prefix
					   lisp-array-type ffi-type complex)))))
  
  
  (defun %num-coerce (lisp-array-type n)
    (cond ((equal lisp-array-type 'double-float)
	   (float n 1d0))
	  ((equal lisp-array-type 'single-float)
	   (float n 1.0))
	  ((equal lisp-array-type '(complex single-float)) ;; complex has 2 components st
	   (complex (float n 1.0) (float (+ 10 n) 1.0)))
	  ((equal lisp-array-type '(complex double-float)) ;; complex has 2 components st
	 (complex (float n 1d0) (float (+ 10 n) 1d0)))
	  (t
	   n)))
  
  (defun %make-test-vector (lisp-array-type &key (n 10) subst-val)
    (let ((v (make-array n :element-type lisp-array-type)))
      (loop for i below n do (setf (aref v i) (%num-coerce lisp-array-type i)))
      ;; substitute head and tail if asked
    (when subst-val
      (setf (aref v 0) (%num-coerce lisp-array-type subst-val))
      (setf (aref v (1- n)) (%num-coerce lisp-array-type subst-val)))
    v))
  
  (defun arrays-equal-p (v1 v2)
    (and (= (array-total-size v1) (array-total-size v2)) ;; will always be true
	 (loop
	    for i below (array-total-size v1)
	    for x1 = (row-major-aref v1 i) and x2 = (row-major-aref v2 i)
	    when (not (= x1 x2)) do (return NIL)
	    finally (return T))))
  
  
  (defun %do-nothing-function (pointer)
    (declare (ignore pointer))
    "I do nothing with the pointer")
  
  (defparameter *subst-val* 55) ;; sfhifty-five!
  (defparameter *n-test* 10)
  
  )


(defun string-test ()
  (let* ((string "0123456789")
	 (s1 (copy-seq string))
	 (s2 (copy-seq string))
	 (smod  "x12345678x")
	 (passed t))
    (format t "  Testing STRING full copy to foreign and back~%")
    (with-array-as-foreign-pointer 
	(s1 ptr :string)
      (fill s1 #\x)
      (%do-nothing-function ptr))
    (if (string-equal string s1)
	(format t "  passed~%")
	(progn 
	  (setf passed nil)
	  (format t "  FAILED~%    EXPECTED: ~A~%    GOT:      ~A~%" string s1)))
    ;;
    (format t "  Testing STRING partial copy to foreign and back~%")
    (with-array-as-foreign-pointer 
	(s2 ptr :string  :start 1 :end 9)
      (fill s2 #\x)
      (%do-nothing-function ptr))
    (if (string-equal smod s2)
	(format t "  passed~%")
	(progn 
	  (setf passed nil)
	  (format t "  FAILED~%    EXPECTED: ~A~%    GOT:      ~A~%" smod s2)))
    (values passed 'string :STRING)))
    


;; macro that makes a function that runs a FFI allocation and copy of a particular triplet of
;; LISP-ARRAY-TYPE FFI-TYPE COMPLEX
(defmacro make-test (lisp-array-type ffi-type complex)
  (let ((test-name (%make-test-name lisp-array-type ffi-type complex)))
    `(defun ,test-name ()
       (let* ((v (%make-test-vector ',lisp-array-type  :n *n-test*))
	      (vp (copy-seq v)) ;; for partial copy test
	      ;; what we put in
	      (vin (copy-seq v))
	      ;; modified version expected as result of 2nd test
	      (vmod (%make-test-vector ',lisp-array-type  :n *n-test* :subst-val *subst-val*))
	      (passed t))
	 (format t "  Testing LISP-TYPE=~A  FFI-TYPE=~A, COMPLEX=~A full copy to foreign and back~%"
		 ',lisp-array-type ,ffi-type ,complex)
	 (waaf:with-array-as-foreign-pointer     
	     (v ptr ,ffi-type  :copy-to-foreign t :copy-from-foreign t :complex ,complex 
		:lisp-type ,lisp-array-type)
	   (%do-nothing-function ptr)) ;; do nothing so this code doesn't get optimized out
	 ;; now ptr is deallocated
	 (if (arrays-equal-p v vin)
	     (format t "  passed~%")
	     (progn 
	       (setf passed nil)
	       (format t "  FAILED~%    EXPECTED: ~A~%    GOT:      ~A~%" vin v)))
	 ;; now do test with partial copy
	 (format t "  Testing LISP-TYPE=~A  FFI-TYPE=~A, COMPLEX=~A partial copy to foreign and back~%"
		 ',lisp-array-type ,ffi-type ,complex)
	 (waaf:with-array-as-foreign-pointer      
	     (vp ptr ,ffi-type  :copy-to-foreign t :copy-from-foreign t :complex ,complex 
		 :lisp-type ,lisp-array-type
		 :start 1 :end (1- *n-test*))
	   ;; fill the array with *SUBST-VAL* which should remain when the array is only
	   ;; partially copied back
	   (fill vp (%num-coerce ',lisp-array-type *subst-val*))
	   (%do-nothing-function ptr))  ;; do nothing so this code doesn't get optimized out
	 ;; now ptr is deallocated
	 (if (arrays-equal-p vp vmod)
	     (format t "  passed~%")
	     (progn 
	       (setf passed nil)
	       (format t "  FAILED~%    EXPECTED: ~A~%    GOT:      ~A~%" vmod v)))
	 (values passed ',lisp-array-type ,complex)))))


(eval-when (:compile-toplevel :load-toplevel)
  (defvar *test-list* NIL))

;; again with the heinous reader macro to make all the tests
#.(progn
    `(progn
       ;; put the tests into the *test-list*
       ,@(loop 
	    initially 
	      (setf *test-list* nil)
	      (pushnew 'string-test *test-list*)	      
	    for func-desc in waaf-cffi::*lisp-ffi-array-pairings*
	    do (pushnew (%make-test-name  (first func-desc) 
					  (second func-desc)
					  (third func-desc))
			*test-list*))
       
       ;; make defuns for the tests
       ,@(loop 
	    for func-desc in waaf-cffi::*lisp-ffi-array-pairings*
	    collect `(make-test ,(first func-desc) 
				,(second func-desc)
				,(third func-desc)))))





(defun run-all-array-tests ()
  (loop 
     with failed-func-list = nil
     with failed-pair-list = nil
     with ntest = 0
     with npassed = 0
     with nfailed = 0
     for func-name in *test-list*
     do
       (incf ntest)
       (format t "Test ~A~%" func-name)
       (multiple-value-bind (passed lisp-type ffi-type)
	   (funcall func-name)
	 (if passed
	   (incf npassed)
	   (progn
	     (incf nfailed)
	     (push func-name failed-func-list)
	     (push (list lisp-type ffi-type) failed-pair-list))))
       (terpri t)
     finally
       (terpri) 
       (format t "~D/~D Passed  ~D/~D Failed~%" npassed ntest nfailed ntest)
       (when (plusp nfailed)
	 (terpri)
	 (format t "Failed tests:~%~%")
	 (loop 
	    for failed-func in failed-func-list
	    for failed-pair in failed-pair-list
	    do
	      (format t "   ~A~%" failed-func)
	      (format t "   LISP-TYPE=~S   FFI-TYPE=~A~%~%" 
		      (first failed-pair) (second failed-pair))))
       ;; return T for success
       (return
	 (zerop nfailed))))
	      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; this is not exhaustive
(defun run--with-lisp-values-as-foreign-pointers--test ()
  (let ((v1 88888)
	(v2 2)
	(v3 3d0)
	(v4 -98765)
	(v5 +9)
	(v6 -9)
	(v7 #.(1- (expt 2 63)))
	(v8 #.(- (1+ (expt 2 62))))
	(v9 #.(1+ (expt 2 15)))
	(v10 #.(- (1- (expt 2 15))))
	;;
	(nfailed 0))
    (multiple-value-bind (u1 u2 u3 u4 u5 u6 u7 u8)
	(waaf:with-lisp-values-as-foreign-pointers 
	    ((v1 p1 :long)
	     (v2 p2 :float)
	     (v3 p3 :double)
	     (v4 p4 :int)
	     (v5 p5 :uchar)
	     (v6 p6 :char)
	     (v7 p7 :uint64)
	     (v8 p8 :int64)
	     (v9 p9 :ushort)
	     (v10 p10 :short))
	  (%do-nothing-function p4))
      (loop 
	 for type in (list :long :float :double :int :uchar :char :uint64 :int64 :ushort :short)
	 for input in (list v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)
	 for output in (list u1 u2 u3 u4 u5 u6 u7 u8 v9 v10)
	 for success = (= input output)
	 do
	   (when (not success) (incf nfailed))
	   (format t "~10A  ~8A   in=~20A  out=~20A~%"
		   (if success "SUCCESS  " "FAILURE**") 
		   type input output)))
    ;;
    (zerop nfailed))) ;; T on all success






(defun run-all-tests ()
  (let ((arrays-OK (prog1 (run-all-array-tests) (terpri) (terpri)))
	(with-lisp-values-as-foreign-pointers-OK
	    (prog1 (run--with-lisp-values-as-foreign-pointers--test)
	      (terpri) (terpri))))
    (terpri)
    (terpri)
    (format t "Array test ~A~%" (if arrays-ok "SUCCEEDED" "FAILED"))
    (format t "WITH-LISP-VALUES-AS-FOREIGN-POINTERS ~A~%"
	    (if with-lisp-values-as-foreign-pointers-OK
		"SUCCEEDED" "FAILED"))
    ;;
    (and arrays-OK with-lisp-values-as-foreign-pointers-OK)))


	 
	 
	