

(defpackage jtypes
  (:use #:cl)
  (:export
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; arrays.lisp
   ;;
   ;; types
   #:agglomeration ;; list or array
   #:fixnum-vector #:single-float-vector #:double-float-vector
   #:fixnumvec #:floatvec #:dblvec  ;; nicknames for common cases
   #:fixnum-matrix #:single-float-matrix #:double-float-matrix 
   #:fixnummat #:floatmat #:dblmat
   ;;
   #:make-fixnum-vector
   #:make-single-float-vector
   #:make-double-float-vector
   #:make-fixnum-matrix
   #:make-single-float-matrix
   #:make-double-float-matrix

   ;;
   #:coerce-agglomeration-to-double-float-vector
   #:coerce-agglomeration-to-single-float-vector
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; numbers.lisp
   ;;
   ;; types for integers (smaller than fixnums) that are fast because of no 
   ;; bounds checking
   #:fast-signed-int
   #:fast-unsigned-int
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ))