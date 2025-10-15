;;;;
;;;; *************************************************************************
;;;; Adapted from: Kevin M. Rosenberg, ffi.lisp in cl-fftw3 which is
;;;; Copyright (c) 2009 by Kevin M. Rosenberg
;;;;
;;;; Users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;; *************************************************************************

(in-package #:fftw3lib)



(defconstant +fftw-forward+ -1)
(defconstant +fftw-backward+ +1)

(defconstant +fftw-r2hc+ 0)
(defconstant +fftw-hc2r+ 1)

(defconstant +fftw-measure+ 0)
(defconstant +fftw-destroy-input+ (ash 1 0))
(defconstant +fftw-unaligned+ (ash 1 1))
(defconstant +fftw-conservative-memory+ (ash 1 2))
(defconstant +fftw-exhaustive+ (ash 1 3))
(defconstant +fftw-preserve-input+ (ash 1 4))
(defconstant +fftw-patient+ (ash 1 5))
(defconstant +fftw-estimate+ (ash 1 6))

(defconstant +size-double+ (cffi:foreign-type-size :double))
(defconstant +size-float+ (cffi:foreign-type-size :float))

;; the double float version (standard)
(cffi:define-foreign-library fftw3
  ((or :darwin :macosx) (:or #P"/opt/local/lib/libfftw3.dylib"
			     #P"/usr/lib/libfftw3.dylib"))
  (:linux (:or #P"/usr/lib/libfftw3.so"
	       #P"/usr/local/lib/libfftw3.so"
	       #+x86-64  #P"/usr/lib/x86_64-linux-gnu/libfftw3.so"
	       #+x86-64  #P"/usr/lib/x86_64-linux-gnu/libfftw3.so.3"
	       #+x86-64  #P"/usr/lib64/libfftw3.so"
	       #+x86-64  #P"/usr/lib64/libfftw3.so.3"
	       ))
  (t (:default "libfftw3")))

;; the single float version
(cffi:define-foreign-library fftw3f
    ((or :darwin :macosx) (:or #P"/opt/local/lib/libfftw3f.dylib"
                               #P"/usr/lib/libfftw3f.dylib"))
    (:linux (:or #P"/usr/lib/libfftw3f.so"
                 #P"/usr/local/lib/libfftw3f.so"
		 #+x86-64  #P"/usr/lib/x86_64-linux-gnu/libfftw3f.so"
		 #+x86-64  #P"/usr/lib/x86_64-linux-gnu/libfftw3f.so.3"
  		 #+x86-64  #P"/usr/lib64/libfftw3f.so"
		 #+x86-64  #P"/usr/lib64/libfftw3f.so.3"
		 ))
  (t (:default "libfftw3f")))

(cffi:use-foreign-library fftw3)
(cffi:use-foreign-library fftw3f)



(cffi:defcfun free :void
  (ptr :pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define the double precision versions - denoted by fftwd-XXX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defctype fftwd-plan :pointer)
 
(cffi:defcstruct fftwd-complex-struct
  "Complex number structure."
  (re :double)
  (im :double))

(declaim (inline fftwd-plan-dft-1d))
(cffi:defcfun ("fftw_plan_dft_1d" fftwd-plan-dft-1d) fftwd-plan
  (n :int)
  (in (:pointer (:struct fftwd-complex-struct)))
  (out (:pointer (:struct fftwd-complex-struct)))
  (sign :int)
  (flags :uint))

(declaim (inline fftwd-plan-dft-2d))
(cffi:defcfun ("fftw_plan_dft_2d" fftwd-plan-dft-2d) fftwd-plan
  (n0 :int)
  (n1 :int)
  (in (:pointer (:struct fftwd-complex-struct)))
  (out (:pointer (:struct fftwd-complex-struct)))
  (sign :int)
  (flags :uint))

(declaim (inline fftwd-plan-dft-3d))
(cffi:defcfun ("fftw_plan_dft_3d" fftwd-plan-dft-3d) fftwd-plan
  (n0 :int)
  (n1 :int)
  (n2 :int)
  (in (:pointer (:struct fftwd-complex-struct)))
  (out (:pointer (:struct fftwd-complex-struct)))
  (sign :int)
  (flags :uint))

(declaim (inline fftwd-plan-dft))
(cffi:defcfun ("fftw_plan_dft" fftwd-plan-dft) fftwd-plan
  (rank :int)
  (n (:pointer :int))
  (in (:pointer (:struct fftwd-complex-struct)))
  (out (:pointer (:struct fftwd-complex-struct)))
  (sign :int)
  (flags :uint))

(declaim (inline fftwd-plan-r2r-1d))
(cffi:defcfun ("fftw_plan_r2r_1d" fftwd-plan-r2r-1d) fftwd-plan
  (n :int)
  (in :pointer)
  (out :pointer)
  (kind :int)
  (flags :uint))

(declaim (inline fftwd-plan-r2r-2d))
(cffi:defcfun ("fftw_plan_r2r_2d" fftwd-plan-r2r-2d) fftwd-plan
  (n0 :int)
  (n1 :int)
  (in :pointer)
  (out :pointer)
  (kind :int)
  (flags :uint))

(declaim (inline fftwd-plan-r2r-3d))
(cffi:defcfun ("fftw_plan_r2r_3d" fftwd-plan-r2r-3d) fftwd-plan
  (n0 :int)
  (n1 :int)
  (n2 :int)
  (in :pointer)
  (out :pointer)
  (kind :int)
  (flags :uint))

(declaim (inline fftwd-plan-dft-r2c-1d))
(cffi:defcfun ("fftw_plan_dft_r2c_1d" fftwd-plan-dft-r2c-1d) fftwd-plan
  (n :int)
  (in :pointer)
  (out :pointer)
  (flags :uint))

(declaim (inline fftwd-plan-dft-r2c-2d))
(cffi:defcfun ("fftw_plan_dft_r2c_2d" fftwd-plan-dft-r2c-2d) fftwd-plan
  (n0 :int)
  (n1 :int)
  (in :pointer)
  (out :pointer)
  (flags :uint))

(declaim (inline fftwd-plan-dft-r2c-3d))
(cffi:defcfun ("fftw_plan_dft_r2c_3d" fftwd-plan-dft-r2c-3d) fftwd-plan
  (n0 :int)
  (n1 :int)
  (n2 :int)
  (in :pointer)
  (out :pointer)
  (flags :uint))

(declaim (inline fftwd-plan-dft-r2c))
(cffi:defcfun ("fftw_plan_dft_r2c" fftwd-plan-dft-r2c) fftwd-plan
  (rank :int)
  (n (:pointer :int))
  (in :pointer)
  (out :pointer)
  (flags :uint))

(declaim (inline fftwd-plan-dft-c2r-1d))
(cffi:defcfun ("fftw_plan_dft_c2r_1d" fftwd-plan-dft-c2r-1d) fftwd-plan
    (n :int)
    (in :pointer)
    (out :pointer)
    (flags :uint))

(declaim (inline fftwd-plan-dft-c2r-2d))
(cffi:defcfun ("fftw_plan_dft_c2r_2d" fftwd-plan-dft-c2r-2d) fftwd-plan
    (n0 :int)
    (n1 :int)
    (in :pointer)
    (out :pointer)
    (flags :uint))

(declaim (inline fftwd-plan-dft-c2r-3d))
(cffi:defcfun ("fftw_plan_dft_c2r_3d" fftwd-plan-dft-c2r-3d) fftwd-plan
    (n0 :int)
    (n1 :int)
    (n2 :int)
    (in :pointer)
    (out :pointer)
    (flags :uint))

(declaim (inline fftwd-plan-dft-c2r))
(cffi:defcfun ("fftw_plan_dft_c2r" fftwd-plan-dft-c2r) fftwd-plan
    (rank :int)
    (n (:pointer :int))
    (in :pointer)
    (out :pointer)
    (flags :uint))

(declaim (inline fftwd-plan-many-dft))
(cffi:defcfun ("fftw_plan_many_dft" fftwd-plan-many-dft) fftwd-plan
    (rank :int)
    (n (:pointer :int))
    (howmany :int)
    (in (:pointer (:struct fftwd-complex-struct)))
    (inembed (:pointer :int))
    (istride :int)
    (idist :int)
    (out (:pointer (:struct fftwd-complex-struct)))
    (onembed (:pointer :int))
    (ostride :int)
    (odist :int)
    (sign :int)
    (flags :uint))

(declaim (inline fftwd-plan-many-dft_r2c))
(cffi:defcfun ("fftw_plan_many_dft_r2c" fftwd-plan-many-dft-r2c) fftwd-plan
    (rank :int)
    (n (:pointer :int))
    (howmany :int)
    (in (:pointer :double))
    (inembed (:pointer :int))
    (istride :int)
    (idist :int)
    (out (:pointer (:struct fftwd-complex-struct)))
    (onembed (:pointer :int))
    (ostride :int)
    (odist :int)
    (flags :uint))

(declaim (inline fftwd-plan-many-dft_c2r))
(cffi:defcfun ("fftw_plan_many_dft_c2r" fftwd-plan-many-dft-c2r) fftwd-plan
    (rank :int)
    (n (:pointer :int))
    (howmany :int)
    (in (:pointer (:struct fftwd-complex-struct)))
    (inembed (:pointer :int))
    (istride :int)
    (idist :int)
    (out (:pointer :double))
    (onembed (:pointer :int))
    (ostride :int)
    (odist :int)
    (flags :uint))

#+nil
(declaim (inline fftwd-plan-many-dft_r2r))
#+nil
(cffi:defcfun ("fftw_plan_many_dft_r2r" fftwd-plan-many-dft-r2r) fftwd-plan
    (rank :int)
    (n (:pointer :int))
    (howmany :int)
    (in (:pointer :double))
    (inembed (:pointer :int))
    (istride :int)
    (idist :int)
    (out (:pointer :double))
    (onembed (:pointer :int))
    (ostride :int)
    (odist :int)
    (kind (:pointer :int))
    (flags :uint))


(declaim (inline fftwd-malloc))
(cffi:defcfun ("fftw_malloc" fftwd-malloc)
    (:pointer (:struct fftwd-complex-struct))
  (n :int))

(declaim (inline fftwd-execute))
(cffi:defcfun ("fftw_execute" fftwd-execute) :void
    (p fftwd-plan))

(declaim (inline fftwd-destroy-plan))
(cffi:defcfun ("fftw_destroy_plan" fftwd-destroy-plan) :void
    (p fftwd-plan))

(declaim (inline fftwd-free))
(cffi:defcfun ("fftw_free" fftwd-free) :void
    (p :pointer))


;;; Wisdom functions

(cffi:defcfun ("fftw_import_wisdom_from_string"
	       fftwd-import-wisdom-from-string) :int
  (input-string :string))

(cffi:defcfun ("fftw_export_wisdom_to_string"
	       fftwd-export-wisdom-to-string) :string+ptr
  )

(cffi:defcfun ("fftw_cleanup" fftwd-cleanup) :void)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define the single precision versions - denoted by fftwf-XXX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defctype fftwf-plan :pointer)

(cffi:defcstruct fftwf-complex-struct
  "Complex number structure."
  (re :float)
  (im :float))

(declaim (inline fftwf-plan-dft-1d))
(cffi:defcfun ("fftwf_plan_dft_1d" fftwf-plan-dft-1d) fftwf-plan
  (n :int)
  (in (:pointer (:struct fftwf-complex-struct)))
  (out (:pointer (:struct fftwf-complex-struct)))
  (sign :int)
  (flags :uint))

(declaim (inline fftwf-plan-dft-2d))
(cffi:defcfun ("fftwf_plan_dft_2d" fftwf-plan-dft-2d) fftwf-plan
  (n0 :int)
  (n1 :int)
  (in (:pointer (:struct fftwf-complex-struct)))
  (out (:pointer (:struct fftwf-complex-struct)))
  (sign :int)
  (flags :uint))

(declaim (inline fftwf-plan-dft-3d))
(cffi:defcfun ("fftwf_plan_dft_3d" fftwf-plan-dft-3d) fftwf-plan
  (n0 :int)
  (n1 :int)
  (n2 :int)
  (in (:pointer (:struct fftwf-complex-struct)))
  (out (:pointer (:struct fftwf-complex-struct)))
  (sign :int)
  (flags :uint))

(declaim (inline fftwf-plan-dft))
(cffi:defcfun ("fftwf_plan_dft" fftwf-plan-dft) fftwf-plan
  (rank :int)
  (n (:pointer :int))
  (in (:pointer (:struct fftwf-complex-struct)))
  (out (:pointer (:struct fftwf-complex-struct)))
  (sign :int)
  (flags :uint))

(declaim (inline fftwf-plan-r2r-1d))
(cffi:defcfun ("fftwf_plan_r2r_1d" fftwf-plan-r2r-1d) fftwf-plan
  (n :int)
  (in :pointer)
  (out :pointer)
  (kind :int)
  (flags :uint))

(declaim (inline fftwf-plan-r2r-2d))
(cffi:defcfun ("fftwf_plan_r2r_2d" fftwf-plan-r2r-2d) fftwf-plan
  (n0 :int)
  (n1 :int)
  (in :pointer)
  (out :pointer)
  (kind :int)
  (flags :uint))

(declaim (inline fftwf-plan-r2r-3d))
(cffi:defcfun ("fftwf_plan_r2r_3d" fftwf-plan-r2r-3d) fftwf-plan
  (n0 :int)
  (n1 :int)
  (n2 :int)
  (in :pointer)
  (out :pointer)
  (kind :int)
  (flags :uint))

(declaim (inline fftwf-plan-dft-r2c-1d))
(cffi:defcfun ("fftwf_plan_dft_r2c_1d" fftwf-plan-dft-r2c-1d) fftwf-plan
  (n :int)
  (in :pointer)
  (out :pointer)
  (flags :uint))

(declaim (inline fftwf-plan-dft-r2c-2d))
(cffi:defcfun ("fftwf_plan_dft_r2c_2d" fftwf-plan-dft-r2c-2d) fftwf-plan
  (n0 :int)
  (n1 :int)
  (in :pointer)
  (out :pointer)
  (flags :uint))

(declaim (inline fftwf-plan-dft-r2c-3d))
(cffi:defcfun ("fftwf_plan_dft_r2c_3d" fftwf-plan-dft-r2c-3d) fftwf-plan
  (n0 :int)
  (n1 :int)
  (n2 :int)
  (in :pointer)
  (out :pointer)
  (flags :uint))

(declaim (inline fftwf-plan-dft-r2c))
(cffi:defcfun ("fftwf_plan_dft_r2c" fftwf-plan-dft-r2c) fftwf-plan
  (rank :int)
  (n (:pointer :int))
  (in :pointer)
  (out :pointer)
  (flags :uint))

(declaim (inline fftwf-plan-dft-c2r-1d))
(cffi:defcfun ("fftwf_plan_dft_c2r_1d" fftwf-plan-dft-c2r-1d) fftwf-plan
    (n :int)
    (in :pointer)
    (out :pointer)
    (flags :uint))

(declaim (inline fftwf-plan-dft-c2r-2d))
(cffi:defcfun ("fftwf_plan_dft_c2r_2d" fftwf-plan-dft-c2r-2d) fftwf-plan
    (n0 :int)
    (n1 :int)
    (in :pointer)
    (out :pointer)
    (flags :uint))

(declaim (inline fftwf-plan-dft-c2r-3d))
(cffi:defcfun ("fftwf_plan_dft_c2r_3d" fftwf-plan-dft-c2r-3d) fftwf-plan
    (n0 :int)
    (n1 :int)
    (n2 :int)
    (in :pointer)
    (out :pointer)
    (flags :uint))

(declaim (inline fftwf-plan-dft-c2r))
(cffi:defcfun ("fftwf_plan_dft_c2r" fftwf-plan-dft-c2r) fftwf-plan
    (rank :int)
    (n (:pointer :int))
    (in :pointer)
    (out :pointer)
    (flags :uint))

(declaim (inline fftwf-plan-many-dft))
(cffi:defcfun ("fftwf_plan_many_dft" fftwf-plan-many-dft) fftwf-plan
    (rank :int)
    (n (:pointer :int))
    (howmany :int)
    (in (:pointer (:struct fftwf-complex-struct)))
    (inembed (:pointer :int))
    (istride :int)
    (idist :int)
    (out (:pointer (:struct fftwf-complex-struct)))
    (onembed (:pointer :int))
    (ostride :int)
    (odist :int)
    (sign :int)
    (flags :uint))

(declaim (inline fftwf-plan-many-dft_r2c))
(cffi:defcfun ("fftwf_plan_many_dft_r2c" fftwf-plan-many-dft-r2c) fftwf-plan
    (rank :int)
    (n (:pointer :int))
    (howmany :int)
    (in (:pointer :float))
    (inembed (:pointer :int))
    (istride :int)
    (idist :int)
    (out (:pointer (:struct fftwf-complex-struct)))
    (onembed (:pointer :int))
    (ostride :int)
    (odist :int)
    (flags :uint))

(declaim (inline fftwf-plan-many-dft_c2r))
(cffi:defcfun ("fftwf_plan_many_dft_c2r" fftwf-plan-many-dft-c2r) fftwf-plan
    (rank :int)
    (n (:pointer :int))
    (howmany :int)
    (in (:pointer (:struct fftwf-complex-struct)))
    (inembed (:pointer :int))
    (istride :int)
    (idist :int)
    (out (:pointer :float))
    (onembed (:pointer :int))
    (ostride :int)
    (odist :int)
    (flags :uint))

#+nil
(declaim (inline fftwf-plan-many-dft_r2r))
#+nil
(cffi:defcfun ("fftwf_plan_many_dft_r2r" fftwf-plan-many-dft-r2r) fftwf-plan
    (rank :int)
    (n (:pointer :int))
    (howmany :int)
    (in (:pointer :float))
    (inembed (:pointer :int))
    (istride :int)
    (idist :int)
    (out (:pointer :float))
    (onembed (:pointer :int))
    (ostride :int)
    (odist :int)
    (kind (:pointer :int))
    (flags :uint))


(declaim (inline fftwf-malloc))
(cffi:defcfun ("fftwf_malloc" fftwf-malloc)
    (:pointer (:struct fftwf-complex-struct))
  (n :int))

(declaim (inline fftwf-execute))
(cffi:defcfun ("fftwf_execute" fftwf-execute) :void
    (p fftwf-plan))

(declaim (inline fftwf-destroy-plan))
(cffi:defcfun ("fftwf_destroy_plan" fftwf-destroy-plan) :void
    (p fftwf-plan))

(declaim (inline fftwf-free))
(cffi:defcfun ("fftwf_free" fftwf-free) :void
    (p :pointer))


;;; Wisdom functions

(cffi:defcfun ("fftwf_import_wisdom_from_string"
	       fftwf-import-wisdom-from-string) :int
  (input-string :string))

(cffi:defcfun ("fftwf_export_wisdom_to_string"
	       fftwf-export-wisdom-to-string) :string+ptr
  )

;; this is defined, even though fftw_cleanup isn't (in Macports, at least)
(cffi:defcfun ("fftwf_cleanup" fftwf-cleanup) :void)
