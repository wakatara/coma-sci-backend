
(in-package fftw3lib)




;; parent structure for transforms
(defstruct fftw3-plan
  ;; the rank and dimensions of the INPUT array
  (rank        0   :type unsigned-byte)
  (ndim        nil   :type (or null (simple-array fixnum (*))))
  (ndim-ptr    nil)
  (plan-ptr    nil) ;; can't specify something like (:TYPE CFFI:POINTER)
  ;; FMEM strctures with blocks of foreign memory allocated by
  ;; FFTWF-MALLOC or FFTWD-MALLOC
  (in-fmem     (make-fmem))
  (out-fmem    (make-fmem))
  (direction   nil :type (member nil -1 1)) ;; exponent, or 0 if inapplicable
  ;; the routine COPY-ARRAY-FROM-FFTW3-PLAN applies this
  (normalization 1d0 :type double-float)
  (in-equals-out nil) ;; if in-fmem is the same as out-fmem
  (float-type  nil :type (member nil :float :double)) ;; fftwf or fftwd
  (n-elem-in   0     :type (unsigned-byte 63))
  (n-elem-out  0     :type (unsigned-byte 63))
  (input-type  nil)  ;; :float :double :complex-float :complex-double
  (output-type nil)  ;; :float :double :complex-float :complex-double
  )

;; structure for foreign memory allocated with fftw3. Keeps track of whether
;; allocated by fftwD or fftwF, and whether it has been deallocated
(defstruct fmem
  (type nil) ;; :FLOAT for FFTWF-MALLOC, :DOUBLE for FFTWD-MALLOC
  (nbytes 0 :type fixnum)
  (ptr nil))

(defun allocate-fmem (fmem nbytes type)
  (declare (type fmem fmem)
	   (type fixnum nbytes)
	   (type (member :float :double) type))
  (when (fmem-ptr fmem)
    (error "Fmem is already allocated in ALLOCATE-FMEM."))
  (let ((ptr (cond ((eq type :float)
		    (fftwf-malloc nbytes))
		   ((eq type :double)
		    (fftwd-malloc nbytes)))))
    (when (cffi:null-pointer-p ptr)
      (error "Null pointer returned by ALLOCATE-FMEM FFTW3 memory allocator."))
    (setf (fmem-type   fmem) type
	  (fmem-nbytes fmem) nbytes
	  (fmem-ptr    fmem) ptr)
    fmem))


;; it is OK to deallocate multiple times; the first times, the
;; pointer is nulled out and ignored subsquently
(defun free-fmem (fmem)
  (declare (type fmem fmem))
  (when (fmem-ptr fmem)
    (cond ((eq (fmem-type fmem) :float)
	   (fftwf-free (fmem-ptr fmem)))
	  ((eq (fmem-type fmem) :double)
	   (fftwd-free (fmem-ptr fmem)))))
  (setf (fmem-nbytes fmem) 0
	(fmem-ptr fmem) nil))

  
  



   



(defmethod print-object ((obj fftw3-plan) stream)
  (format stream "#<FFTW3-PLAN DIMS=~A INPUT=~A OUTPUT=~A~A ALLOC=~A NORM=~A>"
	  (fftw3-plan-ndim obj)
	  (fftw3-plan-input-type obj)
	  (fftw3-plan-output-type obj)
	  (if (fftw3-plan-in-equals-out obj) " IN=OUT" "")
	  (cond ((and (fmem-ptr (fftw3-plan-in-fmem obj))
		      (fmem-ptr (fftw3-plan-out-fmem obj))
		      (fftw3-plan-plan-ptr obj))
		 "Y")
		((and (not (fmem-ptr (fftw3-plan-in-fmem obj)))
		      (not (fmem-ptr (fftw3-plan-out-fmem obj)))
		      (not (fftw3-plan-plan-ptr obj)))
		 "N")
		(t
		 "Partial"))
	  (if (= 1 (fftw3-plan-normalization obj)) "N" "Y")))

;; a struct holding two plans, for forward and backward transformations
(defstruct fftw3-plan-pair
  (plan-forward  nil :type (or null fftw3-plan))
  (plan-backward nil :type (or null fftw3-plan)))

(defmethod print-object ((obj fftw3-plan-pair) stream)
  (let ((plan (fftw3-plan-pair-plan-forward obj)))
    (format stream "#<FFTW3-PLAN-PAIR DIMS=~A  ~A / ~A>"
	    (fftw3-plan-ndim plan)
	    (fftw3-plan-input-type plan)
	    (fftw3-plan-output-type plan))))
	  
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a hash of existing plans, so that we can destroy them and free
;; foreign memory, in case we lose track of them
(defvar *fftw3-plan-lock*   (bordeaux-threads:make-lock "fftw3-plan-lock"))
(defvar *fftw3-plan-hash*   (make-hash-table :test 'eq)) ;; KEY=PLAN, VALUE=T
(defun insert-fftw3-plan-in-hash (plan)
  (bordeaux-threads:with-lock-held (*fftw3-plan-lock*)
    (setf (gethash plan *fftw3-plan-hash*) t))
  plan)
(defun remove-fftw3-plan-from-hash (plan)
  (bordeaux-threads:with-lock-held (*fftw3-plan-lock*)
    (remhash plan *fftw3-plan-hash*)))
(defun destroy-all-fftw3-plans ()
  (bordeaux-threads:with-lock-held (*fftw3-plan-lock*)
    (loop for plan being the hash-value of *fftw3-plan-hash*
	  do (destroy-fftw3-plan plan)
	     (remhash plan *fftw3-plan-hash*))))
	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun %get-elem-size (type) ;; :float :double :complex-float :complex-double
  (declare (type (member :float :double :complex-float :complex-double)
		 type))
  (cond ((eq type :float) (cffi:foreign-type-size :float))
	((eq type :double) (cffi:foreign-type-size :double))
	((eq type :complex-float) (* 2 (cffi:foreign-type-size :float)))
	((eq type :complex-double) (* 2 (cffi:foreign-type-size :double)))))
	 

;; allocate the memory for a plan
(defun allocate-fftw3-plan
    (ndimvec      ;; (simple-array fixnum (*)) 
     float-type   ;; :float or :double
     input-type   ;; :float :double :complex-float :complex-double
     output-type  ;; :float :double :complex-float :complex-double
     n-elem-in
     n-elem-out
     &key
       (in-equals-out nil)
       ;; may want to share IN-FMEM with another plan (typically, the
       ;; reverse transform)
       (existing-in-fmem nil)
       (existing-out-fmem nil)
       ;; it's the job of every high-level FFT routine to
       ;; apply the normalization
       (normalization 1d0))
       
  (declare
   (type (simple-array fixnum (*)) ndimvec)
   (type (member :float :double :complex-float :complex-double)
	 input-type output-type)
   (type (unsigned-byte 63)  n-elem-in n-elem-out)
   (type double-float normalization))

  (let (in-fmem out-fmem ndim-ptr ok)
    (setf ok nil)
    (unwind-protect ;; if an error occurs in the middle
	 (progn
	   (let* ((size-elem-in (%get-elem-size input-type))
		  (size-elem-out (%get-elem-size output-type))
		  (mem-size-in (* size-elem-in  n-elem-in))
		  (mem-size-out (* size-elem-out  n-elem-out))
		  (rank (length ndimvec))
		  (input-float-type (if (or (eq input-type :float)
					    (eq input-type :complex-float))
					:float :double))
		  (output-float-type (if (or (eq output-type :float)
					     (eq output-type :complex-float))
					 :float :double)))

	     (when (or (not (eq input-float-type output-float-type))
		       (not (eq float-type input-float-type)))
	       (error "Non matching float types: float-type=~A, input-float-type=~AA and output-float-type=~A"
		     float-type input-float-type output-float-type))
	     
		  

	     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	     ;; a bit of hackery to handle case of in-fmem = out-fmem
	     (let ((true-mem-size-in (if in-equals-out
					 (max mem-size-in mem-size-out)
					 mem-size-in)))
	       (setf in-fmem (or existing-in-fmem
				 (allocate-fmem
				  (make-fmem) true-mem-size-in float-type)))
	       (setf out-fmem (or existing-out-fmem
				  (if in-equals-out
				      in-fmem
				      (allocate-fmem
				       (make-fmem) mem-size-out float-type)))))
	     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	     (setf ndim-ptr
		   (cffi:foreign-alloc :int :count rank))

	     (when (and in-fmem out-fmem ndim-ptr
			(not (cffi:null-pointer-p ndim-ptr))
			(not (cffi:null-pointer-p (fmem-ptr in-fmem)))
			(not (cffi:null-pointer-p (fmem-ptr out-fmem))))
	       (setf ok t))
	     ;;
	     (loop for n of-type fixnum across ndimvec
		   for i of-type fixnum from 0
		   do (setf (cffi:mem-aref ndim-ptr :int i) n))
	     ;;
	     (insert-fftw3-plan-in-hash
	      (make-fftw3-plan
	       :rank rank
	       :ndim ndimvec
	       :ndim-ptr ndim-ptr
	       :plan-ptr nil ;; to be filled in later
	       :in-fmem in-fmem
	       :out-fmem out-fmem
	       :normalization normalization
	       :in-equals-out in-equals-out
	       :float-type float-type
	       :n-elem-in n-elem-in
	       :n-elem-out n-elem-out
	       :input-type input-type
	       :output-type output-type))))
	   
	   
      (progn ;; unwind-protect form
	(when (not ok)
	  (when in-fmem
	    (when ndim-ptr (cffi:foreign-free ndim-ptr))
	    (when in-fmem (free-fmem in-fmem))
	    (when out-fmem (free-fmem out-fmem))))))))
		

(defun destroy-fftw3-plan (plan)
  (declare (type fftw3-plan plan))
  ;; if plan-ptr exists, destroy it
  (when (fftw3-plan-plan-ptr plan)
    (cond ((eq (fftw3-plan-float-type plan) :double)
	   (fftwd-destroy-plan (fftw3-plan-plan-ptr plan)))
	  ((eq (fftw3-plan-float-type plan) :float)
	   (fftwf-destroy-plan (fftw3-plan-plan-ptr plan))))
    (setf (fftw3-plan-plan-ptr plan) nil))
  (free-fmem (fftw3-plan-in-fmem plan))
  (free-fmem (fftw3-plan-out-fmem plan))
  (cffi:foreign-free (fftw3-plan-ndim-ptr plan))
  (setf (fftw3-plan-ndim-ptr plan) nil)
  (remove-fftw3-plan-from-hash plan)
  plan)

(defun destroy-fftw3-plan-pair (plan-pair)
  (declare (type fftw3-plan-pair plan-pair))
  (destroy-fftw3-plan (fftw3-plan-pair-plan-forward plan-pair))
  (destroy-fftw3-plan (fftw3-plan-pair-plan-backward plan-pair)))


(defun array-input-dimensions-match-plan-p (array plan)
  (and (= (array-rank array)
	  (fftw3-plan-rank plan))
       ;;
       (loop for ndim across (fftw3-plan-ndim plan)
	     for i from 0
	     for na = (array-dimension array i)
	     when (not (= na ndim))
	       do (return nil)
	     finally (return t))))


(defun copy-array-into-fftw3-plan (array plan)
  (declare (type array array)
	   (type fftw3-plan plan))
  (let ((ats (array-total-size array)))
    ;;
    (when (not (= (fftw3-plan-n-elem-in plan) ats))
      (error "total size of ARRAY=~D is larger than N-ELEM-IN=~D"
	     ats  (fftw3-plan-n-elem-in plan)))
    ;;
    (macrolet
	((coerce-by-cffi-type (x cffi-type)
	   `(float ,x ,(if (eq cffi-type :double) 1d0 1.0)))
	 ;;
	 ;; (COPY-XXX-TO-YYY XXX-TYPE YYY-TYPE) is a macro that copies
	 ;; from lisp array of type XXX to type YYY in FFI memory
	 (copy-real-to-real (lisp-type cffi-type)
	   `(locally
		(declare (type (simple-array ,lisp-type) array)
			 (optimize (speed 3) (safety 0)))
	      (loop
		with in-ptr = (fmem-ptr (fftw3-plan-in-fmem plan))
		for i of-type small-index below ats
		for x of-type ,lisp-type = (row-major-aref array i)
		do (setf (cffi:mem-aref in-ptr ,cffi-type i)
			 (coerce-by-cffi-type x ,cffi-type)))))
	 ;;
	 (copy-complex-to-complex (lisp-type cffi-type)
	   `(locally
		(declare (type (simple-array (complex ,lisp-type)) array)
			 (optimize (speed 3) (safety 0)))
	      (loop
		with in-ptr = (fmem-ptr (fftw3-plan-in-fmem plan))
		for i of-type small-index below ats
		for j of-type small-index = (* i 2)
		for x of-type (complex ,lisp-type) = (row-major-aref array i)
		for xr of-type ,lisp-type = (realpart x)
		for xi of-type ,lisp-type = (imagpart x)
		do (setf (cffi:mem-aref in-ptr ,cffi-type j)
			 (coerce-by-cffi-type xr ,cffi-type))
		   (setf (cffi:mem-aref in-ptr ,cffi-type (1+ j))
			 (coerce-by-cffi-type xi ,cffi-type)))))
	 ;;
	 (copy-real-to-complex (lisp-type cffi-type)
	   `(locally
		(declare (type (simple-array ,lisp-type) array)
			 (optimize (speed 3) (safety 0)))
	      (loop
		with in-ptr = (fmem-ptr (fftw3-plan-in-fmem plan))
		for i of-type small-index below ats
		for j of-type small-index = (* i 2)
		for x of-type ,lisp-type = (row-major-aref array i)
		do (setf (cffi:mem-aref in-ptr ,cffi-type j)
			 (coerce-by-cffi-type x ,cffi-type))
		   (setf (cffi:mem-aref in-ptr ,cffi-type (1+ j))
			 (coerce-by-cffi-type 0 ,cffi-type)))))
	 ;;
	 (copy-complex-to-real (lisp-type cffi-type)
	   `(locally
		(declare (type (simple-array (complex ,lisp-type)) array)
			 (optimize (speed 3) (safety 0)))
	      (loop
		with in-ptr = (fmem-ptr (fftw3-plan-in-fmem plan))
		for i of-type small-index below ats
		for x of-type (complex ,lisp-type) = (row-major-aref array i)
		for xr of-type ,lisp-type = (realpart x)
		for xi of-type ,lisp-type = (imagpart x)
		do
		   (when (not (zerop xi))
		     (error "Input array[~D] has a non-zero complex component in a complex-to-real copy." i))
		   (setf (cffi:mem-aref in-ptr ,cffi-type i)
			 (coerce-by-cffi-type xr ,cffi-type))))) 
	 ;;
	 ;; slow copy of anything to complex
	 (copy-generic-to-complex (cffi-type)
	   `(locally
		(declare (type array array))
	      (loop
		with in-ptr = (fmem-ptr (fftw3-plan-in-fmem plan))
		for i of-type small-index below ats
		for j of-type small-index = (* i 2)
		for x of-type  number = (row-major-aref array i)
		for xr of-type real = (realpart x)
		for xi of-type real = (imagpart x)
		do  (setf (cffi:mem-aref in-ptr ,cffi-type j)
			  (coerce-by-cffi-type xr ,cffi-type))
		    (setf (cffi:mem-aref in-ptr ,cffi-type (1+ j))
			  (coerce-by-cffi-type xi ,cffi-type)))))
	 ;;
	 ;; slow copy of anything to real
	 (copy-generic-to-real (cffi-type)
	   `(locally
		(declare (type array array)
			 (optimize (safety 3)))
	      (loop
		with in-ptr = (fmem-ptr (fftw3-plan-in-fmem plan))
		for i of-type small-index below ats
		for x of-type  number = (row-major-aref array i)
		for xr of-type real = (realpart x)
		for xi of-type real = (imagpart x)
		do
		   (when (not (zerop xi))
		     (error "Input array[~D] has a non-zero complex component in a complex-to-real copy." i))
		   (setf (cffi:mem-aref in-ptr ,cffi-type i)
			 (coerce-by-cffi-type xi ,cffi-type))))))
      ;;
      (typecase array
	((simple-array single-float)
	 (cond ((eq (fftw3-plan-input-type plan) :float)
		(copy-real-to-real single-float :float))
	       ((eq (fftw3-plan-input-type plan) :double)
		(copy-real-to-real single-float :double))
	       ((eq (fftw3-plan-input-type plan) :complex-float)
		(copy-real-to-complex single-float :float))
	       ((eq (fftw3-plan-input-type plan) :complex-double)
		(copy-real-to-complex single-float :double))))
	;;
	((simple-array (complex single-float))
	 (cond ((eq (fftw3-plan-input-type plan) :float)
		(copy-complex-to-real single-float :float))
	       ((eq (fftw3-plan-input-type plan) :double)
		(copy-complex-to-real single-float :double))
	       ((eq (fftw3-plan-input-type plan) :complex-float)
		(copy-complex-to-complex single-float :float))
	       ((eq (fftw3-plan-input-type plan) :complex-double)
		(copy-complex-to-complex single-float :double))))
	;;
	((simple-array double-float)
	 (cond ((eq (fftw3-plan-input-type plan) :float)
		(copy-real-to-real double-float :float))
	       ((eq (fftw3-plan-input-type plan) :double)
		(copy-real-to-real double-float :double))
	       ((eq (fftw3-plan-input-type plan) :complex-float)
		(copy-real-to-complex double-float :float))
	       ((eq (fftw3-plan-input-type plan) :complex-double)
		(copy-real-to-complex double-float :double))))
	;;
	((simple-array (complex double-float))
	 (cond ((eq (fftw3-plan-input-type plan) :float)
		(copy-complex-to-real double-float :float))
	       ((eq (fftw3-plan-input-type plan) :double)
		(copy-complex-to-real double-float :double))
	       ((eq (fftw3-plan-input-type plan) :complex-float)
		(copy-complex-to-complex double-float :float))
	       ((eq (fftw3-plan-input-type plan) :complex-double)
		(copy-complex-to-complex double-float :double))))
	;;
	(array
	 (cond ((eq (fftw3-plan-input-type plan) :float)
		(copy-generic-to-real  :float))
	       ((eq (fftw3-plan-input-type plan) :double)
		(copy-generic-to-real :double))
	       ((eq (fftw3-plan-input-type plan) :complex-float)
		(copy-generic-to-complex :float))
	       ((eq (fftw3-plan-input-type plan) :complex-double)
		(copy-generic-to-complex :double)))))
    plan)))
       
  



;; copies array data from (fftw3-plan-in-ptr plan), demanding that
;; there be at least enough points in PLAN
(defun copy-array-from-fftw3-plan (array plan)
  (declare (type array array)
	   (type fftw3-plan plan))
  (let ((ats (array-total-size array)))
    ;;
    (when (not (= (fftw3-plan-n-elem-out plan) ats))
      (error "total size of ARRAY=~D differs from N-ELEM-OUT=~D"
	     ats  (fftw3-plan-n-elem-out plan)))
    ;;
    (macrolet
	((coerce-by-lisp-type (x lisp-type)
	   `(float ,x ,(if (eq lisp-type 'double-float) 1d0 1.0)))
	 ;;
	 ;; (BACKCOPY-XXX-FROM-YYY XXX-TYPE YYY-TYPE) is a macro that
	 ;; copies from type YYY in FFI memory to type XXX in lisp
	 ;; memory
	 (backcopy-real-from-real (lisp-type cffi-type)
	   `(locally
		(declare (type (simple-array ,lisp-type) array)
			 (optimize (speed 3) (safety 0)))
	      (loop
		with norm = (coerce-by-lisp-type (fftw3-plan-normalization plan)
						 ,lisp-type)
		with out-ptr = (fmem-ptr (fftw3-plan-out-fmem plan))
		for i of-type small-index below ats
		for x of-type ,lisp-type
		  = (* norm
		       (coerce-by-lisp-type
			(cffi:mem-aref out-ptr ,cffi-type i) ,lisp-type))
		do (setf (row-major-aref array i) x))))
	 ;;
	 (backcopy-complex-from-complex (lisp-type cffi-type)
	   `(locally
		(declare (type (simple-array (complex ,lisp-type)) array)
			 (optimize (speed 3) (safety 0)))
	      (loop
		with norm = (coerce-by-lisp-type (fftw3-plan-normalization plan)
						 ,lisp-type)
		with out-ptr = (fmem-ptr (fftw3-plan-out-fmem plan))
		for i of-type small-index below ats
		for j of-type small-index = (* i 2)
		for xr of-type ,lisp-type
		  = (* norm
		       (coerce-by-lisp-type (cffi:mem-aref out-ptr ,cffi-type j)
					    ,lisp-type))
		for xi of-type ,lisp-type
		  = (* norm
		       (coerce-by-lisp-type (cffi:mem-aref out-ptr ,cffi-type (1+ j))
					    ,lisp-type))
		do (setf (row-major-aref array i) (complex xr xi)))))
	 ;;
	 (backcopy-real-from-complex (lisp-type cffi-type)
	   `(locally
		(declare (type (simple-array ,lisp-type) array)
			 (optimize (speed 3) (safety 0)))
	      (loop
		with norm = (coerce-by-lisp-type (fftw3-plan-normalization plan)
						 ,lisp-type)
		with out-ptr = (fmem-ptr (fftw3-plan-out-fmem plan))
		for i of-type small-index below ats
		for j of-type small-index = (* i 2)
		for xr of-type ,lisp-type
		  = (* norm
		       (coerce-by-lisp-type (cffi:mem-aref out-ptr ,cffi-type j)
					    ,lisp-type))
		for xi of-type ,lisp-type
		  = (* norm
		       (coerce-by-lisp-type (cffi:mem-aref out-ptr ,cffi-type (1+ j))
					    ,lisp-type))
		do
		   (when (not (zerop xi))
		     (error "Foreign OUT-PTR array[~D] has a non-zero complex component in a real-from-complex copy." i))
		   (setf (row-major-aref array i) xr))))
	 ;;
	 (backcopy-complex-from-real (lisp-type cffi-type)
	   `(locally
		(declare (type (simple-array (complex ,lisp-type)) array)
			 (optimize (speed 3) (safety 0)))
	      (loop
		with norm = (coerce-by-lisp-type (fftw3-plan-normalization plan)
						 ,lisp-type)
		with out-ptr = (fmem-ptr (fftw3-plan-out-fmem plan))
		for i of-type small-index below ats
		for x of-type ,lisp-type
		  =  (* norm
			(coerce-by-lisp-type (cffi:mem-aref out-ptr ,cffi-type i)
					     ,lisp-type))
		do
		   (setf (row-major-aref array i)
			 (complex x (coerce-by-lisp-type 0 ,lisp-type))))))
	 ;;
	 ;; slow copy to lisp generic array from cffi complex
	 (backcopy-generic-from-complex (cffi-type)
	   `(locally
		(declare (type array array)
			 (optimize (safety 3)))
	      (loop
		with norm = (float (fftw3-plan-normalization plan)
				   (if (eq ,cffi-type :float) 1.0 1d0))
		with out-ptr = (fmem-ptr (fftw3-plan-out-fmem plan))
		for i of-type small-index below ats
		for j of-type small-index = (* i 2)
		for x of-type  number = (row-major-aref array i)
		for xr of-type real
		  = (* norm (cffi:mem-aref out-ptr ,cffi-type j))
		for xi of-type real
		  = (* norm (cffi:mem-aref out-ptr ,cffi-type (1+ j)))
		do (setf (row-major-aref array i) (complex xr xi)))))
	 ;;
	 ;; slow copy to lisp generic array from cffi real
	 (backcopy-generic-from-real (cffi-type)
	   `(locally
		(declare (type array array)
			 (optimize (safety 3)))
	      (loop
		with norm = (fftw3-plan-normalization plan)
		with out-ptr = (fmem-ptr (fftw3-plan-out-fmem plan))
		for i of-type small-index below ats
		for x of-type  number
		  = (* norm (cffi:mem-aref out-ptr ,cffi-type i))
		do (setf (row-major-aref array i) x)))))
      ;;
      (typecase array
	((simple-array single-float)
	 (cond ((eq (fftw3-plan-input-type plan) :float)
		(backcopy-real-from-real single-float :float))
	       ((eq (fftw3-plan-input-type plan) :double)
		(backcopy-real-from-real single-float :double))
	       ((eq (fftw3-plan-input-type plan) :complex-float)
		(backcopy-real-from-complex single-float :float))
	       ((eq (fftw3-plan-input-type plan) :complex-double)
		(backcopy-real-from-complex single-float :double))))
	;;
	((simple-array (complex single-float))
	 (cond ((eq (fftw3-plan-input-type plan) :float)
		(backcopy-complex-from-real single-float :float))
	       ((eq (fftw3-plan-input-type plan) :double)
		(backcopy-complex-from-real single-float :double))
	       ((eq (fftw3-plan-input-type plan) :complex-float)
		(backcopy-complex-from-complex single-float :float))
	       ((eq (fftw3-plan-input-type plan) :complex-double)
		(backcopy-complex-from-complex single-float :double))))
	;;
	((simple-array double-float)
	 (cond ((eq (fftw3-plan-input-type plan) :float)
		(backcopy-real-from-real double-float :float))
	       ((eq (fftw3-plan-input-type plan) :double)
		(backcopy-real-from-real double-float :double))
	       ((eq (fftw3-plan-input-type plan) :complex-float)
		(backcopy-real-from-complex double-float :float))
	       ((eq (fftw3-plan-input-type plan) :complex-double)
		(backcopy-real-from-complex double-float :double))))
	;;
	((simple-array (complex double-float))
	 (cond ((eq (fftw3-plan-input-type plan) :float)
		(backcopy-complex-from-real double-float :float))
	       ((eq (fftw3-plan-input-type plan) :double)
		(backcopy-complex-from-real double-float :double))
	       ((eq (fftw3-plan-input-type plan) :complex-float)
		(backcopy-complex-from-complex double-float :float))
	       ((eq (fftw3-plan-input-type plan) :complex-double)
		(backcopy-complex-from-complex double-float :double))))
	;;
	(array
	 (cond ((eq (fftw3-plan-input-type plan) :float)
		(backcopy-generic-from-real  :float))
	       ((eq (fftw3-plan-input-type plan) :double)
		(backcopy-generic-from-real :double))
	       ((eq (fftw3-plan-input-type plan) :complex-float)
		(backcopy-generic-from-complex :float))
	       ((eq (fftw3-plan-input-type plan) :complex-double)
		(backcopy-generic-from-complex :double)))))
    plan)))
       


;; take an ndims sequence (vector or list), and
;; 1. check if correct type, and throw error if not
;; 1. convert to a fixnum vector
;; 2. check if the elements are valid, and if not throw an error
;; 3. check that total size is permitted
;; 4. return total number of elements and a vectorized version of ndims
(defun %validate-ndims (ndims)
  (declare (type sequence ndims))
  (when (or (not (plusp (length ndims)))
	    (not  (every (lambda (n)
		(and (typep n 'fixnum)
		     (not (minusp n))))
			 ndims)))
    (error "NDIMS=~A should be a valid set of dimensions for an array; ie, a sequence of positive fixnums." ndims))
  ;;
  (let ((ndimsvec (map '(simple-array fixnum (*)) #'identity ndims))
	(nelem 1))
    (loop for n across ndimsvec do (setf nelem (* nelem n)))
    (when (not (typep nelem 'fixnum))
      (error "The total number of elements ~A in NDIMS=~A is not a fixnum." nelem ndims))
    (values nelem ndimsvec)))
	  


(defun get-plan-type-code (plan-type)
  (declare (type (member :estimate :patient :exhaustive) plan-type))
  (cond ((eq plan-type :estimate)
	 +fftw-estimate+)
	((eq plan-type :patient)
	 +fftw-patient+)
	((eq plan-type :exhaustive)
	 +fftw-exhaustive+)))


(defun execute-fftw3-plan (array-in array-out plan)
  (declare (type array array-in array-out)
	   (type fftw3-plan plan))
  (copy-array-into-fftw3-plan array-in plan)
  (cond ((eq (fftw3-plan-float-type plan) :float)
	 (fftwf-execute (fftw3-plan-plan-ptr plan)))
	((eq (fftw3-plan-float-type plan) :double)
	 (fftwd-execute (fftw3-plan-plan-ptr plan))))
  (copy-array-from-fftw3-plan array-out plan)
  t)

(defun execute-fftw3-plan-pair (array-in array-out plan-pair direction)
  (declare (type array array-in array-out)
	   (type fftw3-plan-pair plan-pair)
	   (type (member :forward :backward) direction))
  (cond ((eq direction :forward)
	 (execute-fftw3-plan
	  array-in array-out (fftw3-plan-pair-plan-forward plan-pair)))	
	((eq direction :backward)
	 (execute-fftw3-plan
	  array-in array-out (fftw3-plan-pair-plan-backward plan-pair)))))
  


  
	   
