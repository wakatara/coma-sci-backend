

(in-package fftw3lib)
 
(macrolet 
    ((define-fftw-c-1d (func-name lisp-float-type cffi-type
				  plan-func execute-func
				  malloc-func free-func
				  destroy-plan-func)
       `(defun ,func-name (in &key (direction :forward) (normalize t))
	  "Perform an FFT transform in one dimension in the DIRECTION
given, creating a new output array."
	  (declare (type (simple-array (complex ,lisp-float-type) (*)) in)
		   (type (member :forward :backward) direction)
		   #.*standard-optimize-settings*)

	  (let* ((count (length in))
		 (out (make-array
		       count
		       :element-type (quote (complex ,lisp-float-type))))
		 in-cf out-cf plan)
	    (declare (type mini-fixnum count))
	    (unwind-protect ;; to free all foreign objects
		 (progn
		   (setf in-cf
			 (,malloc-func
			  (* 4 count ,(cffi:foreign-type-size cffi-type))))
		   (setf out-cf ;; why can't we (setf out-cf in-cf)? (crashes) 
			 (,malloc-func
			  (* 4 count ,(cffi:foreign-type-size cffi-type))))
		   
		   ;; load the complex data into the input vector
		   (loop with opos of-type mini-fixnum = 0
			 for c of-type (complex ,lisp-float-type) across in
			 do (setf (cffi:mem-aref in-cf ,cffi-type opos)
				  (realpart c))
			    (incf opos)
			    (setf (cffi:mem-aref in-cf ,cffi-type opos)
				  (imagpart c))
			    (incf opos))
		   ;;
		   (setf plan
			 (,plan-func count in-cf out-cf
				      (if (eq direction :forward)
					  +fftw-forward+ +fftw-backward+)
				      (logior +fftw-estimate+
					      +fftw-destroy-input+)))
		   (,execute-func plan)
		   ;;
		   (let ((factor (if normalize
				     (sqrt (/ ,(coerce 1 lisp-float-type) count))
				     ,(coerce 1 lisp-float-type))))
		     (declare (type ,lisp-float-type factor))
		     (loop
		       for i  of-type mini-fixnum below count
		       for j  of-type mini-fixnum = (* 2 i)
		       for jj of-type mini-fixnum = (1+ j)
		       do (setf
			   (aref out i)
			   (complex
			    (* factor (cffi:mem-aref out-cf ,cffi-type j))
			    (* factor (cffi:mem-aref out-cf ,cffi-type jj)))))))
	      (progn ;; unwind-protected form
		(when plan   (,destroy-plan-func plan))
		(when in-cf  (,free-func in-cf))
		(when out-cf (,free-func out-cf))))
	      out))))
  ;;  
  (define-fftw-c-1d FFTWD-C-1D double-float :double fftwd-plan-dft-1d
    fftwd-execute fftwd-malloc fftwd-free fftwd-destroy-plan)
  ;;
  (define-fftw-c-1d FFTWF-C-1D single-float :float fftwf-plan-dft-1d
    fftwf-execute fftwf-malloc fftwf-free fftwf-destroy-plan))
				   


