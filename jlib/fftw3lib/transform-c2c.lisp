
(in-package fftw3lib)

(defun %compute-standard-normalization (ndimsvec)
  (declare (type (simple-array fixnum (*)) ndimsvec))
  (loop with norm = 1d0
	for j across ndimsvec
	do (setf norm (* norm j))
	   finally (return (sqrt (/ 1d0 norm)))))


(defun %build-fftw3-plan-pair-c2c (ndims float-type
				   &key (plan-type :estimate)
				     (normalize t))
  (declare (type vector ndims)
	   (type (member :estimate :patient :exhaustive) plan-type)
	   (type (member :float :double) float-type))
  (multiple-value-bind (nelem ndimsvec)
      (%validate-ndims ndims)
    ;;
    (let* ((normalization (if normalize
			      (%compute-standard-normalization ndimsvec)
			      1d0))
	   (complex-type (if (eq float-type :float)
			     :complex-float
			     :complex-double))			  
	   (plan-forward
	     (allocate-fftw3-plan
	      ndimsvec
	      float-type
	      complex-type complex-type
	      nelem nelem :in-equals-out t
	      :normalization normalization))
	   (plan-backward
	     (allocate-fftw3-plan
	      ndimsvec
	      float-type
	      complex-type complex-type
	      nelem nelem :in-equals-out t
	      :normalization normalization
	      ;; reuse existing foreign arrays in plan-forward
	      :existing-in-fmem (fftw3-plan-in-fmem plan-forward)
	      :existing-out-fmem (fftw3-plan-in-fmem plan-forward)))
	   (plan-pair
	     (make-fftw3-plan-pair
	      :plan-forward plan-forward
	      :plan-backward plan-backward))
	   (plan-code (get-plan-type-code plan-type)))
      ;;
      ;;
      (if (eq float-type :float)
	  (progn
	    (setf (fftw3-plan-plan-ptr plan-forward)
		  (fftwf-plan-dft
		   (fftw3-plan-rank plan-forward)
		   (fftw3-plan-ndim-ptr plan-forward)
		   (fmem-ptr (fftw3-plan-in-fmem plan-forward))
		   (fmem-ptr (fftw3-plan-out-fmem plan-forward))
		   (setf (fftw3-plan-direction plan-forward) +fftw-forward+)
		   plan-code))
	    ;;
	    (setf (fftw3-plan-plan-ptr plan-backward)
		  (fftwf-plan-dft
		   (fftw3-plan-rank plan-backward)
		   (fftw3-plan-ndim-ptr plan-backward)
		   (fmem-ptr (fftw3-plan-in-fmem plan-backward))
		   (fmem-ptr (fftw3-plan-out-fmem plan-backward))
		   (setf (fftw3-plan-direction plan-backward) +fftw-backward+)
		   plan-code)))

	   (progn
	    (setf (fftw3-plan-plan-ptr plan-forward)
		  (fftwd-plan-dft
		   (fftw3-plan-rank plan-forward)
		   (fftw3-plan-ndim-ptr plan-forward)
		   (fmem-ptr (fftw3-plan-in-fmem plan-forward))
		   (fmem-ptr (fftw3-plan-out-fmem plan-forward))
		   (setf (fftw3-plan-direction plan-forward) +fftw-forward+)
		   plan-code))
	    ;;
	    (setf (fftw3-plan-plan-ptr plan-backward)
		  (fftwd-plan-dft
		   (fftw3-plan-rank plan-backward)
		   (fftw3-plan-ndim-ptr plan-backward)
		   (fmem-ptr (fftw3-plan-in-fmem plan-backward))
		   (fmem-ptr (fftw3-plan-out-fmem plan-backward))
		   (setf (fftw3-plan-direction plan-backward) +fftw-backward+)
		   plan-code))))
      ;;
      plan-pair)))	     
	      

(defun build-fftw3-plan-pair-float-c2c (ndims  &key (plan-type :estimate)
						  (normalize t))
  "Construct a FFTW3-PLAN-PAIR of NDIMS dimensions that works on
arrays of complex single floats.  Transform is executed using
FFTW3LIB:EXECUTE-FFTW3-PLAN-PAIR."
  (declare (type vector ndims)
	   (type (member :estimate :patient :exhaustive) plan-type))
  (%build-fftw3-plan-pair-c2c ndims :float :plan-type plan-type
					   :normalize normalize))


(defun build-fftw3-plan-pair-double-c2c (ndims  &key (plan-type :estimate)
						  (normalize t))
    "Construct a FFTW3-PLAN-PAIR of NDIMS dimensions that works on
arrays of complex double floats.  Transform is executed using
FFTW3LIB:EXECUTE-FFTW3-PLAN-PAIR."
  (declare (type vector ndims)
	   (type (member :estimate :patient :exhaustive) plan-type))
  (%build-fftw3-plan-pair-c2c ndims :double :plan-type plan-type
					    :normalize normalize))
	        

