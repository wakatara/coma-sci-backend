
(in-package imutils)

(defun compute-mag+dmag-from-flux+dflux (flux dflux &key (gain 1)
						      (error-value nil))
  "Compute MAG and DMAG (error) from FLUX,DFLUX, using difference
between top and bottom bounds of flux converted to mag.

GAIN is used to multiply FLUX and DFLUX, and is 1 by default.

ERROR-VALUE is the result to return if either MAG or DMAG can't be
computed."
  (let* ((flux (* flux gain))
	 (dflux (* dflux gain))
	 (mag  (if (plusp flux)
		   (* -2.5 (log flux 10))))
	 (dmag (if (and (plusp flux) (plusp dflux))
		   (let* ((flux+ (+ flux dflux))
			  (flux- (- flux dflux))
			  (mag+ (* -2.5 (log flux+ 10)))
			  (mag- (if (plusp flux-) (* -2.5 (log flux- 10)))))
		     (if mag-
			 ;; if we can compute two sided errors, do it
			 (* 0.5 (- mag- mag+))
			 ;; else one sided
			 (- mag mag+))))))
    (values  (or mag error-value) (or dmag error-value))))
		       

