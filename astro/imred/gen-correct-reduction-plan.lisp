

;; generate the correct reduction plan for a fits file
(in-package imred)




(defun make-reduction-plan-for-instrument
    (instrument
     &key
       (make-zero t)
       (make-flats t)
       (ccdproc-objects t)
       (fringecorrect t)
       (delete-intermediate-files t)
       (logger nil)
       (target-dir "./"))

  (declare (type instrument-id:instrument instrument))
  (let ((plan-type (get-reduction-plan-for-instrument instrument)))
    (when plan-type
      (make-instance plan-type
		     :make-zero make-zero
		     :make-flats make-flats
		     :ccdproc-objects ccdproc-objects
		     :make-fringes fringecorrect
		     :logger logger
		     :delete-intermediate-files delete-intermediate-files
		     :target-dir target-dir))))

						  
