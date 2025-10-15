
;; WARNING - THIS IS FOR THE 7X7' single amp mode only!

(in-package imred)


;; reduction plan suitable for smarts
;;
(defclass reduction-plan-ctio-cfccd-on-ctio1.5m (reduction-plan)
  ((inst-id-type :initform 'instrument-id:ctio-cfccd-on-ctio1.5m)
   (trimsec :initform "TRIMSEC") ;; our new trimsec inserted by pre-proc function
   (zero-name :initform "BIAS")  
   (flat-name :initform "SKY FLAT")
   (flat-basename :initform "FLAT")
   (input-fits-patch-function :initform 
			      'ctio-cfccd-on-ctio1.5m-input-fits-patch-function)
   (output-fits-patch-function :initform
			       'ctio-cfccd-on-ctio1.5m-output-fits-patch-function)))



(defmethod get-reduction-plan-for-instrument ((inst instrument-id:ctio-cfccd-on-ctio1.5m))
  (declare (ignorable inst))
  'reduction-plan-ctio-cfccd-on-ctio1.5m)

;; smarts data are missing gain
(defun ctio-cfccd-on-ctio1.5m-input-fits-patch-function (fits-file reduction-plan)
  (declare (ignorable reduction-plan))
  (cf:with-open-fits-file (fits-file ff :mode :io)
  (flet ((getsec (sec) ;; get the [x1:x2,y1:y2] image section named sec
	   (cf:parse-image-section-string
	    (or (cf:read-fits-header ff sec)
		(error "Image Section Header ~A not found in FITS-FILE ~A" sec fits-file))))
	 (shift-im-left (im x2 x1) ;; shift an image left from x2 to x1, 1 indexed
	   (declare (type (array * (* *)) im)) ;; don't know type
	     (loop
	       with ny = (array-dimension im 0)
	       with nx = (array-dimension im 1)
	       for iy below ny
	       do (loop for ix2 from (1- x2) below nx
			for ix1 from (1- x1)
			do (setf (aref im iy ix1)
				 (aref im iy ix2)))))
	 (blanksec (im secarr) ;; blank anything not in secarr=[x1:x2,y1:y2]
	    (declare (type (array * (* *)) im)) ;; don't know type
	   (loop
	     with zero = (coerce 0 (type-of (aref im 0 0)))
	     with ny = (array-dimension im 0)
	     with nx = (array-dimension im 1)
	     for iy below ny
	     do (loop for ix below nx
		      when (not
			    (and
			     (<= (aref secarr 0)  (1+ ix) (aref secarr 1))
			     (<= (aref secarr 2)  (1+ iy) (aref secarr 3))))
			do (setf (aref im iy ix) zero)))))
		   
	   
    (let ((asec21 (getsec "ASEC21"))
	  (csec21 (getsec "CSEC21"))
	  (dsec21 (getsec "DSEC21"))
	  (tsec21 (getsec "TSEC21"))
	  (bsec21 (getsec "BSEC21"))
	  (asec22 (getsec "ASEC22"))
	  (csec22 (getsec "CSEC22"))
	  (dsec22 (getsec "DSEC22"))
	  (tsec22 (getsec "TSEC22"))
	  (bsec22 (getsec "BSEC22"))
	  (imsec (cf:read-image-section ff)))
      (declare (ignorable asec21 csec21 dsec21 tsec21 bsec21
			  asec22 csec22 dsec22 tsec22 bsec22))
      (let ((im (cf:image-section-data imsec))
	    (itrimsec ;; the new image section after we finishing shifting
	      (vector (+ 1    ;; 1 to get rid of first bad col
			 (aref tsec21 0))
		      (+ -1   ;; 2 to get rid last bad col
		       (aref tsec21 1)
			 (- (aref tsec22 1) (aref tsec22 0) -1))
		      (aref tsec21 2)
		      (aref tsec21 3))))
	;; shift the right amp left
	(shift-im-left im (aref dsec22 0) (1+ (aref dsec21 1)))
	(cf:write-fits-header ff "TRIMSEC"
			      (format nil "[~D:~D,~D:~D]"
				      (aref itrimsec 0) ;; 1+ to get rid of bad col
				      (aref itrimsec 1) ;; 1- to get rid of bad col
				      (aref itrimsec 2)
				      (aref itrimsec 3))
			      :comment "New Trimsec after shifting amp2 data by IMRED preproc")
	;; now blank out anything outside of itrimsec
	(blanksec im itrimsec)	      
	(cf:write-back-image-section imsec))))))
				      
	
;; does nothing for now
(defun ctio-cfccd-on-ctio1.5m-output-fits-patch-function (fits-file reduction-plan)
  (declare (ignorable fits-file reduction-plan))
  t)
	  
  


  
