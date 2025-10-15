
#|

a function to add a simulated object to fits files using the SAAPLAN-IMAGE-PREPROC

To use, create a SAAPLAN-IMAGE-PREPROC-FUNCTION from the comet-elem orbit
using MAKE-SIMULATED-OBJECT-IMAGE-PREPROC

|#

(in-package shift-and-add)

(defclass simulated-object-preproc (image-preproc)
  ((fwhm :initarg :fwhm :initform 5.0
	 :accessor simulated-object-preproc-fwhm)
   (flux :initarg :flux :initform 1000.0
	 :accessor simulated-object-preproc-flux)
   (observatory :initarg :observatory
		:initform "MKO" :accessor simulated-object-preproc-observatory)
   (comet-elem :initarg :comet-elem
	       :initform nil
	       :accessor simulated-object-preproc-comet-elem)))

(defmethod run-image-preproc ((image-preproc simulated-object-preproc)
			      (saaplan saaplan)
			      fits-list)
  (let ((fwhm (simulated-object-preproc-fwhm image-preproc))	
	(flux (simulated-object-preproc-flux image-preproc))
	(observatory (simulated-object-preproc-observatory image-preproc))
	(comet-elem (simulated-object-preproc-comet-elem image-preproc)))
    (when (not comet-elem)
      (error "No slalib-ephem:comet-elem in simulated-object-preproc-comet-elem in ~A"
	     image-preproc))
    
    (loop for fits in fits-list
	  do (cf:with-open-fits-file (fits ff :mode :io)
	       (let* ((imsec (cf:read-image-section ff))
		      (headfile (concatenate 'string (file-io:file-basename fits)
					     ".head"))
		      (wcs (or (ignore-errors
				(car (terapix::parse-scamp-wcs-from-headfile headfile)))
			       (error "No WCS found for ~A fits" fits)))
		      (mjd (or (cf:read-fits-header ff (saaplan-mjd-keyword saaplan))
			       (error "MJD header ~A not found in ~A"
				      (saaplan-mjd-keyword saaplan) fits)))
		      (sigma (float (imutils:gaussian-sigma-for-fwhm fwhm) 1.0))
		      (data (cf:image-section-data imsec))
		      (ephem (slalib-ephem:compute-ephem-for-observatory   
			      comet-elem mjd observatory))
		      (ra (slalib-ephem:ephem-ra ephem))
		      (dec (slalib-ephem:ephem-dec ephem)))
		 (multiple-value-bind (x y)
		     (wcs:wcs-convert-ra-dec-to-pix-xy wcs ra dec)
		   (cf:write-fits-header ff "XSIMUL" x :comment "X pixel of simulated object")
		   (cf:write-fits-header ff "YSIMUL" y :comment "Y pixel of simulated object")
		   (imutils:add-gaussian-to-image 
		    data (float x 1.0) (float y 1.0) sigma sigma 0.0 (float flux 1.0)))
		 (cf:write-back-image-section imsec))))))  
  
