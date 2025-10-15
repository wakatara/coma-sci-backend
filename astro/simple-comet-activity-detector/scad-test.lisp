

(defpackage simple-comet-activity-detector/test
  (:nicknames scad/test)
  (:use #:cl #:scad)
  (:export
   #:plot-activity-from-csv
   #:test-is-object-active
   #:estimate-fwhm
   #:generate-comet-coma-image
   #:estimate-fwhm
   ))


(in-package scad/test)



(defun plot-activity-from-csv (csv-file &key (device :x11) (filename nil) (fits-dir "."))
  "Given a CSV-FILE with fields FITS,STATUS,XPIX,YPIX  compute the activity and plot."
  (let* ((dhash (csv-read:read-csv-headers/columns-from-file csv-file :hash-test 'equalp))
	 (fits-file-vec (gethash "FITS" dhash))
	 (activity-vec (gethash "STATUS" dhash))
	 (xpix-vec (gethash "XPIX" dhash))
	 (ypix-vec (gethash "YPIX" dhash))
	 (nobj (length xpix-vec)))

    (let ((p (pgplot:open-device device :filename filename)))
      (pgplot:set-window p 0 (1+ nobj)  -0.1 1.2)
      (pgplot:box p)
      (pgplot:xlabel p "Object number")
      (pgplot:ylabel p "Coma fraction")

      (loop for fits-file across fits-file-vec
	    for full-fits-file = (concatenate 'string fits-dir  "/" fits-file)
	    for status across activity-vec
	    for xpix across xpix-vec
	    for ypix across ypix-vec
	    for active-p = (equalp status "A")
	    for color = (if active-p :red :default)
	    for im = (cfitsio:image-section-data (cf:read-image-section full-fits-file))
	    for scadresult = (is-object-active im (float xpix 1.0) (float  ypix 1.0))
	    for fratio = (scadresult-fratio scadresult)
	    for coma-frac = (scadresult-coma-frac scadresult)
	    for coma-frac-err = (scadresult-coma-frac-err scadresult)
	    for i from 1
	    do
	       #+nil
	       (format t "~A ~A  FRATIO: ~,4F  COMA-FRAC: ~,3F  active=~A~%" fits-file status fratio coma-frac
		       (scadresult-active-p scadresult))
	       (pgplot:points p i coma-frac :filled-circle :color color)
	       (pgplot:errorbars p (vector i) (vector coma-frac) (vector coma-frac-err) :color color))
      ;;
      (when filename (pgplot:close-device p)))))
      


(defun generate-comet-coma-image (&key (n 501)  (fwhm-total 10.0) (flux-nuc 1.0) (fcoma 0.0))
  (multiple-value-bind (fwhm-mof fwhm-coma)
      (scad::fwhm-of-moffat-and-coma-from-fcoma  fcoma fwhm-total)
    (let* ((im (imutils:make-image n n))
	   (n/2 (ash n -1))
	   (rmax (* 8.0 fwhm-mof)) ;; how far to extend profile
	   (coma-rmax 10.0) ;; should not matter - just for normalization
	   (beta scad::+moffat-beta+)
	   (a (imutils:moffat-a-for-fwhm fwhm-mof beta))
	   ;; value of mof at central point, for norm=flux-nuc
	   (mof0 (imutils:moffat 0.0 a beta :norm flux-nuc))
	   ;; value of coma at central point, for norm=1 
	   (coma0 (imutils:1/r-coma-func 0.0 fwhm-coma coma-rmax :norm 1.0))
	   ;; rescaling factor of coma, so that coma(0)/(coma(0)+mof(0)) = fcoma
	   (cscale (/ (* fcoma mof0) (- coma0 (* fcoma coma0)))))

      ;;(format t "FWHM-MOF=~A FWHM-COMA=~A~%"  fwhm-mof fwhm-coma)
      ;;(format t "CSCALE=~A~%" cscale)
      (let ((fn0 (imutils:moffat 0.0 a beta :norm flux-nuc))
	    (fc0 (* cscale (imutils:1/r-coma-func 0.0 fwhm-coma coma-rmax :norm 1.0))))
	;;(format t "FCOMA_OUT=~A~%" (/ fc0 (+ fc0 fn0))))
      
      (imutils:add-radial-function-to-image im (float n/2 1.0) (float n/2 1.0)
					    (lambda (r)
					      (imutils:moffat r a beta :norm flux-nuc))
					    :dist rmax)
      (imutils:add-radial-function-to-image im (float n/2 1.0) (float n/2 1.0)
					    (lambda (r)
					      (* cscale (imutils:1/r-coma-func  r fwhm-coma coma-rmax :norm 1.0)))
					    :dist rmax)
	(values im
		;; return the fcoma output for checking
		(/ fc0 (+ fc0 fn0)))

	))))


;; verify the FWHM of a 2d function centered in an image
(defun estimate-fwhm (im &key (dr 0.2))
  (let ((nx0  (ash (array-dimension im 1) -1))
	(ny0  (ash (array-dimension im 0) -1)))
    (multiple-value-bind (fvec rvec)
	(imutils:compute-radial-profile im nx0 ny0 :dr dr)
      (let* ((fwhm (scad::get-fwhm-from-fvec-rvec fvec rvec)) ;; linear interp
	     (finside 0.0)
	     (foutside 0.0)
	     (hwhm (* 0.5 fwhm)))
	(loop for r across rvec
	      for f across fvec
	      when (< r (* 2 hwhm)) ;; hwhm
		do (incf finside (* 2 pi r f))
	      when (<= (* 3 hwhm) r (* 4 hwhm))
		do (incf foutside (* 2 pi r f)))
	(values fwhm (/ foutside finside))))))
	

(defun test-is-object-active (&key (n 501) (fwhm-total 10.0) (flux-nuc 1000.0) (fcoma 0.10) (r-profile 50))
  (let ((im (generate-comet-coma-image :n n :fwhm-total fwhm-total
				       :flux-nuc flux-nuc :fcoma fcoma))
	(z (* 1.0 (ash n -1))))
    (scad:is-object-active im z z :r-profile r-profile)))
