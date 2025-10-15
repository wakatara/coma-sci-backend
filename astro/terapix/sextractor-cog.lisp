
#|

compute and use aperture curve of growth

The function

  (COMPUTE-APERTURE-CURVE-OF-GROWTH-FOR-FITS-FILE FITS-FILE 
    :PHOT-APERTURES ...)

computes the curve of growth structure, and writes it to a tabular
file in the fits directory.   The pixel scale is taken from the WCS, which
must be present.

The curve of growth can be read back in using 
  (READ-SEXTRACTOR-CURVE-OF-GROWTH-FOR-FITS-FILE FITS-FILE)

The function

  (EVALUATE-CURVE-OF-GROWTH SCOG APERTURE :UNITS :ARCSEC) 

then computes the magnitude offset for an arbitrary APERTURE.


|#


(in-package terapix)


;; the fundamental structure for a curve of growth
(defstruct curve-of-growth
  (nstars nil) ;; number of stars in largest ap
  (aperture-vec/pix nil)
  (aperture-vec/arcsec nil)
  (dmag-vec nil)
  (dmag-err-vec nil)
  (dmag-spline/pix nil)
  (dmag-spline/arcsec nil))

;; compute spline functions for a curve of growth
(defun %compute-splines-for-curve-of-growth (scog)
  (setf (curve-of-growth-dmag-spline/pix scog)
	(cubic-spline:make-splint-func-from-xy 
	 (curve-of-growth-aperture-vec/pix scog)
	 (curve-of-growth-dmag-vec scog)))
  (setf (curve-of-growth-dmag-spline/arcsec scog)
	(cubic-spline:make-splint-func-from-xy
	 (curve-of-growth-aperture-vec/arcsec scog)
	 (curve-of-growth-dmag-vec scog)))
  scog)


;; turn a curve of growth into a tabular file string
(defun %curve-of-growth-string (scog)
  (with-output-to-string (sout)
    (format sout "# Curve of growth file, nstars=~D~%" (curve-of-growth-nstars scog))
    (format sout "#  ap/pix       ap/arcsec     dmag     dmag-err~%")
    (loop 
      for ap/pix across (curve-of-growth-aperture-vec/pix scog)
      for ap/arcsec across (curve-of-growth-aperture-vec/arcsec scog)
      for dmag across (curve-of-growth-dmag-vec scog)
      for dmag-err across (curve-of-growth-dmag-err-vec scog)
      do (format sout "~10,4F   ~10,4F   ~10,6F  ~10,6F~%" ap/pix ap/arcsec dmag dmag-err))))

(defun write-curve-of-growth (scog scog-outfile)
  "Write the curve of growth to a raw tabular file."
  (with-open-file (sout scog-outfile :direction :output 
				     :if-exists :supersede
				     :if-does-not-exist :create)
    (write-string (%curve-of-growth-string scog) sout)))


(defgeneric write-curve-of-growth-to-fits-file (scog fits-file)
  (:documentation "Write the curve of growth in tabular form to a fits file
as a multiline fits header."))

(defmethod write-curve-of-growth-to-fits-file ((scog curve-of-growth) (ff cf:fits-file))
  (cf:write-multiline-header ff "COG" (%curve-of-growth-string scog) 
			     :comment "Curve of growth in column form"))

(defmethod write-curve-of-growth-to-fits-file ((scog curve-of-growth) (ff string))
  (cf:with-open-fits-file (ff fits-file :mode :io :throw-error t)
    (write-curve-of-growth-to-fits-file scog fits-file)))
  



(defun %read-curve-of-growth-from-stream (s)
  (let* ((first-line (or (read-line s nil nil) (error "no first line to read")))
	 (pos-nstars (or (search "nstars=" first-line)
			 (error "cannot find string 'nstars='")))
	 (nstars (or (ignore-errors (parse-integer first-line :start (+ 7 pos-nstars)))
		     (error "Cannot parse nstars=xx"))))
    (let* ((v (numio:read-cols s '(:single-float :single-float :single-float :single-float)
			       :comment-char #\#))
	   (scog
	     (make-curve-of-growth 
	      :nstars nstars
	      :aperture-vec/pix (aref v 0)
	      :aperture-vec/arcsec (aref v 1)
	      :dmag-vec (aref v 2)
	      :dmag-err-vec (aref v 3))))
      (%compute-splines-for-curve-of-growth scog))))


(defun read-curve-of-growth (scog-file)
  "Read a sextractor curve of growth from a raw tabular file."
  (with-open-file (s scog-file :direction :input :if-does-not-exist :error)
    (%read-curve-of-growth-from-stream s)))


(defgeneric read-curve-of-growth-from-fits-file (fits-file)
  (:documentation "Read a curve of growth in multiline form from a
  fits file as a multiline fits header."))

(defmethod read-curve-of-growth-from-fits-file ((ff cf:fits-file))
  (when (not (cf:read-fits-header ff "COG.PART1"))
    (error "No curve of growth in fits file" ))
  (let ((cog-text (cf:read-multiline-header ff "COG")))
    (when cog-text
      (with-input-from-string (sin cog-text)
	(%read-curve-of-growth-from-stream sin)))))

(defmethod read-curve-of-growth-from-fits-file ((ff string))
  (cf:with-open-fits-file (ff fits-file :mode :input :throw-error t)
    (read-curve-of-growth-from-fits-file fits-file)))
	
	 
(defun read-curve-of-growth-for-fits-file 
  (fits-file   &key    (filename "curve-of-growth.dat"))
  "Read the sextractor curve of growth for FITS-FILE."
  (read-curve-of-growth 
   (format nil "~A/~A" (get-fits-directory fits-file) filename)))



(defun write-curve-of-growth-for-fits-file 
  (scog fits-file   &key    (filename "curve-of-growth.dat"))
  "Write the sextractor curve of growth for FITS-FILE into file NAME in
the fits directory."
  (declare (type curve-of-growth scog))
  (write-curve-of-growth scog
   (format nil  "~A/~A" (get-fits-directory fits-file) filename)))


(defun evaluate-curve-of-growth (scog aperture &key (units :pixel))
  "Evalulate the DMAG of SCOG object of type CURVE-OF-GROWTH 
at aperture size APERTURE for :UNITS equal to :ARCSEC or :PIXEL"
  (declare (type curve-of-growth scog)
	   (type real aperture)
	   (type (member :arcsec :pixel) units))
  (let ((n (1- (length (curve-of-growth-dmag-vec scog)))))
    (cond ((eq units :pixel)
	   (cond
	     ((< aperture 
		 (aref (curve-of-growth-aperture-vec/pix scog) 0))
	      (error "Aperture too small in EVALUATE-CURVE-OF-GROWTH."))
	     ((> aperture
		 (aref (curve-of-growth-aperture-vec/pix scog)n))
	      0.0)
	     (t
	      (float
	       (funcall 
		(curve-of-growth-dmag-spline/pix scog) aperture) 
	       1.0))))
	   ((eq units :arcsec)
	    (cond
	      ((< aperture 
		  (aref (curve-of-growth-aperture-vec/arcsec scog) 0))
	       (error "Aperture too small in EVALUATE-CURVE-OF-GROWTH."))
	      ((> aperture 
		  (aref (curve-of-growth-aperture-vec/arcsec scog) n))
	       0.0)
	      (t
	       (float
		(funcall 
		 (curve-of-growth-dmag-spline/arcsec scog) aperture) 
		1.0))))
	   (t (error "Units not :pixel or :arcsec")))))
	 
  

(defun correct-aperture-mag-using-curve-of-growth (scog mag aperture &key (units :pixel))
  "Correct a magnitude MAG taken with APERTURE using the CURVE-OF-GROWTH object SCOG, with
:UNITS equal to :ARCSEC or :PIXEL."
  (declare (type curve-of-growth scog)
	   (type real aperture mag)
	   (type (member :arcsec :pixel) units))
  (+ mag (evaluate-curve-of-growth scog aperture :units units)))
  



(defparameter *curve-of-growth-apertures*
  '(5 10 20 25 30 35 40 45 50 55 60 65 70))

(defun compute-aperture-curve-of-growth-for-fits-file
    (fits-file 
     &key 
       (phot-apertures *curve-of-growth-apertures*)
       (display-errors nil)
       (min-flux 5000)
       (satur-level 20000)
       (dmag 5.0)
       (output-table-file "curve-of-growth.dat")
       (write-cog-to-catalog-headers t)
       (write-cog-to-image-headers nil)
       (md5-avoid-rerun t)
       (deblend-mincont 1.0)
       (catalog-name "sex_curve_of_growth.cat"))
  "Run sextractor on FITS-FILE for PHOT-APERTURES (in pixels), taking
stars brighter than MIN-FLUX with a zero flag, and return a list of
additive constants to add to each aperture size to get the true
magnitude, assuming the outermost aperture contains 100% of the flux.

DMAG is the difference in magnitude for which a contaminating object
is ignored inside the largest aperture.  DMAG=5 (default) means that
any object 100x fainter than this one is not culled.

In practice, removing objects with close neighboring objects seems to have 
little effect on the curve of growth.

Returns ((APERTURE-1 MAG-CORRECT-1 ERR-MAG-CORRECT-1 NOBJ1)
         (APERTURE-2 MAG-CORRECT-2 ERR-MAG-CORRECT-2 NOBJ2)
         ...)

where APERTURE-i is the size of the aperture, MAG-CORRECT-i is the
amount to add to the magnitude in this aperture to get the
true (largest aperture) magnitude, ERR-MAG-CORRECT-i is the boostrap
error on MAG-CORRECT-i, and NOBJi is the number of objects that went
into the calculation (same for all points).

If OUTPUT-TABLE-FILE is set, write curve-of-growth.dat in the
fits directory to this file (default: curve-of-growth.dat).

If WRITE-COG-TO-CATALOG-HEADERS is set (default T) then write the 
curve of growth as an multiline fits header, so it can be read by
 (READ-CURVE-OF-GROWTH-FROM-FITS-FILE CATALOG-FILE).

DEBLEND-MINCONT (sextractor parameter) is set to 1, to avoid
deblending objects at all.

If WRITE-COG-TO-IMAGE-HEADERS is set (default NIL), then write the
curve of growth to the fits image.
"
 
  (setf phot-apertures (sort phot-apertures  '< )) ;; just in case
  ;;
  (run-sextractor fits-file 
		  :display-errors display-errors
		  :output-catalog catalog-name 
		  :deblend-mincont deblend-mincont
		  :satur-level satur-level
		  :md5-avoid-rerun md5-avoid-rerun
		  :phot-apertures phot-apertures)
  (let* ((full-catalog-name 
	   (format nil "~A/~A" (get-fits-directory fits-file) catalog-name))
	 (h (read-sextractor-catalog full-catalog-name))
	 (pixel-scale (wcs:get-pixel-scale-for-wcs (cf:read-wcs fits-file)))
	 ;; the size of the largest aperture in arcseconds
	 (max-aper-arcsec (* (stats:max-of-elements phot-apertures)
			      pixel-scale))
	 (mags-aper (gethash "MAG_APER" h))
	 (flux-auto (gethash "FLUX_AUTO" h))
	 (flags (gethash "FLAGS" h))
	 (has-neighbor nil)
	 (n (length flags))
	 (nap (length phot-apertures))
	 (nstars nil)
	 (outlist nil))
    ;; add vectors of neighboring objects to hash, allowing objects more than 5 mags 
    ;; dimmer to be ignored
    (add-neighbor-dist-to-sextractor-catalog h :match-dist max-aper-arcsec 
					       :dmag dmag)
    (setf has-neighbor (gethash "%NEIGHBOR-INDEX" h))
    (loop for ap in  phot-apertures
	  for i-ap from 0
	  do
	     (loop for i below n
		   for flux = (aref flux-auto i)
		   for mag1 = (aref mags-aper i (1- nap))
		   for mag2 = (aref mags-aper i i-ap)
		   for dmag = (- mag1 mag2)
		   for flag = (aref flags i)
		   when (and (zerop flag) (> flux min-flux)
			     (not (aref has-neighbor i)))
		   collect (float dmag 1d0) into dmag-list
		   finally
		      ;; the number of stars is always the number in the largest 
		      ;; aperture so we pick it off once
		      (when (not nstars) (setf nstars (length dmag-list)))
		      (when (< nstars 1) ;; can't take median
			(return))
		      (multiple-value-bind (med-dev med-dev-err)
			  (bootstrap:resample-one-sided-median-dev/double-float
			   (coerce dmag-list '(simple-array double-float (*))) 
			   n :frac 0.68)
			;;
			(push (list ap med-dev med-dev-err (length dmag-list))
			      outlist))))

    (setf outlist (reverse outlist)) ;; small aps first
    
    (let ((scog (make-curve-of-growth 
		 :nstars nstars
		 :aperture-vec/pix 
		 (map '(simple-array single-float (*))
		      (lambda (pair) (float (first pair) 1.0))
		      outlist)
		 :aperture-vec/arcsec
		 (map '(simple-array single-float (*))
		      (lambda (pair) (float (* pixel-scale (first pair)) 1.0))
		      outlist)
		 :dmag-vec
		 (map '(simple-array single-float (*))
		      (lambda (pair) (float (second pair) 1.0))
		      outlist)
		 :dmag-err-vec
		  (map '(simple-array single-float (*))
		      (lambda (pair) (float (third pair) 1.0))
		      outlist))))
      (when (plusp nstars) ;; answer is technically valid for nstars>1
	(%compute-splines-for-curve-of-growth scog)
	(when output-table-file
	  (write-curve-of-growth-for-fits-file scog fits-file :filename output-table-file))
	(when write-cog-to-catalog-headers
	  (write-curve-of-growth-to-fits-file scog full-catalog-name))
	(when write-cog-to-image-headers
	  (write-curve-of-growth-to-fits-file scog fits-file)))
      scog)))
		  
	     
