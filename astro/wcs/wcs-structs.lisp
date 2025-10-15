
(in-package wcs)

(defstruct wcs
  )

(defstruct (wcs-2d (:include wcs));;2D wcs always has these things
  (crval1 0d0 :type double-float) 
  (crval2 0d0 :type double-float) 
  (crpix1 0d0 :type double-float) 
  (crpix2 0d0 :type double-float) 
  ;;
  (cd1_1  0d0 :type double-float)
  (cd1_2  0d0 :type double-float)
  (cd2_1  0d0 :type double-float)
  (cd2_2  0d0 :type double-float)
  
  (equinox 0d0 :type double-float)
  )

;; a useful helper function
(defun get-pixel-scale-for-wcs (wcs)
  "Return the pixel scale in arcseconds for WCS-2D object using
the determinant of the CD matrix."
  (declare (type wcs-2d wcs))
  (* 3600d0
     (sqrt 
      (abs
       (- (* (wcs-2d-cd1_1 wcs) (wcs-2d-cd2_2 wcs))
	  (* (wcs-2d-cd1_2 wcs) (wcs-2d-cd2_1 wcs)))))))

  
 
; see Calabretta & Greisen A&A 395 1077 p 1090
(defstruct (wcs-radec-tan (:include wcs-2d))
  ) 

; see Calabretta & Greisen A&A 395 1077 p 1090
(defstruct (wcs-radec-sin (:include wcs-2d))
  )


;; TPV as in https://fits.gsfc.nasa.gov/registry/tpvwcs/tpv.html
;; NASA calls this RA---TPV but Terapix cals it RA---TAN and adds PV terms
;; again, we don't do any math
(defstruct (wcs-radec-tan-tpv (:include wcs-radec-tan))
  (pv1vec (make-array 0 :element-type 'double-float)
   :type (simple-array double-float (*)))
  (pv2vec (make-array 0 :element-type 'double-float)
   :type (simple-array double-float (*))))


;; radec-tan adjusted using A,B and reverse AP,BP matrixes
;; eg in https://fits.gsfc.nasa.gov/registry/sip/shupeADASS.pdf
(defstruct sip-cor
  (order 0 :type (integer 0 100))
  (matrix nil :type (or null
			(simple-array double-float (* *))
			;; for now, allow this for read/write-ability
			(simple-array T (* *)))))


(defstruct (wcs-radec-tan-sip (:include wcs-radec-tan))
  ;; allow corrections to be NULL because they might not exist
  (a nil  :type (or null sip-cor))
  (b nil  :type (or null sip-cor))
  (ap nil :type (or null sip-cor))
  (bp nil :type (or null sip-cor)))

;; FIXME - we can't do any math with the SIPs yet

;; zenithal polynomial - essentially, SIN is first order
;; and higher order polynomial terms are corrections to SIN
;; see Calabretta & Greisen A&A 395 1077 p 1090
(defstruct (wcs-radec-zpn (:include wcs-2d))
  (np 1 :type (unsigned-byte 20)) ;; number of polynomial terms,
  ;; 1=r^0,etc the poly terms - p[0]=r^1, p[1]=r^2, etc generally p[1]
  ;; should be 1.0, and only odd terms are used (why? because r/r' is
  ;; an even function?)
  (pvec  (make-array 2 :element-type 'double-float
		     :initial-contents '(0d0 1d0) )
	 :type (simple-array double-float (*)))
  )

 

(defstruct (wcs-1d (:include wcs))
  (crval1 0d0 :type double-float)
  (crpix1 0d0 :type double-float)
  )

(defstruct (wcs-linear (:include wcs-1d))
  (cdelt1 0d0 :type double-float)
  ) 

