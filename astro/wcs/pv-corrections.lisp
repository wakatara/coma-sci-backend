(in-package wcs)

#|
 Given a PV or TPV style PV correction, compute new xi,eta (Calabretta's psi,phi)
 coeff-vec is a vector of 3 values: power of XI, power of ETA, power of R=sqrt(ZI,ETA)
 eg  #(1 2 3) at jth index means to add PV[j]*xi*eta^2*r^3 to the result

NOTE: this isn't much use because we can convert pix to RA,Dec but 
the inverse transform is a numerical PITA

|#
(defun do-pv-style-correction (coeff-vec xi eta pvec1 pvec2 &key (use-r nil))
  (declare (type double-float xi eta)
	   (type (simple-array double-float (*)) pvec1 pvec2)
	   (type (simple-array (integer 0 7) (* 3)) coeff-vec))
  (let ((r (if use-r (sqrt (the (double-float 0d0) (+ (* xi xi) (* eta eta))))
	       0d0)))
    (flet ((apply-coeff-vec (pvec a b) ;; a,b are xi,eta, or eta,xi
	     (loop
	       for i from 0 below (length pvec)
	       for apow of-type (integer 0 7) = (aref coeff-vec i 0)
	       for bpow of-type (integer 0 7) = (aref coeff-vec i 1)
	       for rpow of-type (integer 0 7) = (aref coeff-vec i 2)
	       for aa of-type double-float = (expt a apow)
	       for bb of-type double-float = (expt b bpow)
	       for rr of-type double-float = (if use-r (expt r rpow) 1d0)
	       sum  (* (aref pvec i) aa bb rr))))
      (values
       (apply-coeff-vec pvec1 xi eta)     ;; new xi, a=xi,b=eta
       (apply-coeff-vec pvec2 eta xi))))) ;; new eta, a=eta,b=xi

(defparameter *tpv-correction-coeff* ;; tabulate coeffs in https://fits.gsfc.nasa.gov/registry/tpvwcs/tpv.html
  (make-array
   '(40 3)
   :initial-contents
   '((0 0 0) ;; PV1_0 +  
     (1 0 0) ;; PV1_1 * xi +
     (0 1 0) ;; PV1_2 * eta +
     (0 0 1) ;; PV1_3 * r +
     (2 0 0) ;; PV1_4 * xi^2 +
     (1 1 0) ;; PV1_5 * xi * eta +
     (0 2 0) ;; PV1_6 * eta^2 +
     (3 0 0) ;; PV1_7 * xi^3 +
     (2 1 0) ;; PV1_8 * xi^2 * eta +
     (1 2 0) ;; PV1_9 * xi * eta^2 +
     (0 3 0) ;; PV1_10 * eta^3 +
     (0 0 3) ;; PV1_11 * r^3 +
     (4 0 0) ;; PV1_12 * xi^4 +
     (4 3 1) ;; PV1_13 * xi^3 * eta +
     (2 2 0) ;; PV1_14 * xi^2 * eta^2 +
     (1 3 0) ;; PV1_15 * xi * eta^3 +
     (0 4 0) ;; PV1_16 * eta^4 +
     (5 0 0) ;; PV1_17 * xi^5 +
     (4 1 0) ;; PV1_18 * xi^4 * eta +
     (3 2 0) ;; PV1_19 * xi^3 * eta^2 +
     (2 3 0) ;; PV1_20 * xi^2 * eta^3 +
     (1 4 0) ;; PV1_21 * xi * eta^4 +
     (0 5 0) ;; PV1_22 * eta^5 +
     (0 0 5) ;; PV1_23 * r^5 +
     (6 0 0) ;; PV1_24 * xi^6 +
     (5 1 0) ;; PV1_25 * xi^5 * eta +
     (4 2 0) ;; PV1_26 * xi^4 * eta^2 +
     (3 3 0) ;; PV1_27 * xi^3 * eta^3 +
     (2 4 0) ;; PV1_28 * xi^2 * eta^4 +
     (1 5 0) ;; PV1_29 * xi * eta^5 +
     (0 6 0) ;; PV1_30 * eta^6 +
     (7 0 0) ;; PV1_31 * xi^7 +
     (6 1 0) ;; PV1_32 * xi^6 * eta +
     (5 2 0) ;; PV1_33 * xi^5 * eta^2 +
     (4 3 0) ;; PV1_34 * xi^4 * eta^3 +
     (3 4 0) ;; PV1_35 * xi^3 * eta^4 +
     (2 5 0) ;; PV1_36 * xi^2 * eta^5 +
     (1 6 0) ;; PV1_37 * xi * eta^6 +
     (0 7 0) ;; PV1_38 * eta^7 +
     (0 0 7) ;; PV1_39 * r^7
     )
   ;;
   :element-type '(integer 0 7)))



#|
for a distorted WCS, with a forward function pix2ra-func that takes
xpix,ypix to ra,dec, convert ra,dec to pixels through an iterative process

The Jacobian [[dra/dx, dDec/dx],[dRA/dy,dDec/dy]] obtained from exact pix-to-radec
transform is inverted and is used to improve an initial guess for the pixel 
values.

xp0,yp0 are initial estimates of the pixels, eg obtained by assuming a simple TAN
|#
(defun iteratively-solve-radec-to-pix (wcs ra dec xp0 yp0 pix2ra-func)
  (declare (type wcs-radec-tan wcs)
	   (type double-float ra dec xp0 yp0)
	   (type (function (wcs-radec-tan double-float double-float) (values double-float double-float))
		 pix2ra-func))
  (let ((xp xp0)
	(yp yp0)
	(dx 0.1d0) ;; pixel increments for computing derivs
	(dy 0.1d0))
    ;; macro to fix ra-a if they jump the 360 boundary 
    (macrolet ((fix-360-jump (ra-a ra-b) 
		 `(cond  ((> (- ,ra-a ,ra-b) 180) ;; eg ra-a=359 ra-b=1
			  (decf ,ra-a 360d0))
			 ((> (- ,ra-b ,ra-a) 180) ;; eg ra-a=1 ra-b=359
			  (incf ,ra-a 360d0))))
	       (fix-zero-deriv (deriv)
		 `(when (< (abs ,deriv) 1d-40)
		    (setf ,deriv 1d-40))))
      (loop
	for i below 100  ;; usually takes about 3 iterations
	do ;; compute the four derivatives
	   (multiple-value-bind (ra0 dec0) ;; initial point mapped into by current xp,yp
	       (funcall pix2ra-func wcs xp yp)
	     (fix-360-jump ra0 ra) ;; adjust ra0 by 360 to be close to original ra
	     ;; change x and compute dx derivs
	     (multiple-value-bind (ra/x dec/x) 
		 (funcall pix2ra-func wcs (+ xp dx) yp)
	       (fix-360-jump ra/x ra0) ;; adjust ra/x by 360 to be close to ra0
	       (let ((dra/dx  (/ (- ra/x  ra0)  dx))
		     (ddec/dx (/ (- dec/x dec0) dx)))
		 ;; change y and compute dy derivs
		 (multiple-value-bind (ra/y dec/y) 
		      (funcall pix2ra-func wcs xp (+ yp dy))
		   (fix-360-jump ra/y ra0) ;; adjust ra/y by 360 to be close to ra0
		   (let ((dra/dy  (/ (- ra/y  ra0)  dy))
			 (ddec/dy (/ (- dec/y dec0) dy)))
		     ;;
		     ;; if any derivatives are zero, set them to be tiny so they get discarded below
		     (fix-zero-deriv dra/dx)
		     (fix-zero-deriv dra/dy)
		     (fix-zero-deriv ddec/dx)
		     (fix-zero-deriv ddec/dy)
		     ;;
		     ;; compute the error in ra,dc
		     (let* ((ra-err (- ra0 ra))
			    (dec-err (- dec0 dec))
			    ;; Jacobian determinant  ad-bc where [[a b],[c d]] is
			    ;; [[dra/dx, ddec/dx],[ddec/dx, ddec/dy]]
			    (det (- (* dra/dx ddec/dy) (* ddec/dx dra/dy))))
		       
		       (when (< (abs det) 1d-100)
			 (error "Determinant ~A of WCS transform Jacobian is close to zero." det))

		       ;; aa,bb,cc,dd are inverse matrix of derivs
		       (let* ((aa (/ ddec/dy det))
			      (dd (/ dra/dx  det))
			      (bb (/ (- dra/dy) det))
			      (cc (/ (- ddec/dx) det))
			      ;;
			      (delx (+ (* aa ra-err) (* bb dec-err)))
			      (dely (+ (* cc ra-err) (* dd dec-err))))

			 ;; shrink the deltas to a sane value if they're big
			 #+nil
			 (loop
			   while (or (> (abs delx) 1000d0) (> (abs dely) 1000d0))
			   do (setf delx (/ delx 10d0))
			      (setf dely (/ dely 10d0)))
			 
			 ;; and adjust xp,yp to the new value
			 (decf xp  delx)
			 (decf yp  dely)

			 #+nil
			 (progn
			   (format t "delx=~,3F dely=~,3F det-~,5F  dra/dx=~,3F dra/dy=~,3F   ddec/dx=~,3F  ddec/dy=~,3F~%"
				   delx dely det dra/dx dra/dy ddec/dx ddec/dy)
			   (format t "xp: ~,3F yp: ~,3F  ra0: ~,3F dec0: ~,2F  ra-err: ~,5F  dec-err: ~,5F~%~%"
				   xp yp ra0 dec0 ra-err dec-err))
			 ))))))
	     ;;
	     (when (and (< (abs (- ra ra0)) 0.000001d0)
			(< (abs (- dec dec0)) 0.000001d0)
			(> i 4))
	       (return)))
	   finally (error "Could not converge when inverting PV-type WCS."))

      ;;
      (values xp yp))))
		       
				 
