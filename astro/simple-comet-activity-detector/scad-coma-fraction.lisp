
(in-package simple-comet-activity-detector)

(defconstant +moffat-beta+ 2.8)


#|

A coma is modeled as (1-fcoma)*Moffatt + fcoma*Cored1/R   with the same FWHM

Our measurement metric is, as in other files, 

FRATIO = (flux inside 2 HWHM)/ (flux between 3 HWHM and 4 HWHM)

which is tabulated to a coma fraction based on a model consisting of 
fraction (1-FCOMA) times a beta=2.8 Moffat, plus fraction FCOMA 
times a cored 1/r coma.

The normalization for FCOMA is the fraction of the central peak, not 
the total flux, because coma function has infinite integral

We use BETA=2.8 for Moffat because it seems to detect activity a little bit more
strongly than 2.5

|#



#|
 A function that returns   (1-fcoma) * [Moffat(r)/Moffat(0)] + fcoma *  [ComaFunc(r)/ComaFunc(0)]
 ie, a model where the center pixel is fraction 'fcoma' of coma, with both the coma
 and the coma model having the same FWHM


 We ensure that the combined FWHM is what is given, using the empirical
 fact that a 1/r convolved with a Moffat is 1.54x wider than the Moffat alone.
 The linear interpolation is
  
       FWHM_mof = FWHM / [(1-fcoma) + fcoma * 1.876]
  
 It is not strictly correct, but the function below has a FWHM of
 1.0 (equals ~0.5 when called with r=0.5) to within a couple of
 percent for all FCOMA between 0 and 1.  So just interpolating the
 FWHMs as shown above is an OK approximation.
|#


;; given an overall profile FWHM and an FCOMA, return the respective
;; FWHM of the underlying Moffat, and the coma component.    For example,
;; if FCOMA is 0 then the underlying Moffat is just FWHM, but if FCOMA is 0.999,
;; then the underlying Moffat much be much narrower to give the same observed FWHM.
(defun fwhm-of-moffat-and-coma-from-fcoma (fcoma fwhm)
  (let* ((fwhmrat 1.876) ;; how much wider a moffat convolved 1/r is, vs moffat
	 ;; naive interpolations that are ALMOST correct
	 (fwhm-mof (/ fwhm (+ (- 1.0 fcoma) (* fcoma fwhmrat)))) 
	 (fwhm-coma (* fwhmrat fwhm-mof))  ;; true for beta=2.8 which
	 ;; Use parabolic fudge factor to correct underestimation of
	 ;; true resulting fwhm, a factor of 0.89 too small at
	 ;; f=0.5. Fudge ranges from 0.0 at fcoma=0,1 to 1.123 at
	 ;; f=0.5.  After fudge factor is applied, the final composite FWHM
	 ;; matches input FWHM to about 5 decimals.
	 (fudge  (+
		  1.0 ;; baseline multiplier in 1+ epsilon
		  (* 0.12359 ;; the epsilon excess needed at 0.5
		   (- 1.0 (* 4 (expt (- fcoma 0.50) 2))))))) ;; 1 at fcoma=0.5, 0 at fcoma=0,1
    ;;(format t "Fudge is ~A~%" fudge)
    (values (* fudge fwhm-mof)
	    (* fudge fwhm-coma))))

(defun mof+coma-func (r fcoma  &key (beta 2.8) (fwhm 1.0))
  (multiple-value-bind (fwhm-mof fwhm-coma)
      (fwhm-of-moffat-and-coma-from-fcoma fcoma fwhm)
    (let* ((a (imutils:moffat-a-for-fwhm fwhm-mof beta))
	   (coma-rmax 10.0) ;; this doesn't matter because it sets norm of 1/r coma
	   (mof0 (imutils:moffat 0.0 a beta))
	   (coma0 (imutils:1/r-coma-func 0.0  fwhm-coma coma-rmax)))
      (+ (* (- 1d0 fcoma) (/ (imutils:moffat r a beta) mof0))
	 (* fcoma (/ (imutils:1/r-coma-func r fwhm-coma coma-rmax) coma0))))))
  




;; compute the fratio (light between 3-4xFWHM to 1-2xFWHM) given a FCOMA.
(defun mof+coma-fratio (fcoma  &key (beta +moffat-beta+) (hwhm 1.0)) 
  (let ((fwhm (* 2.0 hwhm)))
    (flet ((ifunc (r)
	     (* 2 pi r (mof+coma-func r fcoma :beta beta :fwhm fwhm))))
      (/
       (nintegrate:romberg-integrate #'ifunc (* 3d0 hwhm)  (* 4d0 hwhm) :eps 1d-4)
       (nintegrate:romberg-integrate #'ifunc 0d0 (* 2d0 hwhm) :eps 1d-4)))))


;; table of (frac-coma . fratio)
(defparameter *fratio-table* nil)

(eval-when (:load-toplevel)
  (setf *fratio-table*
	(coerce 
	 (loop for fcoma from 0.0 to 1.0 by 0.005
	       for fratio = (mof+coma-fratio fcoma)
	       collect (cons fcoma fratio))
	 'vector))) 

        
;; fratio is the ratio (Flux from 1-2 FWHM) / (Flux from 3-4 FWHM).
;; Use this to estimate the fraction of peak (r=0) flux from the coma.
(defun estimate-coma-fraction-and-err-from-fratio (fratio fratio-err)
  (loop
    ;; the pair from the table that is closest to fratio
    with best-pair = nil and best-dist = 1d100
    ;; the pair from the table that corresponds to fratio+fratio-err
    with best-epair = nil and best-edist = 1d100 
    for pair across *fratio-table*
    for  fcoma = (car pair) and fr = (cdr pair)
    for dist = (abs (- fr fratio))
    ;; compute the closest to fratio+fratio-err
    for edist = (abs (- fr (+ fratio fratio-err)))
    when (< dist best-dist)
      do (setf best-dist dist
	       best-pair pair)
    when (< edist best-edist)
      do (setf best-edist edist
	       best-epair pair)    
    finally
       (return
	 (values 
	      (car best-pair)
	      (abs (- (car best-pair)
		      (car best-epair)))))))


;; get the FWHM from a radial profile given by FVEC,RVEC, performing linear
;; interpolation to get a more accurate answer
(defun get-fwhm-from-fvec-rvec (fvec rvec)
  (declare (type (simple-array single-float (*)) fvec rvec))
  (loop with f0 = (aref fvec 0)
	for ir below (length fvec)
	for r1 = (aref rvec ir)
	for f1 = (/ (aref fvec ir) f0)
	when (< f1 0.5) ;; when we cross the 0.5 boundary
	  do
	     (return
	       ;; linearly interpolate last 2 points
	       (let ((f0 (/ (aref fvec (1- ir)) f0))
		     (r0 (aref rvec (1- ir))))
		 (let ((frac (/ (- 0.5 f0) ;; fraction of the way 0.5 is from f0 to f1
				(- f1 f0))))
		   #+nil
		   (format t "f0=~A f1=~A  r0=~A r1=~A   frac=~A~%"
			   f0 f1 r0 r1 frac)
		   (* 2 ;; HWHM -> FWHM
		      (+ r0 (* frac (- r1 r0)))))))))


;; integrate a radial profile given by FVEC,RVEC from r1 to r2
;; using trapezoids, performing linear interpolation between
;; pixel bins if needed
(defun integrate-fvec-and-rvec (fvec rvec rmin rmax &key (background 0.0))
  (declare (type (simple-array single-float (*)) fvec rvec)
	   (type single-float rmin rmax background)
	   (optimize speed))
  (loop with integral of-type single-float = 0.0
	for ir from 0 below (1- (length fvec))
	for r0 = (aref rvec ir)
	for r1 = (aref rvec (1+ ir))
	for f0 = (- (aref fvec ir) background)
	for f1 = (- (aref fvec (1+ ir)) background)
	for df/dr = (/ (- f1 f0) (- r1 r0))
	;; is this in the integral?
	when (not (or (> rmin r1)   ;; bottom bound is above [r0,r1]
		      (< rmax r0))) ;; top bound is below [r0,r1]
	  do (let* ((r0* (max r0 rmin))
		    (r1* (min r1 rmax))
		    (dr* (- r1* r0*))
		    ;; lineraly interpolate f at f0*, f1* in case they're not f0,f1
		    (f0* (+ f0 (* df/dr (- r0* r0))))
		    (f1* (+ f0 (* df/dr (- r1* r0))))
		    ;; 2 pi r flux(r)
		    (a0  (* 6.2831855 r0* f0*))
		    (a1  (* 6.2831855 r1* f1*))
		    (trapezoid-area
		      (* dr* 0.5 (+ a0 a1))))
	       (incf integral trapezoid-area))
	finally
	   (return integral)))

	  
	  
		 
  
	   



;; compute the integrated flux of the Moffatt part only of a
;; profile with total FWHM and given central flux
(defun estimate-nucleus-flux (fcoma fwhm &key (central-flux 1.0) (beta +moffat-beta+))
  (let* ((fwhm-mof (fwhm-of-moffat-and-coma-from-fcoma fcoma fwhm))
	 (a (imutils:moffat-a-for-fwhm fwhm-mof beta))
	 (mof0 (imutils:moffat 0.0 a beta :norm 1.0))
	 ;; the part of the central flux attributable to mofatt
	 (moffat-central-flux (* central-flux (- 1.0 (max 0.0 fcoma)))))
    ;;(format t "Central flux: ~A    Mof0: ~A~%"  central-flux moffat-central-flux)
    ;; figure out how much we need to rescale this norm=1 mofatt to get the central flux,
    ;; after adjusting downward by fcoma
    ;; for example, if moffatt-central-flux=1 but mof0=0.5, then the moffatt must be
    ;; lifted up by factor of 2
    (/ moffat-central-flux mof0)))
    
    
