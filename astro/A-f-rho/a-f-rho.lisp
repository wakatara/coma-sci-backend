

(defpackage a-f-rho
  (:use #:cl)
  (:export #:a-f-rho))


(in-package a-f-rho)

#|

From Karen Meech's Fortran code


xk=c * rhelio^2 * delta / r_aperture ^2
dm=ApparentMagSun - ApparentMagObj
c = 2.4669e19
 and
Afrho = xk * * 10^(dm/2.5)


and sigma_Afrho = (0.4/log10(e)) * Afrho * magerr

|#


(defun a-f-rho (mag-obj mag-obj-err rhelio delta r-aperture
		&key
		  filter mag-sun
		  (mag-system :ab))
  "Given the apparent
  MAG-OBJ (apparent), and MAG-OBJ-ERR
  RHELIO, DELTA   (au)
  R-APERTURE  (arcsec)
  FILTER -or- MAG-SUN (apparent)
    where FILTER is one of the usual {:uj :bj :rj :ic :rc :usdss, ... :zsdss}
return the a-f-rho measure of cometary activity.

If FILTER is given, then MAG-SYSTEM for computing magnitude of sun
defaults to :AB but can be :VEGA or :ST

Output is  (VALUES A-F-RHO A-F-RHO-ERROR  MAG-SUN)."
  
  (when (or (not (or filter mag-sun))
	    (and filter mag-sun))
    (error "Either the FILTER or MAG-SUN must be given, but not both"))

  (let* ((%mag-sun (or mag-sun (magnitude-of-sun:magnitude-of-sun
				filter :system mag-system :mag-type :apparent)))
	 (c 2.4669e19)
	 (xk (* c (expt rhelio 2) delta (/ 1.0 (expt r-aperture 2))))
	 (dm (- %mag-sun mag-obj))
	 (afrho (* xk  (expt 10 (/ dm 2.5))))
	 (afrho-err (* (/ 0.4 #.(log (exp 1.0) 10))
		       afrho
		       mag-obj-err)))
    (values afrho afrho-err %mag-sun))) 
    
    
			     






