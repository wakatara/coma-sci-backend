;; some photometric conversions


;; FIXME - should have better notation so we so it is clear V=Vj,etc

(in-package phot-transforms)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lupton https://www.sdss.org/dr12/algorithms/sdssubvritransform/
;; B = u - 0.8116*(u - g) + 0.1313;  sigma = 0.0095
;; B = g + 0.3130*(g - r) + 0.2271;  sigma = 0.0107

;; V = g - 0.2906*(u - g) + 0.0885;  sigma = 0.0129
;; V = g - 0.5784*(g - r) - 0.0038;  sigma = 0.0054

;; R = r - 0.1837*(g - r) - 0.0971;  sigma = 0.0106
;; R = r - 0.2936*(r - i) - 0.1439;  sigma = 0.0072

;; I = r - 1.2444*(r - i) - 0.3820;  sigma = 0.0078
;; I = i - 0.3780*(i - z)  -0.3974;  sigma = 0.0063

(defun sloan-u+g-to-Bj/lupton (u g)
  #I(u - 0.8116*(u - g) + 0.1313)) ;; infix reader macro
(defun sloan-g+r-to-Bj/lupton (g r)
  #I( g + 0.3130*(g - r) + 0.2271))
;;
(defun sloan-u+g-to-Vj/lupton (u g)
  #I(g - 0.2906*(u - g) + 0.0885))
(defun sloan-g+r-to-Vj/lupton (g r)
  #I(g - 0.5784*(g - r) - 0.0038))
;;
(defun sloan-g+r-to-Rc/lupton (g r)
  #I(r - 0.1837*(g - r) - 0.0971))
(defun sloan-r+i-to-Rc/lupton (r i)
  #I(r - 0.2936*(r - i) - 0.1439))
;;
(defun sloan-r+i-to-Ic/lupton (r i)
  #I(r - 1.2444*(r - i) - 0.3820))
(defun sloan-i+z-to-Ic/lupton (i z)
  #I(i - 0.3780*(i - z)  -0.3974))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; JESTER set taken from
;; http://www.sdss.org/dr4/algorithms/sdssUBVRITransform.html
;;
;; based on this table taken from http://xxx.lanl.gov/abs/astro-ph/0506022
;; Jester et al 2005
;;
;; All stars with Rc-Ic < 1.15
;;         Transformation                RMS residual
;;     u-g    =    1.28*(U-B)   + 1.13      0.06
;;     g-r    =    1.02*(B-V)   - 0.22      0.04
;;     r-i    =    0.91*(Rc-Ic) - 0.20      0.03
;;     r-z    =    1.72*(Rc-Ic) - 0.41      0.03
;;     g      =    V + 0.60*(B-V) - 0.12    0.02
;;     r      =    V - 0.42*(B-V) + 0.11    0.03
;;
;; All stars with Rc-Ic < 1.15     
;;         Transformation                RMS residual
;;     U-B    =    0.78*(u-g) - 0.88        0.05
;;     B-V    =    0.98*(g-r) + 0.22        0.04
;;     V-R    =    1.09*(r-i) + 0.22        0.03
;;     Rc-Ic  =    1.00*(r-i) + 0.21        0.01
;;     B      =    g + 0.39*(g-r) + 0.21    0.03
;;     V      =    g - 0.59*(g-r) - 0.01    0.01





(defun sloan-gr-to-VB/jester  (g r)
  "convert sloan g,r to V,B - from Jester et al - +/- 0.03"
  (values
   (- g (* 0.59d0 (- g r)) 0.01d0)
   (+ g (* 0.39d0 (- g r)) 0.21d0)))

   
(defun VB-to-sloan-gr/jester (v b)
  "convert V,B to Sloan g,r - from Jester et al -  +/- 0.03"
  (values
   (+ v (*  0.60d0  (- b v))  -0.12d0)
   (+ v (* -0.42d0  (- b v))   0.11d0)))


;;(defun VRc-to-sloan-r (v rc)
;; "convert V,Rc to Sloan r - from Fukugita et al AJ 111 1748 - error is
;;their sample set is about 0.03 +/- 0.03"
;;  (+ v (* -0.84d0 (- v rc)) 0.13d0))

(defun B-V-to-sloan-g-r/jester (B-V)
  "convert B-V to Sloan g-r - from Fukugita et al AJ 111 1748"
  (+ (* 1.02d0 B-V) -0.22d0))


(defun Rc-Ic-to-sloan-r-i/jester (Rc-Ic)
  "convert Rc-Ic to Sloan r-i - Jester et al - valid only for Rc-Ic<1.15"
  (+ (* 0.91d0 Rc-Ic) -0.20d0))

(defun sloan-r-i-to-Rc-Ic/jester (r-i)
  "convert Rc-Ic to Sloan r-i -  - Jester et al - valid only for Rc-Ic<1.15 -
this is not consistent with the inverse equation used in sloan-r-i-to-Rc-Ic/jester
and is what causes the Jester routines to give a different answer than Lupton and
Fukugita - CAVEAT USER"
  (+ (* 1.00d0  r-i) 0.21d0))
;; the following expression is the inversion of Rc-Ic-to-sloan-r-i/jester 
;; It causes the transformations to be consistent with their inverse 
;; so that test-sloan-conversions/jester correctly returns the input
;; values, but it does not make the jester magnitutudes match fukugita and lupton
;;  (* (/ 0.91d0) (+ r-i  0.20)))



(defun Rc-Ic-to-sloan-r-z/jester (Rc-Ic)
  "convert Rc-Ic to Sloan r-z - from Jester et al"
  (+ (* 1.72d0 Rc-Ic) -0.41d0))


(defun sloan-r-z-to-Rc-Ic/jester (r-z)
  "convert Sloan r-z to Rc-Ic - from Jester et al - a simple
  inversion of the inverse transform, not an transform derivd by Jester"
  (* (/ 1d0 1.72d0) 
     (+ r-z 0.41d0)))



  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a composite function to get V,I from sdss system
(defun BVRI-from-sloan-gri/jester (g r i)
  "Estimate B,V,Rc,Ic from Sloan g,r,i - returns (values B V Rc Ic)"
  (let (Ic Rc B V Rc-Ic)
    (multiple-value-setq (V B) (sloan-gr-to-VB/jester g r))
    (setf Rc-Ic (sloan-r-i-to-Rc-Ic/jester (- r i)))
    ;;
    (setf Rc (+ V (* -1.09d0 (- r i)) -0.22d0))
    ;;
    (setf Ic (- Rc Rc-Ic))
    (values B V Rc Ic)))
    

;; here is a test to convert g,r,i to bvri and back
(defun test-sloan-conversions/jester (g r i)
  "consistency check that converts sloan g,r,i to B,V,Rc,Ic and then
converts back, printing out recovered g,r,i values."
  (multiple-value-bind (b v rc ic) (bvri-from-sloan-gri/jester g r i)
    (let*
	((gg  (+ v (*  0.60d0  (- b v)) -0.12d0))
	 (rr1 (+ v (* -0.42d0  (- b v))  0.11d0))
	 (r-i (Rc-Ic-to-sloan-r-i/jester (- Rc Ic)))
	 (ii1 (- rr1 r-i)))
      (format t "g=~F~%r=~F~%i=~F~%" gg rr1 ii1))))
     
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; color conversions from Fukugita et al AJ 111 1748
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sloan-gr-to-VB/fukugita  (g r)
  "convert sloan g,r to V,B - from Fukugita et al AJ 111 1748 - error is
their sample set is about 0.03 +/- 0.03"
  (let ((gg (+ g  0.12d0))
	(rr (+ r -0.11d0)))
    (values
     (+ (* 0.467d0 gg) (* 0.533d0 rr))
     (+ (* 1.419d0 gg) (* -0.419d0 rr)))))

(defun VB-to-sloan-gr/fukugita (v b)
  "convert V,B to Sloan g,r - from Fukugita et al AJ 111 1748 -
derived by inverting above - error is their sample set is about
0.03 +/- 0.03"
  (values
   (+ v (* 0.56d0  (- b v)) -0.12d0)
   (+ v (* -0.49d0 (- b v))  0.11d0)))


(defun VRc-to-sloan-r/fukugita (v rc)
  "convert V,Rc to Sloan r - from Fukugita et al AJ 111 1748 - error is
their sample set is about 0.03 +/- 0.03"
  (+ v (* -0.84d0 (- v rc)) 0.13d0))

(defun B-V-to-sloan-g-r/fukugita (B-V)
  "convert B-V to Sloan g-r - from Fukugita et al AJ 111 1748"
  (+ (* 1.05d0 B-V) -0.23d0))


(defun Rc-Ic-to-sloan-r-i/fukugita (Rc-Ic)
  "convert Rc-Ic to Sloan r-i - from Fukugita et al AJ 111 1748 - note
  that the two solutions don't quite mesh at the boundary rc-ic=1.1 5"
  (cond ((< Rc-Ic +1.15d0)
	 (+ (* 0.98d0 Rc-Ic) -0.23d0))
	(t
	 (+ (* 1.40d0 Rc-Ic) -0.72d0))))

(defun sloan-r-i-to-Rc-Ic/fukugita (r-i)
  "convert Rc-Ic to Sloan r-i - from Fukugita et al AJ 111 1748 - note
  that the two solutions don't quite mesh at the boundary rc-ic=1.33"
  (cond ((< r-i +0.89699020d0)
	 (*  (/ 1d0 0.98d0) (+ r-i +0.23d0)))
	(t
	 (* (/ 1d0 1.40d0)  (+ r-i +0.72d0)))))



(defun Rc-Ic-to-sloan-r-z/fukugita (Rc-Ic)
  "convert Rc-Ic to Sloan r-z - from Fukugita et al AJ 111 1748 - note
  that the two solutions don't quite mesh at the boundary rc-ic=1.65"
  (cond ((< Rc-Ic +1.65d0)
	 (+ (* 1.59d0 Rc-Ic) -0.40d0))
	(t
	 (+ (* 2.64d0 Rc-Ic) -2.16d0))))


(defun sloan-r-z-to-Rc-Ic/fukugita (r-z)
  "convert Sloan r-z to  Rc-Ic - from Fukugita et al AJ 111 1748 - note
  that the two solutions don't quite mesh at the boundary rc-ic=1.65"
  (cond ((< r-z +2.196d0)
	 (* (/ 1d0 1.59d0)  (+ r-z +0.40d0)))
	(t
	  (* (/ 1d0 2.64d0) (+ r-z +2.16d0)))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a composite function to get V,I from sdss system
(defun BVRI-from-sloan-gri/fukugita (g r i)
  "Estimate B,V,Rc,Ic from Sloan g,r,i - returns (values B V Rc Ic)"
  (let (Ic Rc B V Rc-Ic)
    (multiple-value-setq (V B) (sloan-gr-to-VB/fukugita g r))
    (setf Rc-Ic (sloan-r-i-to-Rc-Ic/fukugita (- r i)))
    ;;
    (setf Rc (* (/ 1d0 0.84d0)
		(- r (* 0.16d0 V) 0.13d0)))
    ;;
    (setf Ic (- Rc Rc-Ic))
    (values B V Rc Ic)))
    

;; here is a test to convert g,r,i to bvri and back
(defun test-sloan-conversions/fukugita (g r i)
  (multiple-value-bind (b v rc ic) (bvri-from-sloan-gri/fukugita g r i)
    "consistency check that converts sloan g,r,i to B,V,Rc,Ic and then
converts back, printing out recovered g,r,i values.  There are two
values of r,i printed because the equations of Fukugita are redundant,
so we arrive at final results via 2 routes for a complete check."
    (let*
	((gg (+ v (* 0.56d0 (- b v)) -0.12d0))
	 (rr1 (+ v (* -0.49d0  (- b v)) 0.11d0))
	 (rr2 (+ v (* -0.84d0 (- v rc)) 0.13d0))
	 (r-i (Rc-Ic-to-sloan-r-i/fukugita (- rc ic)))
	 (ii1 (- rr1 r-i))
	 (ii2 (- rr2 r-i)))
      (format t "g=~F~%r1=~F~%r2=~F~%i1=~F~%i2=~F~%~%" gg rr1 rr2 ii1 ii2))))



     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lupton's conversions from 
;;  http://www.sdss.org/dr4/algorithms/sdssUBVRITransform.html#Lupton2005

(defun BVRI-from-sloan-gri/lupton (g r i)
  "Estimate B,V,Rc,Ic from Sloan g,r,i - returns (values B V Rc Ic)"
  (let ((b  (+ g (* 0.313d0 (- g r)) 0.2271d0))
	(v  (- g (* 0.5784d0 (- g r)) 0.0038d0))
	(rc (- r (* 0.1837d0 (- g r)) 0.0971d0))
	(ic (- r (* 1.2444d0 (- r i)) 0.382d0)))
    (values B V Rc Ic)))



