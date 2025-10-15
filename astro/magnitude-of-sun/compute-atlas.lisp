
;; helper functions to compute the magnutide of sun in atlas system
;; not used in production code, only to fill in constants of
;; magnitude-of-sun


;; color conversion from
;; https://iopscience.iop.org/article/10.1088/1538-3873/aabadf
;;
;; J. L. Tonry et al 2018 PASP 130 064505
;; DOI 10.1088/1538-3873/aabadf

(defpackage magnitude-of-sun/atlas
  (:use #:cl #:magnitude-of-sun)
  (:export #:compute-atlas-terms))

(in-package magnitude-of-sun/atlas)


(defun c-from-ps (g r)
  (+ (* 0.49 g) (* 0.51 r)))


(defun o-from-ps (r i)
  (+ (* 0.55 r) (* 0.45 i)))

(defun compute-atlas-terms ()
  `((:catlas
     ,@(loop for system in '(:vega :ab  :st)
	     collect
	     (c-from-ps
	      (magnitude-of-sun :gps1 :system system :mag-type :apparent)
	      (magnitude-of-sun :rps1 :system system :mag-type :apparent))))
    (:oatlas
     ,@(loop for system in '(:vega :ab  :st)
	     collect
	     (c-from-ps
	      (magnitude-of-sun :rps1 :system system :mag-type :apparent)
	      (magnitude-of-sun :ips1 :system system :mag-type :apparent))))))
	    


