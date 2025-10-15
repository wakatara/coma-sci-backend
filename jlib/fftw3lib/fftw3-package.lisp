

(defpackage fftw3lib
  (:use #:cl)
  (:export
   ;; fftw3-plans.lisp
   #:destroy-all-fftw3-plan ;; last ditch function
   #:fftw3-plan #:fftw3-plan-rank #:fftw3-plan-ndim  #:fftw3-plan-nptr
   #:fftw3-plan-in-equals-out  #:fftw3-plan-float-type
   #:fftw3-plan-input-type  #:fftw3-plan-output-type
   #:fftw3-plan-pair
   #:destroy-fftw3-plan
   #:destroy-fftw3-plan-pair
   #:execute-fftw3-plan
   #:execute-fftw3-plan-pair
   ;;
   ;; transform-c2c.lisp
   #:build-fftw3-plan-pair-float-c2c
   #:build-fftw3-plan-pair-double-c2c
   ;;
   ;; utils.lisp
   #:complex-fft-index-to-frequency
    
   
   ))

(in-package fftw3lib)

(defparameter *standard-optimize-settings*
  '(optimize
    speed
    (safety 2)
    (space 0)
    (debug 1)
    (compilation-speed 0)
    #+:lispworks (hcl:fixnum-safety 0))
  "The standard optimize settings used by most declaration expressions.")

;; a bit smaller than a fixnum, for faster math at safety>0
(deftype mini-fixnum ()
  '(integer
    #.(ash most-negative-fixnum -2)  
    #.(ash most-positive-fixnum -2)))


(deftype small-index ()
  '(integer
    0
    #.(ash most-positive-fixnum -4)))




