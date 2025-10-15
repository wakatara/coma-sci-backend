#|

frwap - function wrapping to prevent boxing.

structures are designed to be sub-classed  by the packages that use them

|#


(defpackage fwrap
  (:use #:cl)
  (:export
   #:f1d #:f1d-p #:f1d-x #:f1d-y
   #:fwrap-1d-func
   #:funcall-f1d
   ))

(in-package fwrap)

;; function holder that takes R1->R1 (ie, x to y)
(defstruct f1d
  (x 0d0 :type double-float)
  (y 0d0 :type double-float)
  (params nil))

(defun fwrap-1d-func (func)
  "Take an ordinary function func(x)->y and turn it into a function
that takes an f1d, evaluates y=func(x) and places it into the y-slot."
  (lambda (f1d)
    (declare (type f1d f1d))
    (setf (f1d-y f1d)
	  (float (funcall func (f1d-x f1d)) 1d0))
    f1d))

(defmacro funcall-f1d (func x f1d)
  "Macro to call func(f1d) and return (f1d-y f1d)"
  `(progn (setf (f1d-x ,f1d) ,x)
	  (funcall ,func ,f1d) 
	  (f1d-y ,f1d)))
	       