
(defpackage #:bisection-root
  (:use #:common-lisp)
  (:export
   #:findroot))

(in-package bisection-root)


;; this turns out to be rather inefficient because of float-boxing (sigh)
;;
(defun findroot (func x1 x2 &optional (eps 1.0d-8))
  "find the root (zero-crossing) of double-precision func of one
double-precision variable.  The root is assumed to lie in [x1,x2].
eps is the precision desired"
  (declare (type (function (double-float) double-float)
                 func)
           (type double-float x1 x2 eps)
           (optimize speed))
  (let* ((top x2)
         (bot x1)
	 (cent (the double-float (* 0.5d0 (+ top bot))))
         (f-bot (funcall func bot))
         (f-top (funcall func top))
         (f-cent (funcall func cent))
	 (s (if (< f-top f-bot) -1.0d0 1.0d0))
	 (n 0))
    (declare (double-float cent top bot f-top f-bot f-cent s)
	     (type fixnum n))
    (loop
     (incf n)
     (if (> n 300) (error "Too many iterations in findroot"))
     (if (< (abs f-cent) eps) (return cent)) ;; done
     (progn
       (if (< (* s f-cent) 0)
	   (progn (setf bot cent)
		  (setf cent (the double-float (/ (+ cent top) 2.0d0))))
	 (progn (setf top cent)
		(setf cent (the double-float (/  (+ cent bot) 2.0d0)))))
       (setf f-cent (funcall func cent))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
