
;; general linear least squares lisp taken from translation
;; of NR v1 - NOT optimized for efficiency

;; also includes some simple linear and quadratic regressions

(defpackage llsq
  (:use #:cl)
  (:export
   ;; llsq.lisp
   #:lfit-funcs	;; fit basis given as functions of x vector
   #:lfit-values ;; fit basis given as vectors (actually, columns in matrix)
   #:lfit-polynomial  ;; fit a polynomial y=c + c1 x^1 + ...
   ;;
   ;; simple-regressions.lisp
   #:linear-regression
   #:quadratic-regression
   ;;
   ;; orthogonal-fit.lisp
   #:orthogonal-line-fit
   ))
