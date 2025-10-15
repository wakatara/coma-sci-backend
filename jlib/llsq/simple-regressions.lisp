
(in-package llsq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some simple, fast linear and quadratic regression routines,
;; that assume that the relevant sums have been computed.  These
;; will avoid matrix allocation and computations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(declaim (inline %quadratic-regression %linear-regression))



(defun quadratic-regression (s1 sx sy sx2 sx3 sx4 sxy sx2y)
  (declare (type double-float s1 sx sy sx2 sx3 sx4 sxy sx2y)
	   (optimize speed))
  "Perform a quadratic regression of y=c2*x^2+c1*x+c0,
returning (values c2 c1 c0) where s1=sum(1/sigma^2),
sx=sum(x/sigma^2), sx2y=sum(x^2*y/sigma^2), etc.  May not be as robust
is a proper matrix solution with pivoting.

Does NOT return errors."
  (let* ((hxx   (- sx2  (/ (* sx sx)   s1)))
	 (hxy   (- sxy  (/ (* sx sy)   s1)))
	 (hxx2  (- sx3  (/ (* sx2 sx)  s1)))
	 (hx2y  (- sx2y (/ (* sx2 sy)  s1)))
	 (hx2x2 (- sx4  (/ (* sx2 sx2) s1)))
	 ;;
	 (c2 (/ (- (* hx2y hxx)   (* hxy  hxx2))
		(- (* hxx hx2x2)  (* hxx2 hxx2))))
	 (c1 (/ (- (* hxy hx2x2)  (* hx2y hxx2))
		(- (* hxx hx2x2)  (* hxx2 hxx2))))
	 (c0 (- (/ sy s1)
		(* c1 (/ sx s1))
		(* c2 (/ sx2 s1)))))
    (values c2 c1 c0)))


;; return (values c1 c0) where y = c1 x + c0
(defun linear-regression (s1 sx sy sx2 sxy)
  "Perform a linear regression y=c1*x+c0 returning (values c1 c0)
where s1=sum(1/sigma^2), sx=sum(x/sigma^2), sxy=sum(x*y/sigma^2).

Does NOT return errors."
  (declare (type double-float s1 sx sy sx2 sxy)
	   (optimize speed))
  (let* ((c1 (/ (- (* s1 sxy) (* sy sx))
		(- (* s1 sx2) (* sx sx))))
	 (c0  (/ (- sy (* c1 sx)) s1)))
    (values c1 c0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

