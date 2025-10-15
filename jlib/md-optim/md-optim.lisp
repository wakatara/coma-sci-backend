
;; a package that defines several structures for multidimensional
;; optimization that are then sub-classed by routines like powell.c
;; and amoeba.c


(defpackage md-optim
  (:use #:cl)
  (:export 
   #:md-optim-struct #:md-optim-struct-p 
   #:md-optim-struct-name #:md-optim-struct-x-vec 
   #:md-optim-struct-optim-flags #:md-optim-struct-y
   #:md-optim-struct-neval #:md-optim-struct-params
   ;;
   #:compute-error-bounds
   ))


(in-package md-optim)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the parent structure for multidim optimization structures; 
;; parent of   AMOEBA:AMOEBA-STRUCT and POWELL:POWELL-STRUCT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct md-optim-struct
  (name "un-named-md-optim-struct")
  (n 0 :type (unsigned-byte 28)) 
  ;; the vector of x values
  (x-vec (make-array 0 :element-type 'double-float)
	 :type (simple-array double-float (*)))
  ;; a vector parallel to x-vec telling if a particular dimension
  ;; is to be optimized - dimensions with NIL are treated as constant
  ;; parameters
  (optim-flags (make-array 0) :type (simple-array t (*)))
  ;; the value y(x) that is being optimized
  (y 0d0 :type double-float)
  ;; the number of function evaluations
  (neval 0 :type (unsigned-byte 28))
  ;; any additional parameters
  (params nil))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 




(defun compute-error-bounds (mdos func &key (delta 1d0) (x-scale 1d-10)
			     (bisection-eps 1d-3))
  "For an optimized MD-OPTIM-STRUCT MDOS and a FUNC that takes it as
as argument and computes MD-OPTIM-STRUCT-Y from MD-OPTIM-STRUCT-XVEC,
compute the positive and negative distances needed to travel along
each dimension to obtain a given DELTA, by default 1d0.  DELTA is
either a real, or a vector of DELTAS to substitute for the default
value of 1d0; NIL means that this dimension should be ignored, and the
returned distance will be 0d0.

X-SCALE is an optional argument for the typical scale size in
MD-OPTIM-X-VEC; it can be a scalar or vector.  It should be a few
orders of magnitude smaller than the change in X required to produce a
change of DELTA in the function.

Return (VALUES POS-DVEC NEG-DVEC DELTA-VEC) where POS-DVEC and NEG-DVEC
are the changes in the +/-1 directions, times elements in DELTA-VEC."
  (declare (type md-optim-struct mdos)
	   (type (or real vector) delta x-scale))
  ;;
  (let* 
      ((n (md-optim-struct-n mdos))
       (y0 (md-optim-struct-y mdos))
       (delta-vec (cond ((vectorp delta) delta)
			((realp delta) 
			 (make-array n :initial-element (float delta 1d0)))))
       (x-scale-vec (cond ((vectorp x-scale) x-scale)
			  ((realp x-scale) 
			   (make-array n :initial-element (float x-scale 1d0)))))
       (pos-dvec (make-array n :element-type 'double-float :initial-element 0d0))
       (neg-dvec (make-array n :element-type 'double-float :initial-element 0d0))
       ;; copy so we don't do any damage to original on interrupts
       (mdos2 (copy-structure mdos)))
    ;;
    (setf (md-optim-struct-x-vec mdos2) (copy-seq (md-optim-struct-x-vec mdos)))
    ;;
    (labels ((find-bounding-dx-outward (ndim sign delta)
	     ;; find how far in direction sign (+/-1) we need to move
	     ;; to produce a change larger than delta
	       (loop 
		with xvec = (md-optim-struct-x-vec mdos2)
		with jump = 10d0 ;; initial factor
		with x0 = (aref xvec ndim)
		with dx0 = (aref x-scale-vec ndim)
		for dx = (* sign dx0) then (* dx jump)
		for dy = (progn (setf (aref xvec ndim) (+ x0 dx))
				(funcall func mdos2)
				(- (md-optim-struct-y mdos2) y0))
		;; keep going until dy>delta,  AND we've reset JUMP
		;; to a small value so we don't overshoot by too much
		until (and (> dy delta) (< jump 5d0))
		do (format t "ndim=~A dx=~E dy=~,5F delta=~,5F~%" 
			   ndim dx dy delta)
		;; when we overshot, reset JUMP to a small value
		;; and keep going
		(when (> dy delta)
		  (setf dx (/ dx jump))
		  (setf jump 1.41d0)) ;; sqrt(2)'ish
		finally 
		(format t "DONE: ndim=~A dx=~E dy=~,5F delta=~5F~%" 
			ndim dx dy delta)
		(setf (aref xvec ndim) x0) ;; restore starting value
		(return (abs dx))))
	     ;;
	     (find-dx-in-one-dimension (ndim sign delta)
	       ;; return how far we need to travel along dimension NDIM
	       ;; in direction SIGN=+/-1 to produce a change DELTA
	       (let* ((xvec  (md-optim-struct-x-vec mdos2))
		      (x0 (aref xvec ndim))
		      (dx (find-bounding-dx-outward ndim sign delta))
		      (x1 (bisection-root:findroot 
			   ;; lambda = 0 when f(x)=y0+delta
			   (lambda (x)
			     (setf (aref xvec ndim) x)
			     (funcall func mdos2)
			     (format t "   X=~,5F  y=~,5F  y-(y0+delta)=~,5F~%"
				     x (md-optim-struct-y mdos2) 
				     (- (md-optim-struct-y mdos2) (+ y0 delta)))
			     (- (md-optim-struct-y mdos2) (+ y0 delta)))
			   ;;
			   (aref xvec ndim) ;; bisection x0
			   (+ (aref xvec ndim) (* sign dx)) ;; bisection x1
			    bisection-eps)))
		 (setf (aref xvec ndim) x0) ;; restore starting value
		 (- x1 x0))))
      ;;
      (loop for delta across delta-vec
	    for idim below n
	    when delta
	    do
	    (setf (aref pos-dvec idim) (find-dx-in-one-dimension idim +1 delta))
	    (setf (aref neg-dvec idim) (find-dx-in-one-dimension idim -1 delta)))
      ;;
      (values pos-dvec neg-dvec delta-vec))))
		   

