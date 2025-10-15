
#|

Numerical integration.

For example:

  (nintegrate:romberg-integrate 
          ;; gaussian
	  (lambda (x)
	    (* (/ (sqrt (* 2 pi)))
	       (exp (* -0.50 x x))))
           ;; 
	  -10 10 :eps 1d-15)
  => 0.9999999999999999d0 ;; That is a bingo!




The FWRAPPED version looks like:

     (nintegrate:romberg-integrate/fwrap
     ;;
      (lambda (f1d)
                (let ((x (fwrap:f1d-x f1d)))
		    (setf (fwrap:f1d-y f1d)
		           (* (/ (sqrt (* 2 pi)))
		              (exp (* -0.50 x x))) ))
                     T) ;; be sure not to return a float to avoid boxing
		  ;;
      -10 10 :eps 1d-15)))

This version is much faster, but still conses a bit because arrays and
structures are allocated at the start of the integration.  However,
GC time is a minor fraction of runtime for any reasonable function -
for example, 15% for a Gaussian.


The inteval can be specified as :CLOSED or :OPEN.  In theory, :OPEN
intervals can be used to approach integrable singularities, but in
practice, it isn't that great.  Smooth functions integrate nicely, but
things without power series expansions have a bit of trouble, even something
like sqrt(x) takes a lot of computing around 0.  


|#


(defpackage nintegrate
  (:use #:cl)
  (:export
   #:romberg-integrate
   #:romberg-integrate/fwrap
   ))

(in-package nintegrate)

;; a smaller fixnum for which math was faster
(deftype sfixnum ()
  `(integer ,(ash most-negative-fixnum -1) ,(ash most-positive-fixnum -1)))

;; structure to assist in performing integration
(defstruct (romstr (:include fwrap:f1d))
  ;; trapezoid method if :CLOSED, otherwise MIDPT
  (interval :closed :type (member :open :closed)) 
  ;; polint vars
  (npi 0 :type sfixnum)
  (xpi 0d0 :type double-float)
  (cpi nil :type (or null (simple-array double-float (*))))
  (dpi nil :type (or null (simple-array double-float (*))))
  (ypi 0d0 :type double-float)
  (dypi 0d0 :type double-float)
  ;; trapzd vars
  (atr  0d0 :type double-float)
  (btr  0d0 :type double-float)
  (ntr  0 :type (unsigned-byte 20))
  (ss  0d0 :type double-float)
  ;;
  ;; romberg vars
  (a  0d0 :type double-float)
  (b  0d0 :type double-float)
  (eps  0d0 :type double-float)
  (scale  0d0 :type double-float)
  (jmax  0d0 :type (unsigned-byte 20))
  (k  0d0 :type (unsigned-byte 20))
  (s nil :type (or null (simple-array double-float (*))))
  (h nil :type (or null (simple-array double-float (*))))
  (hj nil :type (or null (simple-array double-float (*))))
  (sj nil :type (or null (simple-array double-float (*))))
  (retval 0d0 :type double-float))

(defun build-romstr (jmax k a b eps scale interval)
  (make-romstr
   :interval interval
   :jmax jmax :k k 
   :eps (float eps 1d0) :scale (float scale 1d0)
   :a (float a 1d0) :b (float b 1d0)
   :cpi (make-array k :element-type 'double-float)
   :dpi (make-array k :element-type 'double-float)
   :s  (make-array (1+ jmax) :element-type 'double-float :initial-element 0d0)
   :h  (make-array (1+ jmax) :element-type 'double-float :initial-element 0d0)
   :sj  (make-array k :element-type 'double-float :initial-element 0d0)
   :hj  (make-array k :element-type 'double-float :initial-element 0d0)))
       
 

(defun %polint (xa ya romstr)
  (declare (type (simple-array double-float (*)) xa ya)) 
  (declare (type romstr romstr))
  (declare (optimize speed))
	    

 (prog* ((y 0d0) (dy 0d0) 
	 (x (romstr-xpi romstr))
	 (n (romstr-npi romstr))
	 (c (romstr-cpi romstr))
	 (d (romstr-dpi romstr))
	 (dif 0d0) (dift 0d0)
	 (ns 0) (ho 0d0) (hp 0d0) (w 0d0) (den 0d0))

    (declare (type (simple-array double-float (*)) c d)
	     (type double-float y dy dif dift ho hp w den)
	     (type sfixnum n ns))


    (setf ns 1) 
    (setf dif (abs (- x (aref xa 0)))) 
    (do ((i 1 (+ i 1)))
	((> i n) t)
      (declare (type integer i))
      (setf dift (abs (- x (aref xa (1- i)))))
      (when 
	  (< dift dif)
	(setf ns i) 
	(setf dif dift))
      (setf (aref c (1- i)) (aref ya (1- i)))
      (setf (aref d (1- i)) (aref ya (1- i)))) 

    (setf y (aref ya (1- ns))) 
    (setf ns (1- ns)) 
    (do ((m 1 (+ m 1)))
	((> m (1- n)) t)
      (declare (type integer m))
      (do ((i 1 (+ i 1)))
	  ((> i (- n m)) t)
        (declare (type integer i))
	(setf ho (- (aref xa (1- i)) x))
	(setf hp (- (aref xa (1- (+ i m))) x))
	(setf w (- (aref c i) (aref d (1- i))))
	(setf den (- ho hp))
	(if (= den 0d0) (error " den = 0d0 in polint "))
	(setf den (/ w den))
	(setf (aref d (1- i)) (* hp den))
	(setf (aref c (1- i)) (* ho den)))

      (cond 
	((< (* 2 ns) (- n m)) 
	 (setf dy (aref c ns)))
	(t 
	 (setf dy (aref d (1- ns)))
	 (setf ns (1- ns))))
      (setf y (+ y dy))) 
    (setf (romstr-ypi romstr) y)
    (setf (romstr-dypi romstr) dy)
    nil))


(defun trapzd (romstr func)
  (declare (type romstr romstr)
	   (optimize speed)
	   (type (function (fwrap:f1d) t) func))
  
  (prog ((sum 0d0) (tnm 0) (del 0d0) (x 0d0)
	 (s (romstr-ss romstr)) ;; s is a static var in c code
	 (it 0)
	 (a (romstr-atr romstr))
	 (b (romstr-btr romstr))
	 (n (romstr-ntr romstr)))

     (declare (type double-float sum del x a b s)
	      (type sfixnum it tnm)
	      (type (integer 0 20) n))

     (cond 
       ((= n 1) 
	(setf s (* (* 0.5d0 (- b a)) 
		   (+ (fwrap:funcall-f1d func a romstr)
		      (fwrap:funcall-f1d func b romstr)))))
       (t 
	(setf it (the sfixnum (ash 1 (- n 2))))
	(setf tnm it)
	(setf del (/ (- b a) tnm)) 
	(setf x (+ a (* 0.5d0 del))) 
	(setf sum 0d0)
	(do ((j 1 (+ j 1)))
	    ((> j it) t)
	  (declare (type sfixnum j))
	  (setf sum (+ sum  (fwrap:funcall-f1d func x romstr)))
	  (setf x (+ x del)))
	(setf s (* 0.5d0 (+ s (/ (* (- b a) sum) tnm)))) ))
     ;;
     (setf (romstr-ss romstr) s)
     t
     )) ;; value returned is in (romstr-ss romstr)




(defun midpt (romstr func)
  (declare (type romstr romstr)
	   (optimize speed)
	   (type (function (fwrap:f1d) t) func))
  (prog ((tnm 0d0) (it 0) (del 0d0) (ddel 0d0) (sum 0d0) (x 0d0)
	 (a (romstr-atr romstr))
	 (b (romstr-btr romstr))
	 (n (romstr-ntr romstr))
	 (s (romstr-ss romstr)))
  (declare (type double-float tnm del ddel sum s a b x)
	   (type (unsigned-byte 24) it n))
  
  (cond 
    ((= n 1)
     (setf s (* (- b a) 
		(fwrap:funcall-f1d func  (* 0.5d0 (+ a b)) romstr))))
    (t  
     (setf it 1)
     (loop for i of-type sfixnum from 1 to (- n 2) do (setf it (* it 3)))
     (setf tnm (float it 1d0)) 
     (setf del (/ (- b a) (* 3d0 tnm)))
     (setf ddel (+ del del)) 
     (setf x (+ a (* 0.5d0 del))) 
     (setf sum 0d0)

     (do ((j 1 (+ j 1)))
	 ((> j it) t)
       (declare (type sfixnum j))
       (setf sum (+ sum (fwrap:funcall-f1d func x romstr)))
       (setf x (+ x ddel))
       (setf sum (+ sum (fwrap:funcall-f1d func x romstr)))
       (setf x (+ x del)))

     (setf s (* (+ s (/ (* (- b a) sum) tnm)) 0.333333333333333333d0))))
  ;;
  (setf  (romstr-ss romstr) s)
  t))








;; the innermost function, that uses just a FUNC(ROMSTR) and the ROMSTR.
;; this might be usable as a basis for a multidimensional integrator
(defun %romberg-integrate/romstr   (func romstr)
  (declare (type (function (fwrap:f1d) romstr) func)
	   (type romstr romstr)
	   (optimize speed))
  (prog* ((k (romstr-k romstr))
	  (jmax (romstr-jmax romstr))
	  (closed (eq (romstr-interval romstr) :CLOSED))
	  (a (romstr-a romstr))
	  (b (romstr-b romstr))
	  (km (1- k))
	  (jmaxp (+ jmax 1))
	  (eps (romstr-eps romstr))
	  (scale (romstr-scale romstr))
	  (s (romstr-s romstr))
	  (h (romstr-h romstr))
	  (hj (romstr-hj romstr))
	  (sj (romstr-sj romstr))
	  (ss 0d0) (dss 0d0))

     (declare (type sfixnum k jmax jmaxp km)
	      (type (simple-array double-float (*)) s)
	      (type (simple-array double-float (*)) h) 
	      (type (simple-array double-float (*)) sj) 
	      (type (simple-array double-float (*)) hj) 
	      (type double-float a b eps ss dss))

     ;;

     (setf (aref h 0) 1d0)
 
       ;;
     (do ((j 1 (+ j 1)))
	 ((> j jmax) t)
       (declare (type (signed-byte 10) j))
       (setf (aref s (1- j))
	     (progn
	       (setf (romstr-atr romstr) a)
	       (setf (romstr-btr romstr) b)
	       (setf (romstr-ntr romstr) j)
	       (if closed
		   (trapzd romstr func)
		   (midpt  romstr func))
	       (romstr-ss romstr)))
       (when 
	   (>= j k)
	 (do ((i 1 (1+ i)))
	     ((> i k) t)
	   (declare (type (signed-byte 10) i))
	   (setf (aref sj (1- i)) (aref s (+ -2 (+ i (- j km)))))
	   (setf (aref hj (1- i)) (aref h (+ -2 (+ i (- j km))))))

	 (setf (romstr-npi romstr) k)
	 (setf (romstr-xpi romstr) 0d0)
	 (%polint hj sj romstr)
	 (setf ss (romstr-ypi romstr))
	 (setf dss (romstr-dypi romstr))
	 ;;
	 (if (< (abs dss) (* eps (+ scale (abs ss))))
	     (go end)))
       (setf (aref s j) ss)		; (aref s (1- j)))
       (setf (aref h j) (* 0.25d0 (aref h (1- j)))))
    
     (error "too many steps in romberg-integrate")
     end
     (setf (romstr-retval romstr) ss)
     romstr))


 



;; NR qromb
(defun romberg-integrate/fwrap
    (func a b
     &key (eps 1.0d-6) (scale 0d0) (jmax 20) (k 5) (interval :CLOSED))
  "Integrate FUNC(X) from A to B using Romberg's rule of order K, with EPS being
desired fractional accuracy.

SCALE is an optional parameter that sets the scale of the integral,
if it is close to zero.  Otherwise, a fractional EPS criterion will fail.
By default, it is 0.

FUNC is a fwrap'ed function that takes an FWRAP:F1D structure, takes (F1D-X F1D)
and puts its result in (F1D-Y F1D).

INTERVAL is :OPEN or CLOSED, to determine whether midpoint or trapezoid should be used."
  (declare (type real a b eps scale)
	   (type sfixnum jmax k)
	   (type (member :open :closed) interval))

  (let ((romstr  (build-romstr jmax k a b eps scale interval)))
    (%romberg-integrate/romstr func romstr)
    (romstr-retval romstr)))
  


(defun romberg-integrate
    (func a b
     &key (eps 1.0d-6) (scale 0d0)  (jmax 20)  (k 5) (interval :CLOSED))
  "Integrate FUNC(X) from A to B using Romberg's rule of order K, with EPS being
desired fractional accuracy.

SCALE is an optional parameter that sets the scale of the integral,
if it is close to zero.  Otherwise, a fractional EPS criterion will fail.
By default, it is 0.

FUNC is an ordinary function f(x)=y, taking and returning double float

INTERVAL is :OPEN or CLOSED, to determine whether midpoint or trapezoid should be used."
  (romberg-integrate/fwrap
   (fwrap:fwrap-1d-func func)
   a b
   :eps eps :scale scale :jmax jmax  :k k :interval interval))

