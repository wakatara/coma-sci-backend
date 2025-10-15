

(defpackage shellsort-jk/tests
  (:use #:shellsort-jk #:cl)
  (:export
   #:test-validity
   #:compare-speed-to-builtin-sort
   #:compare-comparisons-to-builtin-sort
   ))

(in-package shellsort-jk/tests)

(defun make-unique-random-vector (n)
  (loop with v = (make-array n :element-type '(signed-byte 32))
	for i below n
	do (setf (aref v i) i)
	finally
	   (loop for j from (1- n) downto 1
		 do (rotatef (aref v j) (aref v (random j))))
	   (return v)))

(defun make-non-unique-random-vector (n)
  (loop with v = (make-array n :element-type '(signed-byte 32))
	for i below n
	do (setf (aref v i) (random n))
	finally (return v)))

(defmacro pos-log (z) ;; log that isn't paranoid about imaginary result
  `(log (the (double-float #.least-positive-double-float #.most-positive-double-float) ,z)))


;; sort a bunch of random arrays with different lengths to see if shellsort
;; fails to sort correctly
(defun test-validity (&key (nmin 1) (nmax 10000) (niter 10) (from-start 0) (from-end 0) (nstep 1))
  (flet ((test-if-sorted (v)
	   (declare (type (simple-array (signed-byte 32) (*)) v))
	   (loop for i from (+ 1 from-start) below (- (length v) from-end)
		 when (not (<= (aref v (1- i)) (aref v i)))
		   do (error "Array not sorted for length ~A v=~A" (length v) v))))
    (loop for n from nmin to nmax by nstep
	  do (loop for i below niter
		   for v of-type (simple-array (signed-byte 32) (*)) = (make-non-unique-random-vector n)
		   do (shellsort-macro
		       (< (aref v i) (aref v j))
		       (rotatef (aref v i) (aref v j))
		       from-start  (- (length v) from-end 1)
		       i j)
		      (test-if-sorted v)))))
		      
(defun multi-test-validity ()
  (loop for k below 100
	for from-start = (random 10)
	for from-end = (random 10)
	for nmin = (+ 1 from-start from-end)
	do (test-validity :nmin nmin :from-start from-start :from-end from-end :nstep 10)))
			 


;; returns (values abs-log-gamma-of-z sign-log-gamma-of-z)
(defun pos-real-log-gamma (z)  ;; function to return LogGamma(z) for Re(z)>0
         (declare (type  double-float z)
                  (optimize speed))
          (let* ((zz (- z 1.d0))  ; because this returns log-gamma(Z+1)
                 (H 1.d0 ) ;;(/ zz (+ zz 1.0d0)))  ;; first H term
                 (sum  4.162443691643908d1))  ;; load the 0th and 1st terms
            (declare (type double-float zz H sum))
            ;;
            ;; the following sum could be expressed more simply as a loop, but we unroll it
            ;; because the coefficient array would then have to be transferred into a simple array
            ;; for efficienty
            (setq H (* H (/ (- zz 0.0d0) (+ zz 1.d0))))     (setq sum (+ sum (* H -5.122424102237478d1)))
            (setq H (* H (/ (- zz 1.0d0) (+ zz 2.d0))))     (setq sum (+ sum (* H 1.133875581348898d1)))
            (setq H (* H (/ (- zz 2.0d0) (+ zz 3.d0))))     (setq sum (+ sum (* H -7.477326877723884d-1)))
            (setq H (* H (/ (- zz 3.0d0) (+ zz 4.d0))))     (setq sum (+ sum (* H 8.7828774930613d-3)))
            (setq H (* H (/ (- zz 4.0d0) (+ zz 5.d0))))     (setq sum (+ sum (* H -1.899030263766831d-6)))
            (setq H (* H (/ (- zz 5.0d0) (+ zz 6.d0))))     (setq sum (+ sum (* H 1.946334554456852d-9)))
            (setq H (* H (/ (- zz 6.0d0) (+ zz 7.d0))))     (setq sum (+ sum (* H -1.993452308865567d-10)))
            (setq H (* H (/ (- zz 7.0d0) (+ zz 8.d0))))     (setq sum (+ sum (* H 8.43318428062705d-12)))
            (setq H (* H (/ (- zz 8.0d0) (+ zz  9.d0))))    (setq sum (+ sum (* H 1.486111394972753d-12)))
            (setq H (* H (/ (- zz 9.0d0) (+ zz 10.d0))))    (setq sum (+ sum (* H -8.06056050283971d-13)))
            (setq H (* H (/ (- zz 10.0d0) (+ zz 11.d0))))   (setq sum (+ sum (* H 2.925949721659105d-13)))
            (setq H (* H (/ (- zz 11.0d0) (+ zz 12.d0))))   (setq sum (+ sum (* H -1.0218297851058d-13)))
            ;; additional terms are 3.665253931602144d-14 and -1.373290025386337d-14 but really
            ;; terms beyond first 9 do not contribute
            ;;
            (- (+ (pos-log sum)
                  0.91893853320467274d0   ;; 0.5 log(2 Pi)
                  (* (+ zz 0.5d0) (pos-log (+ zz  5.5d0))))
               (+ zz 5.5d0 0.0d0))))


;; (log (factorial n) 2), the information content needed to sort an N element array
(defun log2nfactorial (n)
  (/ (pos-real-log-gamma (+ 1d0 n))
     #.(log 2d0))) ;; convert log() to log2()
  
  

;; warning - this uses the same vector many times to minimize times
;; spent in random number generation, so it doesn't sample a wide 
;; space of sortable vectors.   However, it gives the same results
;; on different iterations.
(defun compare-speed-to-builtin-sort (&key (nvec 20) (niter 100000))
  (declare (type (integer 1 100000000) nvec niter))
  (let* ((v    (make-unique-random-vector nvec))
	 (vbak (copy-seq v)))
    (declare (type (simple-array (signed-byte 32) (*)) v vbak)
	     (optimize speed))
    (flet ((restore-v ()
	     (loop for i below (length v)
		   do (setf (aref v i) (aref vbak i)))))
      (format t "======================================~%")
      (format t "Testing shellsort-macro  NVEC=~D  NITER=~A ~%" nvec niter)
      (time (loop for k below niter
		  do
		     (restore-v)
		     (shellsort-macro
		      ;; predicate expression
		      (< (aref v i) (aref v j))
		      ;; swapper expression
		      (rotatef (aref v i) (aref v j))
		      ;; imin and imax
		      0 (1- (length v))
		      ;; variables for rotator and swapper
		      i j)))
            (format t "======================================~%")
      (format t "Testing shellsort-function  NVEC=~D  NITER=~A ~%" nvec niter)
      (time (loop for k below niter
		  do
		     (restore-v)
		     (shellsort-function
		      ;; predicate function
		      (lambda (i j)
			(declare (type fixnum i j))
			(< (aref v i) (aref v j)))
		      ;; swapper function
		      (lambda (i j)
			(declare (type fixnum i j))
			(rotatef (aref v i) (aref v j)))
		      ;; imin and imax
		      0 (1- (length v)))))
      (format t "======================================~%")
      (format t "Testing builtin sort  NVEC=~D  NITER=~A ~%" nvec niter)
      (time (loop for k below niter
		  do
		     (restore-v)
		     (sort v #'<))))))
	      


(defun compare-comparisons-to-builtin-sort (&key (nvec 20) (niter 100000))
  (declare (type (integer 1 100000000) nvec niter)
	   (optimize speed))
  (let ((ncomp 0)
	(ntheoretical (log2nfactorial nvec))) ;; theoretical minimum comparisons
    
    (flet ((%< (x y) ;; comparison that counts calls
	     (declare (type (unsigned-byte 32) x y))
	     (incf ncomp)
	     (< x y)))
      
      (format t "======================================~%")
      (format t "Testing shellsort-macro  NVEC=~D  NITER=~A ~%" nvec niter)
      (loop initially (setf ncomp 0)
	    for k below niter
	    for v of-type (simple-array (signed-byte 32) (*)) =  (make-non-unique-random-vector nvec)
	    do

	       (shellsort-macro
		;; predicate expression
		(%< (aref v i) (aref v j))
		;; swapper expression
		(rotatef (aref v i) (aref v j))
		;; imin and imax
		0 (1- (length v))
		;; variables for rotator and swapper
		i j))
      (format t "Average number of comparisons per sort: ~,2F  [~,2F x theoretical]~%"
	      (/ (* 1d0 ncomp) niter)
	      (/ (/ (* 1d0 ncomp) niter) ntheoretical))
      ;
      (format t "======================================~%")
      (format t "Testing builtin sort NVEC=~D  NITER=~A ~%" nvec niter)
      (loop initially (setf ncomp 0)
	    for k below niter
	    for v of-type (simple-array (signed-byte 32) (*)) =  (make-non-unique-random-vector nvec)
	    do
	       (sort v #'%<))
       (format t "Average number of comparisons per sort: ~,2F  [~,2F x theoretical]~%"
	      (/ (* 1d0 ncomp) niter)
	      (/ (/ (* 1d0 ncomp) niter) ntheoretical)))))	      

