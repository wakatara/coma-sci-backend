




(in-package llsq)



;; numerical recipes routines for linear fitting 

(defmacro fref (arr &rest indices)
  `(aref ,arr ,@(mapcar #'(lambda (x) (list '1- x)) indices)))

;; a version that guarantees optimizable values for int arrays
(defmacro ifref (arr &rest indices)
  `(the (unsigned-byte 28)
    (aref ,arr ,@(mapcar #'(lambda (x) (list '1- x)) indices))))

(defmacro fset (a b) 
  `(setf (fref ,(second a) ,@(cddr a)) ,b))

(defun dfloat (x) 
  (declare (type number x))
  (the double-float (coerce x 'double-float)))


(defun lfit-funcs (x y sig funcs &key mfit lista a)
  "Fit y=c1*func1(x)+c2*func2(x),..] where X,Y are vectors of
double-float and FUNCS is a list of basis functions.  SIG is a
vector of errors on y, MFIT is the number of basis functions we are
fitting, and LISTA is a (simple-array (UNSIGNED-BYTE 28) (*)) with the
numbers of the dimensions that are being fit, starting at 1.  For
example, if there are a total of 3 basis functions, and their
coefficients 1 and 3 are being fit, and parameter 2 is being
skipped (held constant), then MFIT=2 and LISTA=#(1 3 2).

Returns (VALUES A COVAR CHISQ) where A is the vector of
coefficients, COVAR is the MFITxMFIT covariance matrix, and CHISQ
is the final chi square.

Note that the keyword argument A containing the weight vector
must be specified and initialized if an incomplete LISTA set is
used - otherwise the weights of the non-fitted parameters are
zero.

Note that the index numbers in LISTA begin at 1, not 0."

  (declare (type (simple-array double-float (*)) x)) 
  (declare (type (simple-array double-float (*)) y)) 
  (declare (type (simple-array double-float (*)) sig))
  (declare (type (or null (simple-array double-float (*))) a))
  (declare (type (or null (unsigned-byte 28)) mfit))
  (declare (type (or null (simple-array (unsigned-byte 28) (*))) lista)) 

  (prog* ((chisq 0d0) 
	  (kk 0)
	  (mfit (or mfit (length funcs)))
	  (lista (or lista
		     (make-array  mfit :element-type '(unsigned-byte 28)
				  :initial-contents 
				  (loop for i from 1 to mfit collect i))))
	  (ihit 0)   
	  (ndata (array-dimension x 0))
	  (ma (array-dimension lista 0))
	  (ncvm ma)
	  (a (or a (make-array ma :element-type 'double-float :initial-element 0d0)))
	  (covar (make-array (list ma ma) 
			     :element-type 'double-float :initial-element 0d0))
	  (covar1 (make-array (list mfit mfit) 
			      :element-type 'double-float :initial-element 0d0))
	  (beta (make-array mfit 
			    :element-type 'double-float :initial-element 0d0))
	  (beta2 (make-array (list mfit 1)
			     :element-type 'double-float :initial-element 0d0))
	  (afunc (make-array ma 
			     :element-type 'double-float :initial-element 0d0))
	  (ym 0d0) (sig2i 0d0) (wt 0d0) (sum 0d0)) 
    

     (declare (type (simple-array double-float (*)) a)) 
     (declare (type (simple-array (unsigned-byte 28) (*)) lista)) 
     (declare (type (simple-array double-float (* *)) covar)) 
     (declare (type (simple-array double-float (* *)) covar1))
     (declare (type (simple-array double-float (*)) beta)) 
     (declare (type (simple-array double-float (* *)) beta2))
     (declare (type (simple-array double-float (*)) afunc)) 
     (declare (type fixnum kk ihit ndata ma ncvm mfit))
     (declare (type double-float ym sig2i wt sum))
     (declare (ignore ncvm))
    
    
     (setf kk (+ mfit 1)) 
     (do ((j 1 (+ j 1)))
	 ((> j ma) t)
       (declare (type fixnum j))
       (setf ihit 0)
       (do ((k 1 (+ k 1)))
	   ((> k mfit) t)
	 (declare (type fixnum k))
	 (if (= (aref lista (1- k)) j) (setf ihit (+ ihit 1))))
      
       (cond 
	 ((= ihit 0) 
	  (setf (aref lista (1-  kk)) j)
	  (setf kk (+ kk 1)))
	 ((> ihit 1)
	  (error " Improper set in lista for lfit ")))) 
    
     (if (not (= kk (1+ ma))) (error " improper set in lista for lfit ")) 
    
     (do ((j 1 (+ j 1)))
	 ((> j mfit) t)
       (declare (type fixnum j))
       (do ((k 1 (+ k 1)))
	   ((> k mfit) t)
	 (declare (type fixnum k))
	 (fset (fref covar j k) 0d0))
       (fset (fref beta j) 0d0))
    
     (do ((i 1 (+ i 1)))
	 ((> i ndata) t)
       (declare (type fixnum i))
       (do ((j 1 (1+ j)))
	   ((> j ma) t)
	 (declare (type fixnum j))
	 (setf (aref afunc (1- j)) 
	       (dfloat (funcall (nth (1- j) funcs) (aref x (1- i))))))
       (setf ym (aref y (1- i)))
      
       (if (< mfit ma)
	   (do ((j (+ mfit 1) (+ j 1)))
	       ((> j ma) t)
	     (declare (type fixnum j))
	     (setf ym (- ym
			 (* (fref a (fref lista j))
			    (fref afunc (fref lista j)))))))
      
       (setf sig2i (/ 1d0 (expt (fref sig i) 2)))
       (do ((j 1 (+ j 1)))
	   ((> j mfit) t)
	 (declare (type fixnum j))
	 (setf wt (* (fref afunc (fref lista j)) sig2i))
	 (do ((k 1 (+ k 1)))
	     ((> k j) t)
	   (setf (aref covar1 (1- j) (1- k)) (+ (fref covar1 j k)
						(* wt (fref afunc (fref lista k))))))
	 (setf (aref beta (1- j)) (+ (fref beta j) (* ym wt))))) 
    
    
     (if (> mfit 1)
	 (do ((j 2 (+ j 1)))
	     ((> j mfit) t)
	   (declare (type fixnum j))
	   (do ((k 1 (+ k 1)))
	       ((> k (1- j)) t)
	     (declare (type fixnum k))
	     (setf (aref covar1 (1- k) (1- j)) (fref covar1 j k)))))
    
     (do ((j 0 (1+ j))) ((> j (1- mfit)) t)
       (setf (aref beta2 j 0) (aref beta j)))
    
     (multiple-value-setq (covar1 beta2) (gaussj covar1 beta2))
    
     (do ((j 0 (1+ j))) ((> j (1- mfit)) t)
       (setf (aref beta j) (aref beta2 j 0)))
    
     (do ((j 1 (+ j 1)))
	 ((> j mfit) t)
       (declare (type fixnum j))
       (setf (aref a (1- (fref lista j))) (fref beta j))) 
    
     (setf chisq 0d0) 
     (do ((i 1 (+ i 1)))
	 ((> i ndata) t)
       (declare (type fixnum i))
       (do ((j 1 (1+ j)))
	   ((> j ma) t)
	 (declare (type fixnum j))
	 (setf (aref afunc (1- j)) 
	       (dfloat (funcall (nth (1- j) funcs) (fref x i)))))
       (setf sum 0d0)
       (do ((j 1 (+ j 1)))
	   ((> j ma) t)
	 (declare (type fixnum j))
	 (setf sum (+ sum (* (fref a j) (fref afunc j)))))
       (setf chisq (+ chisq (expt (/ (- (fref y i) sum) (fref sig i)) 2)))) 
    
    
     (setq covar (covsrt covar1 ma lista mfit)) 
     ;; covar1 is mfitxmfit and lista mfit 
     (return (values a covar chisq))))




(defun lfit-values (y sig xbasis &key mfit lista a)
  "Fit y[i]=c0*xbasis[1,0]+c1*xbasis[1,i]+... where Y is a vector
of double-float and XBASIS is an 2d array of double precision
basis vectors.  SIG is a vector of errors on y, MFIT is the number
of basis functions we are fitting, and LISTA is a 
 (simple-array (unsigned-byte 28) (*)) with the numbers of the
dimensions that are being fit, starting at 1.  For example, if there
are a total of 3 basis functions, and their coefficients 1 and 3 are
being fit, and parameter 2 is being skipped (held constant), then
MFIT=2 and LISTA=#(1 3 2).

Returns (VALUES A COVAR CHISQ) where A is the vector of
coefficients, COVAR is the MFITxMFIT covariance matrix, and CHISQ
is the final chi square.

Note that the keyword argument A containing the weight vector
must be specified and initialized if an incomplete LISTA set is
used - otherwise the weights of the non-fitted parameters are
zero.

Note that the index numbers in LISTA begin at 1, not 0."

  ;;(declare (optimize debug))
  (declare (type (simple-array double-float (*)) y)) 
  (declare (type (simple-array double-float (*)) sig))
  (declare (type (simple-array double-float (* *)) xbasis))
  (declare (type (or null (simple-array double-float (*))) a))
  (declare (type (or null (unsigned-byte 28)) mfit))
  (declare (type (or null (simple-array (unsigned-byte 28) (*))) lista)) 


  (prog* ((chisq 0d0) 
	  (kk 0)
	  (mfit (or mfit (array-dimension xbasis 0)))
	  (lista (or lista
		     (make-array  
		      mfit :element-type '(unsigned-byte 28)
		      :initial-contents   (loop for i from 1 to mfit collect i))))
	  (ihit 0)   
	  (ndata (array-dimension y 0))
	  (ma (array-dimension lista 0))
	  (ncvm ma)
	  (a (or a (make-array ma :element-type 'double-float :initial-element 0d0)))
	  (covar (make-array (list ma ma) 
			     :element-type 'double-float :initial-element 0d0))
	  (covar1 (make-array (list mfit mfit) 
			      :element-type 'double-float :initial-element 0d0))
	  (beta (make-array mfit 
			    :element-type 'double-float :initial-element 0d0))
	  (beta2 (make-array (list mfit 1)
			     :element-type 'double-float :initial-element 0d0))
	  (afunc (make-array ma 
			     :element-type 'double-float :initial-element 0d0))
	  (ym 0d0) (sig2i 0d0) (wt 0d0) (sum 0d0)) 
    
     (declare (type double-float chisq))
     (declare (type (simple-array double-float (*)) a)) 
     (declare (type (simple-array (unsigned-byte 28) (*)) lista)) 
     (declare (type (simple-array double-float (* *)) covar)) 
     (declare (type (simple-array double-float (* *)) covar1))
     (declare (type (simple-array double-float (*)) beta)) 
     (declare (type (simple-array double-float (* *)) beta2))
     (declare (type (simple-array double-float (*)) afunc)) 
     (declare (type (unsigned-byte 28) kk ihit ndata ma ncvm mfit))
     (declare (type double-float ym sig2i wt sum))
     (declare (ignore ncvm))
     (locally
	 (declare (optimize speed))
       
       (setf kk (+ mfit 1)) 
       (do ((j 1 (+ j 1)))
	   ((> j ma) t)
       (declare (type (unsigned-byte 28) j))
	 (setf ihit 0)
	 (do ((k 1 (+ k 1)))
	     ((> k mfit) t)
	   (declare (type (unsigned-byte 28) k))
	   (if (= (aref lista (1- k)) j) (setf ihit (+ ihit 1))))
	 
	 (cond 
	   ((= ihit 0) 
	    (setf (aref lista (1-  kk)) j)
	    (setf kk (+ kk 1)))
	   ((> ihit 1)
	    (error " Improper set in lista for lfit ")))) 
       
       (if (not (= kk (1+ ma))) (error " improper set in lista for lfit ")) 
       
       (do ((j 1 (+ j 1)))
	   ((> j mfit) t)
	 (declare (type (unsigned-byte 28) j))
	 (do ((k 1 (+ k 1)))
	     ((> k mfit) t)
	   (declare (type (unsigned-byte 28) k))
	   (fset (fref covar j k) 0d0))
	 (fset (fref beta j) 0d0))
       
       (do ((i 1 (+ i 1)))
	   ((> i ndata) t)
	 (declare (type (unsigned-byte 28) i))
	 (do ((j 1 (1+ j)))
	     ((> j ma) t)
	   (declare (type (unsigned-byte 28) j))
	   (setf (aref afunc (1- j)) 
		 (aref xbasis (1- j) (1- i))))
	 ;;        (setf (aref afunc (1- j)) 
	 ;;	      (dfloat (funcall (nth (1- j) funcs) (aref x (1- i))))))
	 (setf ym (aref y (1- i)))
	 
	 (if (< mfit ma)
	     (do ((j (+ mfit 1) (+ j 1)))
		 ((> j ma) t)
	       (declare (type (unsigned-byte 28) j))
	       (setf ym (- ym
			   (* (fref a (fref lista j))
			      (fref afunc (fref lista j)))))))
	 
	 (setf sig2i (/ 1d0 (expt (fref sig i) 2)))
	 (do ((j 1 (+ j 1)))
	     ((> j mfit) t)
	   (declare (type (unsigned-byte 28) j))
	   (setf wt (* (fref afunc (fref lista j)) sig2i))
	   (do ((k 1 (+ k 1)))
	       ((> k j) t)
	     (setf (aref covar1 (1- j) (1- k)) (+ (fref covar1 j k)
						  (* wt (fref afunc (fref lista k))))))
	   (setf (aref beta (1- j)) (+ (fref beta j) (* ym wt))))) 
       
       
       (if (> mfit 1)
	   (do ((j 2 (+ j 1)))
	       ((> j mfit) t)
	     (declare (type (unsigned-byte 28) j))
	     (do ((k 1 (+ k 1)))
		 ((> k (1- j)) t)
	       (declare (type (unsigned-byte 28) k))
	       (setf (aref covar1 (1- k) (1- j)) (fref covar1 j k)))))
       
       (do ((j 0 (1+ j))) ((> j (1- mfit)) t)
	 (setf (aref beta2 j 0) (aref beta j)))
       
       (multiple-value-setq (covar1 beta2) (gaussj covar1 beta2))
       
       (do ((j 0 (1+ j))) ((> j (1- mfit)) t)
	 (setf (aref beta j) (aref beta2 j 0)))
       
       (do ((j 1 (+ j 1)))
	   ((> j mfit) t)
	 (declare (type (unsigned-byte 28) j))
	 (setf (aref a (1- (fref lista j))) (fref beta j))) 
       
       (setf chisq 0d0) 
       (do ((i 1 (+ i 1)))
	   ((> i ndata) t)
	 (declare (type (unsigned-byte 28) i))
	 (do ((j 1 (1+ j)))
	     ((> j ma) t)
	   (declare (type (unsigned-byte 28) j))
	   (setf (aref afunc (1- j)) 
		 (aref xbasis (1- j) (1- i))))
	 ;;	(setf (aref afunc (1- j)) 
	 ;;	      (dfloat (funcall (nth (1- j) funcs) (fref x i)))))
	 (setf sum 0d0)
	 (do ((j 1 (+ j 1)))
	     ((> j ma) t)
	   (declare (type (unsigned-byte 28) j))
	   (setf sum (+ sum (* (fref a j) (fref afunc j)))))
	 (setf chisq (+ chisq (expt (/ (- (fref y i) sum) (fref sig i)) 2)))) 
       
       
       (setq covar (covsrt covar1 ma lista mfit))) 
     ;; covar1 is mfitxmfit and lista mfit 
     (return (values a covar chisq))))

;-------------------------------------------------------------------------------

(defun covsrt (covar1 ma lista mfit)
  (declare (type (simple-array double-float (* *)) covar1)) 
  (declare (type (simple-array (unsigned-byte 28) (*)) lista)) 
  (declare (type (unsigned-byte 28) ma mfit))


  (prog ((ncvm 0)  
	 (covar (make-array (list ma ma) :element-type 'double-float
			    :initial-element 0d0)) 
	 (swap 0d0))
     (declare (type (unsigned-byte 28) ncvm))
     (declare (type double-float swap))
     (declare (type (simple-array double-float (* *)) covar))
     (declare (ignore ncvm))
     (declare (optimize speed))

     (do ((i 0 (1+ i)))
	 ((> i (1- mfit)) t)
       (declare (type (unsigned-byte 28) i))
       (do ((j 0 (1+ j)))
	   ((> j (1- mfit)) t)
	 (declare (type (unsigned-byte 28) j))
	 (setf (aref covar i j) (aref covar1 i j))))

     (do ((j 1 (1+ j)))
	 ((> j (1- ma)) t)
       (declare (type (unsigned-byte 28) j))
       (do ((i (1+ j) (1+ i)))
	   ((> i ma) t)
	 (declare (type (unsigned-byte 28) i))
	 (fset (fref covar i j) 0d0)))


     (do ((i 1 (+ i 1)))
	 ((> i (1- mfit)) t)
       (declare (type (unsigned-byte 28) i))
       (do ((j (1+ i) (+ j 1)))
	   ((> j mfit) t)
	 (declare (type (unsigned-byte 28) j))
	 (if (> (ifref lista j) (ifref lista i)) 
	     (setf (aref covar (1- (ifref lista j)) (1- (ifref lista i))) 
		   (fref covar i j))
	     (setf (aref covar (1- (ifref lista i)) (1- (ifref lista j))) 
		   (fref covar i j)))))

     (setf swap (fref covar 1 1)) 

     (do ((j 1 (+ j 1)))
	 ((> j ma) t)
       (declare (type (unsigned-byte 28) j))
       (setf (aref covar 0 (1- j)) (fref covar j j))
       (setf (aref covar (1- j) (1- j)) 0d0)) 

     (fset (fref covar (ifref lista 1) (ifref lista 1)) swap) 
     (do ((j 2 (+ j 1)))
	 ((> j mfit) t)
       (declare (type (unsigned-byte 28) j))
       (setf (aref covar (1- (ifref lista j)) (1- (ifref lista j))) 
	     (fref covar 1 j))) 

     (do ((j 2 (+ j 1)))
	 ((> j ma) t)
       (declare (type (unsigned-byte 28) j))
       (do ((i 1 (+ i 1)))
	   ((> i (1- j)) t)
	 (declare (type (unsigned-byte 28) i))
	 (setf (aref covar (1- i) (1- j)) (fref covar j i)))) 

     (return covar)))




(defun gaussj (a b)
  (declare (type (simple-array double-float (* *)) a)) 
  (declare (type (simple-array double-float (* *)) b))
  (declare (optimize speed))


					;(format t "A=~A~%" a)
					;(format t "B=~A~%" b)

  (prog* (
	  (n (array-dimension a 0))
	  (m (array-dimension b 1))
	  (ipiv (make-array n :element-type '(unsigned-byte 28) :initial-element 0))
	  (indxr (make-array n :element-type '(unsigned-byte 28) :initial-element 0))
	  (indxc (make-array n :element-type '(unsigned-byte 28) :initial-element 0))
	  (irow 0) (icol 0) (dum 0d0) (pivinv 0d0) (big 0d0))


     (declare (type (simple-array (unsigned-byte 28) (*)) ipiv)) 
     (declare (type (simple-array (unsigned-byte 28) (*)) indxr)) 
     (declare (type (simple-array (unsigned-byte 28) (*)) indxc)) 
     (declare (type (unsigned-byte 28) n m irow icol))
     (declare (type double-float dum pivinv big))

     (do ((j 1 (+ j 1)))
	 ((> j n) t)
       (declare (type (unsigned-byte 28) j))
       (fset (ifref ipiv j) 0)) 


     (do ((i 1 (+ i 1)))
	 ((> i n) t)
       (declare (type (unsigned-byte 28) i))
       (setf big 0d0)
       (do ((j 1 (+ j 1)))
	   ((> j n) t)
	 (declare (type (unsigned-byte 28) j))
	 (when 
	     (not (= (ifref ipiv j) 1))
	   (do ((k 1 (+ k 1)))
	       ((> k n) t)
	     (declare (type (unsigned-byte 28) k))
	     (cond 
	       ((= (ifref ipiv k) 0) 
		(when 
		    (>= (abs (fref a j k)) big)
		  (setf big (abs (fref a j k))) 
		  (setf irow j) 
		  (setf icol k)))
	       ((> (ifref ipiv k) 1) 
		(error " Singular matrix in gaussj"))))))
 
       (fset (ifref ipiv icol)  (1+ (the (unsigned-byte 28) (ifref ipiv icol))))
       (when 
	   (not (= irow icol))
	 (do ((l 1 (+ l 1)))
	     ((> l n) t)
	   (declare (type (unsigned-byte 28) l))
	   (setf dum (fref a irow l))
	   (fset (fref a irow l) (fref a icol l))
	   (fset (fref a icol l) dum))

	 (do ((l 1 (+ l 1)))
	     ((> l m) t)
	   (declare (type (unsigned-byte 28) l))
	   (setf dum (fref b irow l))
	   (fset (fref b irow l) (fref b icol l))
	   (fset (fref b icol l) dum)))

       (fset (ifref indxr i) irow)
       (fset (ifref indxc i) icol)

       (if (= (fref a icol icol) 0d0) 
	   (error " singular matrix in gaussj "))
       (setf pivinv (/ 1d0 (fref a icol icol)))
       (fset (fref a icol icol) 1d0)

       (do ((l 1 (+ l 1)))
	   ((> l n) t)
	 (declare (type (unsigned-byte 28) l))
	 (fset (fref a icol l) (* (fref a icol l) pivinv)))

       (do ((l 1 (+ l 1)))
	   ((> l m) t)
	 (declare (type (unsigned-byte 28) l))
	 (fset (fref b icol l) (* (fref b icol l) pivinv)))

       (do ((ll 1 (+ ll 1)))
	   ((> ll n) t)
	 (declare (type (unsigned-byte 28) ll))
	 (when 
	     (not (= ll icol)) 
	   (setf dum (fref a ll icol)) (fset (fref a ll icol) 0d0)
	   (do ((l 1 (+ l 1)))
	       ((> l n) t)
	     (declare (type (unsigned-byte 28) l))
	     (fset (fref a ll l) (- (fref a ll l) (* (fref a icol l) dum))))
	   (do ((l 1 (+ l 1)))
	       ((> l m) t)
	     (declare (type (unsigned-byte 28) l))
	     (fset (fref b ll l) 
		   (- (fref b ll l) (* (fref b icol l) dum)))))))

     (do ((l n (+ l (- 1))))
	 ((< l 1) t)
       (declare (type (unsigned-byte 28) l))
       (when 
	   (not (= (ifref indxr l) (ifref indxc l)))
	 (do ((k 1 (+ k 1)))
	     ((> k n) t)
	   (declare (type (unsigned-byte 28) k))
	   (setf dum (fref a k (ifref indxr l)))
	   (fset (fref a k (fref indxr l)) (fref a k (fref indxc l)))
	   (fset (fref a k (fref indxc l)) dum)))) 
   
     (return (values a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a simple common case
(defun lfit-polynomial (y x n &key (sig nil) mfit lista a)
  "Fit a N degree polynomial Y=C0+C1*X+...+CnX^n.  SIG is the error
bar on Y, or 1 by default.  Return (VALUES A COVAR CHISQ) where A is
the vector of coefficients.

MFIT and LISTA are for restricting which dimensions are fit, as
described in documentation for LFIT-VALUES, and A is the optional
input/output if some dimensions are fixed"
  (let* ((y (map '(simple-array double-float (*)) (lambda (z) (float z 1d0)) y))
	 (x (map '(simple-array double-float (*)) (lambda (z) (float z 1d0)) x))
	 (np (1+ n)) 
	 (ny (length y))
	 (sig (if sig
		  (map '(simple-array double-float (*)) (lambda (z) (float z 1d0)) sig)
		  (make-array ny :element-type 'double-float :initial-element 1d0)))
	 (xbasis (make-array (list np ny)  :element-type 'double-float)))
    (declare (type (simple-array double-float (*)) x y sig)
	     (type (simple-array double-float (* *)) xbasis))
    (loop for iy below ny
	  for xx of-type double-float across x
	  do
	  (loop for ipoly below np
		do (setf (aref xbasis ipoly iy) (expt xx ipoly))))
    (lfit-values y sig xbasis :mfit mfit :lista lista :a a)))




