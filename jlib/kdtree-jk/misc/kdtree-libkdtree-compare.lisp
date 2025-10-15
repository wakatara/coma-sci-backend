(eval-when (:load-toplevel)
  (asdf:load-system "libkdtree")
  (asdf:load-system "kdtree-jk"))


#|
  
conclusion:

  libkdtree:  13s
  kdtree-jk:   18.6s
  kdtree-jk with defer: 10.4s

Conclude: the 'defer' balancing makes it faster, not faster insertion,
because most time is spent in accessing the tree, not building.


|#

(defun make-dblvec (&rest vals)
  (map '(simple-array double-float (*))
       (lambda (x) (float x 1d0))
       vals))

(defun make-random-dblvec (&key (ndim 3))
  (let ((v (make-array ndim :element-type 'double-float)))
    (loop for i below ndim
	  do (setf (aref v i) (random 100d0)))
    v))

(defparameter *dataset*
  (loop for i below 100000
	collect (make-random-dblvec)))

(defparameter *target-vec* (make-dblvec 30 30 30))
(defparameter *radius* 10d0)
(defparameter *niter* 100000)

(defun test-libkdtree (&key (niter *niter*))
  (let ((kdtree (libkdtree:build-kdtree 3)))
    (loop for v in *dataset*
	  for i from 0
	  do (libkdtree:kdtree-insert kdtree v i))
    (dotimes (i niter)
      (let ((kdresult
	      (libkdtree:kdtree-get-points-within-dist
	       kdtree *target-vec* *radius*)))
	(libkdtree:kdresult-free kdresult)))))
	    
(defun test-kdtree-jk  (&key (niter *niter*) (defer nil) (npoints 1000))
  (let ((kdtree (kdtree-jk:build-kdtree 3 :npoints npoints))
	(kdresult (kdtree-jk:build-kdresult)))
      (loop for v in *dataset*
	  for i from 0
	    do (kdtree-jk:insert-vector  kdtree v i :defer defer))
    (when defer (kdtree-jk:balance-kdtree kdtree))
    (dotimes (i niter)
      (let ((kdresult
	      (kdtree-jk:kd-search-in-radius kdtree *target-vec* *radius*
					     :kdresult kdresult)))
	(declare (ignore kdresult))))))

