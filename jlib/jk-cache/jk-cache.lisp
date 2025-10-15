
#|

Cache items using an EQUALP hash, with automatic purging every PURGE-INTERVAL,
and purging by optional EXPIRE-TIME, so that elements time out.

Note that PURGE-INTERVAL is really not needed, because it is purged
at NMAX anyway, but it provides a way to regularly remove old (expired) entries.

(defparameter *cache* (jk-cache:build-cache :nmax 10 :purge-fraction 0.5
                                            :expire-time 60))
(jk-cache:cache-item 'foo "this thing" *cache*)
(jk-cache:cache-item 'bar "that thing" *cache*)
(jk-cache:retrieve-item 'bar *cache*)
(jk-cache:purge-cache *cache*)) ;; also auto-purges

;; insert 12
(loop for i below 12
      do (jk-cache:cache-item i (format nil "~A" i) *cache*))

;; note that we cleared 50% once we hit 11, taking it down to 5,
;; and then we added one more
(jk-cache:list-entries *cache* :ignore-expired t)
   ==>  ((11 . "11") (6 . "6") (7 . "7") (8 . "8") (9 . "9") (10 . "10"))

(sleep 60)  ;; wait for expiration time
(jk-cache:list-entries *cache* :ignore-expired t)
  ==> NIL


|#

(defpackage jk-cache
  (:use #:cl)
  (:export
   #:cache #:cache-p
   #:build-cache
   #:purge-cache
   #:clear-cache
   #:cache-item
   #:retrieve-item
   #:remove-item
   #:list-entries
   ))

(in-package jk-cache)

(defstruct cache-element
  insert-time  ;; ut of insertion
  insert-i     ;; order in which it was inserted  
  item)

(defclass cache ()
    ((lock  :initarg :lock
	    :initform (bordeaux-threads:make-lock "jk-cache-lock")
	    :accessor cache-lock)
     (i     :initform 0        ;; current ordinal number of thing being cached
	    :accessor cache-i)
     (expire-time :initarg :expire-time ;; seconds to expiration
		  :initform nil
		  :accessor cache-expire-time)
     (nmax      :initarg :nmax ;; maximum elements
		:initform 10000
		:accessor cache-nmax)
     (hash      :initform (make-hash-table :test 'equalp)
		:reader cache-hash)
     (purge-fraction :initarg :purge-fraction  ;; fraction to purge each time
		     :initform 0.2
		     :reader cache-purge-fraction)
     (i-since-purge  :initform 0    ;; how many insertions since purge
		     :accessor cache-i-since-purge)
     (purge-interval :initarg :purge-interval ;; purge this many insertions 
		     :initform 10000
		     :accessor cache-purge-interval)))
   
  

(defmacro with-cache-lock (cache &body body)
  `(bordeaux-threads:with-lock-held ((cache-lock ,cache))
     ,@body))

    

(defun build-cache (&key (expire-time nil)
		      (nmax 10000)
		      (purge-fraction 0.2)
		      (purge-interval 10000))
  (make-instance 'cache
		 :expire-time expire-time
		 :nmax nmax
		 :purge-fraction purge-fraction
		 :purge-interval purge-interval))
 
(defun %purge-cache (cache)
  (declare (type cache cache))
  (let ((ut (get-universal-time))
	(et (cache-expire-time cache))
	(hash (cache-hash cache)))
    ;; expire elements older than expire-time
    (when et   
      (loop for key being the hash-key of hash
	    for elem being the hash-value of hash
	    when (> (- ut (cache-element-insert-time elem)) et)
	      do (remhash key hash)))
    ;; if table is too big, filter out the old ones
    (when (> (hash-table-count hash) (cache-nmax cache))
      (let* ((ntot (hash-table-count hash))
	     (ndel (round (* (cache-purge-fraction cache) ntot)))
	     (key-i-pairs ;; ((KEY . i-insert) ... ) in order of insertion
	       (sort 
		(loop for key being the hash-key of hash
		      for elem being the hash-value of hash
		      collect (cons key (cache-element-insert-i elem)))
		'< :key 'cdr)))
	(loop for j below ndel
	      for (key)  in key-i-pairs
	      do (remhash key hash))))))

;; cache if we've had too many insertions, or 
(defun %maybe-purge-cache  (cache)
  (declare (type cache cache))
  (when (or (> (cache-i-since-purge cache)
	       (cache-purge-interval cache))
	    (> (hash-table-count (cache-hash cache))
	       (cache-nmax cache)))
    (setf (cache-i-since-purge cache) 0)
    (%purge-cache cache)))

(defmethod purge-cache ((cache cache))
  "Purge the cache, deleting items older than CACHE-EXPIRE-TIME
and deleting PURGE-FRACTION if number of items is more than CACHE-NMAX"
  (declare (type cache cache))
  (with-cache-lock cache
    (%purge-cache cache)))

(defmethod clear-cache ((cache cache))
  "Completely empty cache"
  (declare (type cache cache))
  (with-cache-lock cache
    (clrhash (cache-hash cache))
    (setf (cache-i-since-purge cache) 0)
    cache))
  
 
(defmethod cache-item (key item (cache cache))
  "Insert ITEM into CACHE by KEY (using EQUALP)"
  (declare (type cache cache))
  (with-cache-lock cache
    (incf (cache-i-since-purge cache))
    (%maybe-purge-cache cache)
    (incf (cache-i cache))
    (let ((elem (make-cache-element
		 :insert-time (get-universal-time)
		 :insert-i (cache-i cache)
		 :item item)))
      (setf (gethash key (cache-hash cache)) elem))
    item))

(defmethod retrieve-item (key (cache cache))
  "Retrieve an item from CACHE using KEY.
If the item is expired, don't return it, but delete it."
  (declare (type cache cache))
  (with-cache-lock cache
    (let ((elem 
	    (gethash key (cache-hash cache))))
      (when elem
	(if (and  ;; don't return expired item, but delete it
	     (cache-expire-time cache)
	     (> (- (get-universal-time) (cache-element-insert-time elem))
		(cache-expire-time cache)))
	    (progn
	      (remhash key (cache-hash cache))
	      nil)
	    (cache-element-item elem))))))
	    
			  

(defmethod remove-item (key (cache cache))
  "Remove an item with KEY in CACHE."
  (with-cache-lock cache
    (remhash key (cache-hash cache)))) 
    
    
(defmethod list-entries ((cache cache) &key (ignore-expired t))
  "List the entries in a cache as cons list (KEY . VAL)"
  (with-cache-lock cache
    (let ((ut (get-universal-time))
	  (et (cache-expire-time cache))
	  (hash (cache-hash cache)))
      ;; expire elements older than expire-time
      (when et   
	(loop for key being the hash-key of hash
	      for elem being the hash-value of hash
	      when (or (not ignore-expired)
		       (<= (- ut (cache-element-insert-time elem)) et))
		collect (cons key (cache-element-item elem)))))))
  
