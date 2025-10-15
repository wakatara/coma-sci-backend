
(defpackage #:small-body-identify/test
  (:nicknames sbid/test)
  (:use #:cl)
  (:export
   ;; a function that builds a SBID-TEST-SNAP with exact positions, and compares it to
   ;; result of using SBID
   #:run-test

  ))

(in-package sbid/test)

;; a snapshot that is exact at mjd 
(defstruct sbid-test-snap
  mjdtt
  ra-vec
  dec-vec
  ok-vec
  observatory)

(defun build-sbid-test-snap (mjdtt &key (observatory "MKO") (logstream t))
  (let* ((n (length sbid::*orbit-element-vector*))
	 (ra-vec  (make-array n :element-type 'double-float))
	 (dec-vec (make-array n :element-type 'double-float))
	 (ok-vec  (make-array n)) ;; NIL if this was a bad point
	 (sbid-test-snap (make-sbid-test-snap
			  :mjdtt mjdtt :ra-vec ra-vec :dec-vec dec-vec
			  :ok-vec ok-vec
			  :observatory observatory)))
    (format logstream "Computing exact ra,dec snapshot at MJD=~,6F~%" mjdtt)
    (loop for i below n
	  when (zerop (rem i 20000))
	    do (format logstream "   Computed ~A/~A orbits~%" i n)
	  do (multiple-value-bind (ra dec)
		 (ignore-errors
		  (slalib-ephem:compute-radecr-from-comet-elem-for-observatory
		   (aref sbid::*orbit-element-vector* i) mjdtt observatory
		   :perturb t
		   :correct-mjd-to-tt NIL))
	       (if ra
		   (progn
		     (setf (aref ra-vec i) ra)
		     (setf (aref dec-vec i) dec)
		     (setf (aref ok-vec i) t))
		   (setf (aref ok-vec i) nil))))
    sbid-test-snap))

;; brute force search, returning '((i dist-i) ..) for orbit i
(defun find-list-near-ra-dec-in-test-snap (test-snap ra0 dec0 &key (dist 100.0))
  (sort 
   (loop 
     for i below (length sbid::*orbit-element-vector*)
     for ra across (sbid-test-snap-ra-vec test-snap)
     for dec across (sbid-test-snap-dec-vec test-snap)
     for ok across (sbid-test-snap-ok-vec test-snap)
     for d = (if ok (astro-coords:sky-angle ra dec ra0 dec0 :units :arcsec))
     when (and d (<= d dist))
       collect (list i d))
   '< :key 'second))

;; run ntest tests in which we generate points dmjd-tt away from the mjd
;; of snap-test, and see if we get the same result from exact solution

;; variables to save a failure in
(defvar *the-bruteforce-list* nil)
(defvar *the-sbid-list* nil)
(defvar *snap* nil)
(defvar *ra0* nil)
(defvar *dec0* nil)
;;
(defun run-test-for-one-snap (test-snap &key (ntest 10000) (dmjd-tt 0.5d0) (dist 300d0)
					(logstream t) (caching nil))
  (let ((mem (sbid:make-sbid-mem)))
    (loop with mjdtt = (sbid-test-snap-mjdtt test-snap)
	  with observatory = (sbid-test-snap-observatory test-snap)
	    initially ;; set up a snap inside mem at mjdtt + dmjd-tt
		      (sbid:generate-candidates 1d0 2d0 (+ dmjd-tt mjdtt) mem)
	  for i below ntest
	  for ecliptic-lon = (random 360d0)
	  for ecliptic-lat = (+ -5d0 (random 10d0))
	  do
	     (multiple-value-bind (ra0 dec0)
		 (astro-coords:ecliptic-to-equatorial  ecliptic-lon ecliptic-lat)
	       (let* ((bruteforce-list (find-list-near-ra-dec-in-test-snap
					test-snap ra0 dec0 :dist dist))

		      (sbid-cand-list
			(if caching
			    (sbid:generate-candidates-with-caching
			     ra0 dec0 mjdtt mem :max-distance dist
						:observatory observatory)
			    (sbid:generate-candidates 
			     ra0 dec0 mjdtt mem :max-distance dist
						:observatory observatory)))

		      (sbid-list
			(mapcar (lambda (cand) 
				  (list (sbid:candidate-index  cand)
					(sbid:candidate-dist cand)))
				sbid-cand-list)))
		 (format logstream "i=~A  ra0=~,3F dec0=~,3F Nbrute=~A  Ns=~A~%"
			 i ra0 dec0
			 (length bruteforce-list) (length sbid-list))
		 (when (not (equalp
			     ;; sort returned obect indices because a
			     ;; rare but harmless failure mode is that
			     ;; two object distances are almost equal, but
			     ;; sorted differently for the two methods.  So
			     ;; look at only objects returned, not their order
			     (sort (mapcar 'first bruteforce-list) '<)
			     (sort (mapcar 'first sbid-list) '<)))
		   (setf *the-bruteforce-list* bruteforce-list) ;; record the bad one
		   (setf *the-sbid-list* sbid-list)
		   (setf *snap* (first (sbid::sbid-mem-sbid-snap-list mem)))
		   (setf *ra0* ra0
			 *dec0* dec0)
		   (error "Mismatch! brute=~A sbid=~A" bruteforce-list sbid-list)))))))
		 
				  
  
(defvar *test-snap* nil)


;; can call when test fails
(defun autopsy ()
  (let* ((missing-index
	   (loop for thing in *the-bruteforce-list*
		 when (not (find (car thing) *the-sbid-list* :key 'car))
		   do (return (car thing))))
	 (elem
	   (aref sbid::*orbit-element-vector* missing-index))
	 (ra-exact (aref (sbid-test-snap-ra-vec *test-snap*) missing-index))
	 (dec-exact (aref (sbid-test-snap-dec-vec *test-snap*) missing-index)))
    elem))
    
    

;; the mjd we put our *test-snap* at, close to the mjd of astorb for speed of
;; initial computation
(defparameter *mjd0* (astro-time:parse-ut-date-and-time-string-to-mjd "2020-08-01T00:00:00"))

(defun run-test (&key (mjd0 *mjd0*) (ntest 10000) (dmjd-tt 0.5d0) (dist 600d0)
		      (logstream t) (caching t))
  (setf *test-snap* (build-sbid-test-snap mjd0 :logstream logstream))
  (run-test-for-one-snap *test-snap* :dmjd-tt dmjd-tt :ntest ntest :dist dist
				     :logstream logstream :caching caching))
