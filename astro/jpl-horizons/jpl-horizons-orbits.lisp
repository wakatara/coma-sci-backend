

(in-package jpl-horizons)

;; 
(defparameter *jpl-horizons-orbit-form-page*
  "https://ssd.jpl.nasa.gov/api/horizons.api" ;; changed 2021xs
  #+nil "https://ssd.jpl.nasa.gov/horizons_batch.cgi")

;; get the JPL object name that is the center field of the 2nd line
(defun %extract-object-name (jpl-string)
  (let ((n (search "JPL/HORIZONS" jpl-string)))
    (when n
      (string-trim '(#\space #\tab)
		   (subseq jpl-string (+ n 12) (+ n 58))))))
 

;; patch up the orbital orbital using values from $$SOE...$$EOE block
;; which has elements for latest MJD
#+nil ;; no longer need this 
(defun  %update-jpl-elements-with-$$-block (text elem)
  (declare (type string text)
	   (type orbital-elements:comet-elem elem))
  (let* ((n1 (+ 6 (search "$$SOE" text)))
	 (n2 (search "$$EOE" text))
	 (block
	     (if (not (and n1 n2))
		 (error "$$SOE...$$EOE block not found in JPL elements text")
		 (concatenate 'string "EPOCH= " (subseq text n1 n2))))
	 (elem2
	   (orbital-elements-parse:parse-jpl-elem-string block)))
    (setf (orbital-elements:comet-elem-epoch elem)
	  (orbital-elements:comet-elem-epoch elem2))
    (setf (orbital-elements:comet-elem-time-peri elem)
	  (orbital-elements:comet-elem-time-peri elem2))
    (setf (orbital-elements:comet-elem-orbinc elem)
	  (orbital-elements:comet-elem-orbinc elem2))
    (setf (orbital-elements:comet-elem-anode elem)
	  (orbital-elements:comet-elem-anode elem2))
    (setf (orbital-elements:comet-elem-perih elem)
	  (orbital-elements:comet-elem-perih elem2))
    (setf (orbital-elements:comet-elem-q elem)
	  (orbital-elements:comet-elem-q elem2))
    (setf (orbital-elements:comet-elem-e elem)
	  (orbital-elements:comet-elem-e elem2))
    elem))
	  
	  
    
    
    
    
    
    

(defun get-jpl-horizons-elements (object-name
				  &key
				    (parse-elements t)
				    (resolve-ambiguous-object t)
				    (return-all-elements nil)
				    (mjd nil)
				    (use-file-locking t)
				    (file-locking-timeout 60)
				    (use-precessed-to-mjd t)
				    (eph-center "@sun") ;; heliocentric
				    (ntries 1))
  
  "Try to get the orbital elements of OBJECT-NAME as
  ORBITAL-ELEMENTS:COMET-ELEM, returning
 (VALUES NIL-OR-ELEMENTS JPL-HORIZONS-ELEMENT-TEXT ERROR).  If
  PARSE-ELEMENTS is NIL then NIL-OR-ELEMENTS is NIL and just the
  JPL-HORIZONS-ELEMENT-TEXT is returned.

If RESOLVE-AMBIGUOUS-OBJECT a request that returns several possible
objects is retried for the LATEST object if MJD is NIL, or the year
for which YYYY-MM-DD is closest to MJD.

RETURN-ALL-ELEMENTS means to return a list of all elements returned
by the JPL resolver instead of picking closest year, as (eg)
   ( ((\"900857\" 2006 \"101P\" \"101P            Chernykh\")
     #S(orbital-elements:comet-elem ...))
     ...)
where 900857 is the JPL internal designation, and 2006 is the epoch
year, and the two other fieds are MATCH_DESIG and PRIMARY_DESIGN+NAME.
All returned elements will be at the MJD epoch, however.

USE-PRECESSED-TO-MJD (T by default) uses the main block for object
information and non-gravs, but then parses the $SOE .. $EOE block for
the orbital elements precessed to the exact MJD given."

  (when (not mjd)
    (setf mjd (astro-time:ut-to-mjd (get-universal-time))))

  (let* ((lockfile *jpl-lockfile*)
	 (dont-file-lock (not use-file-locking)))
  
  (multiple-value-bind (text/nil returncode/err)
      (ignore-errors
       

       ;; limit simultaneous access from this machine to prevent JPL
       ;; lockout
       (with-jpl-locking (:jpl-lockfile
			  lockfile
			  :lockfile-timeout file-locking-timeout
			  :hard-timeout 180
			  :dont-file-lock dont-file-lock)
	 (sleep 1)
	 (drakma:http-request ;; note that this returns KM units by default
	  *jpl-horizons-orbit-form-page* 
	  :method :post
	  :parameters
	  `(;;("batch" . "1")
	    ("format" . "text")	;; the json format is pretty useless - just 2 fields
	    ;; COMMAND='objectname'
	    ("COMMAND" . ,(format nil "'~A'"
				  (fix-jpl-object-name object-name)))
	    ("CENTER" . ,eph-center)
	    ("TABLE_TYPE" . "ELEMENTS")
	    ("START_TIME" . ,(%mjd-to-jpl-ut mjd))
	    ("STOP_TIME" .  ,(%mjd-to-jpl-ut (+ mjd 0.1)))
	    ("TLIST"  . ,(%mjd-to-jpl-ut mjd))
	    ("STEP_SIZE" . "1d") ;; limit it to one element
	    ("CSV_FORMAT" . "NO")
	    ("OBJ_DATA"   . "YES") ;; don't need this
	    ("MAKE_EPHEM" . "YES")))))
    (loop
      for itry from 1
      for ambiguous-resolver-result = nil
      for done-tries = (= itry ntries)
      do
	 (assert (<= itry ntries)) ;; paranoid
	 ;; this is ugly because every block has a return
	 (cond
	   ;; failed in drakma
	   ((not text/nil)
	    (when done-tries
	      (return (values nil text/nil returncode/err))))
	   ;;
	   ;; invalid result
	   ((or (search "No matches found" text/nil)
		(search "ERROR" text/nil))
	    (when done-tries
	      (return (values nil text/nil nil))))
	   ;;
	   ;; returning all elements, not just closest to MJD
	   ((and return-all-elements
		 (jpl-response-has-multiple-bodies text/nil))
	    (let ((all-matches (parse-resolver-jpl-response text/nil
							    :desig object-name)))
	      (return
		(loop for match in all-matches
		      for jpl-id = (first match)
		      for elem =  (get-jpl-horizons-elements
				   jpl-id 
				   :parse-elements parse-elements
				   :resolve-ambiguous-object nil
				   :return-all-elements nil
				   :mjd mjd
				   :use-file-locking use-file-locking)
		      when elem
			collect (list match elem)))))
	   ;;
	   ;; ambiguous object
	   ((and resolve-ambiguous-object
		 (setf ambiguous-resolver-result ;; kludgy
		       (resolve-ambiguous-jpl-horizons-id text/nil
							  :desig object-name
							  :mjd mjd)))
	    (return
	      (get-jpl-horizons-elements
	       ambiguous-resolver-result
	       :parse-elements parse-elements
	       :resolve-ambiguous-object nil
	       :mjd mjd
	       :use-file-locking nil))) ;; our first file-lock is exited
	   
	   ((not parse-elements)
	    (when done-tries
	      (return (values nil text/nil nil))))
	   (t
	    ;;(print text/nil)
	    (multiple-value-bind (elements parsing-error)
		(ignore-errors  (orbital-elements-parse:parse-jpl-elem-string
				 text/nil
				 :id object-name
				 :use-soe-eoe-block (if use-precessed-to-mjd
							:mandatory
							nil)
				 :object-desc-name (%extract-object-name text/nil)))
	      (cond ((not elements) ;; we had an error
		     (when done-tries
		       (return (values nil text/nil  parsing-error))))
		    (t
		     ;; set the name we used to download it, so we know we
		     ;; can dowload again
		     (setf (slalib-ephem:comet-elem-id elements) object-name)
		     (return (values elements text/nil nil)))))))
	 (sleep 4))))) ;; between tries



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; using *jpl-horizons-orbit-form-page*
#+nil
(defun get-jpl-horizons-elements (object-name
				  &key
				    (parse-elements t)
				    (resolve-ambiguous-object t)
				    (mjd nil)
				    (use-file-locking t)
				    (ntries 1))
  
  "Try to get the orbital elements of OBJECT-NAME as
  ORBITAL-ELEMENTS:COMET-ELEM, returning
 (VALUES NIL-OR-ELEMENTS JPL-HORIZONS-ELEMENT-TEXT ERROR).  If
  PARSE-ELEMENTS is NIL then NIL-OR-ELEMENTS is NIL and just the
  JPL-HORIZONS-ELEMENT-TEXT is returned.

If RESOLVE-AMBIGUOUS-OBJECT a request that returns several possible
objects is retried for the LATEST object if MJD is NIL, or the year
for which YYYY-06-01 is closest to MJD."
  (multiple-value-bind (text/nil returncode/err)
      (ignore-errors
       ;; limit simultaneous access from this machine to prevent JPL
       ;; lockout
       (file-io:with-lock-file
	   ((format nil "~A/.JPL_HORIZONS_LISP_LOCKFILE"
		    file-io:*home-directory*)
	    :timeout 60
	    :on-timeout :grab
	    :dont-lock (not use-file-locking))
	 (sleep 1)
	 (drakma:http-request
	  *jpl-horizons-orbit-form-page* 
	  :method :post
	  :parameters
	  `(("batch" . "1")
	    ;; COMMAND='objectname"'
	    ("COMMAND" . ,(format nil "'~A;'" (fix-jpl-object-name object-name)))
	    ;; limiting the epoch seems to do nothing
	    ;;("EPOCH" . ,(format nil "~A"
	    ;;		    (astro-time:mjd-to-jd
	    ;;		     (or mjd (astro-time:ut-to-mjd (get-universal-time))))))
	    ("MAKE_EPHEM" . "NO")))))

    (loop
      for itry from 1
      for done-tries = (= itry ntries)
      do
	 (assert (<= itry ntries)) ;; paranoid
	 ;; this is ugly because every block has a return
	 (cond
	   ;; failed in drakma
	   ((not text/nil)
	    (when done-tries
	      (return (values nil text/nil returncode/err))))
	   ;; ambiguous object
	   ((and resolve-ambiguous-object
		 (resolve-ambiguous-jpl-horizons-id text/nil))
	    (return
	      (get-jpl-horizons-elements
	       (resolve-ambiguous-jpl-horizons-id text/nil :mjd mjd)
	       :parse-elements parse-elements
	       :resolve-ambiguous-object nil
	       :use-file-locking nil))) ;; don't file-lock when recursing
	   ;; invalid result
	   ((or (search "No matches found" text/nil)
		(search "ERROR" text/nil))
	    (when done-tries
	      (return (values nil text/nil nil))))
	   ((not parse-elements)
	    (when done-tries
	      (return (values nil text/nil nil))))
	   (t
	    (multiple-value-bind (elements parsing-error)
		(ignore-errors  (orbital-elements-parse:parse-jpl-elem-string
				 text/nil
				 :id object-name
				 :object-desc-name (%extract-object-name text/nil)))
	      (cond ((not elements) ;; we had an error
		     (when done-tries
		       (return (values nil text/nil  parsing-error))))
		    (t
		     ;; set the name we used to download it, so we know we
		     ;; can dowload again
		     (setf (slalib-ephem:comet-elem-id elements) object-name)
		     (return (values elements text/nil nil)))))))
	 (sleep 4)))) ;; between tries






(defclass jpl-orbit-cache (jk-cache:cache)
  ((day-span :initarg :day-span :initform 10 :accessor jpl-orbit-cache-day-span)))

(defun build-jpl-orbit-cache (&key
				(day-span 10)
				(expire-days 10)
				(nmax 10000))
  "Build a JPL orbit cache with orbits valid for DAY-SPAN, and expiring in
EXPIRE-DAYS, with NMAX entries."
  (make-instance 'jpl-orbit-cache
		 :day-span    day-span
		 :expire-time (* 24 3600 expire-days)
		 :nmax nmax))

(defvar *default-jpl-orbit-cache* (build-jpl-orbit-cache))


(defmethod retrieve-cached-orbit (object-name mjd
				  &key (jpl-orbit-cache *default-jpl-orbit-cache*))
  (let ((key (list object-name (round (/ mjd (jpl-orbit-cache-day-span jpl-orbit-cache))))))
    (jk-cache:retrieve-item key jpl-orbit-cache)))

;; below, RESULTS is
;; (list nil-or-elements jpl-horizons-element-text error)
(defmethod save-orbit-to-cache  (object-name mjd results
				 &key (jpl-orbit-cache *default-jpl-orbit-cache*))
  (let ((key (list object-name (round (/ mjd (jpl-orbit-cache-day-span jpl-orbit-cache))))))
    (jk-cache:cache-item key results jpl-orbit-cache)))




    


(defun get-jpl-horizons-elements-with-caching
    (object-name
     &key
       (mjd nil)
       (sleep-time 0)
       (jpl-orbit-cache *default-jpl-orbit-cache*)
       (ntries 1))
							     
  "Try to get the orbital elements of OBJECT-NAME as
  ORBITAL-ELEMETS:COMET-ELEM, returning
 (VALUES NIL-OR-ELEMENTS  JPL-HORIZONS-ELEMENT-TEXT ERROR).  

Unlike GET-JPL-HORIZONS-ELEMENTS, this will cache the elements in a hash table.

If MJD is given it saves the element from the nearest day, and uses
the MJD's integer day as the hash key for retrieval, so 
we get elements fresh to within 10 days.

If MJD is not given, use the present time.

DAY-SPAN is how many days an orbit will cover; essentially, the hash-key is
an integer MJD/DAY-SPAN.

SLEEP-TIME is time to sleep before sending a request to JPL, to avoid a flood of requests."

  (let* ((mjd (* 1d0 (or mjd (astro-time:ut-to-mjd (get-universal-time)))))
	 (day-span (jpl-orbit-cache-day-span jpl-orbit-cache))
	 (mjd-round ;; round the MJD to be in the middle of the day span interval
	   (* day-span (round (/ mjd day-span))))
	 (results ;; '(NIL-OR-ELEMENTS JPL-TEXT ERROR-STRING)
	   (retrieve-cached-orbit object-name mjd
				  :jpl-orbit-cache jpl-orbit-cache)))
    ;; if the results are still fresh, use them
    (if results
	;; return old results from hash
	(values (first results) (second results) (third results))
	;;
	;; put new results in hash table
	(progn
	  (sleep sleep-time)        ;; avoid hitting JPL Horizons too fast
	  (multiple-value-bind (nil-or-elements  jpl-horizons-element-text error)
	      (get-jpl-horizons-elements object-name
					 :parse-elements t
					 :resolve-ambiguous-object t
					 :mjd mjd-round
					 :ntries ntries)
	    (when nil-or-elements ;; cache only real orbits
	      (save-orbit-to-cache
	       object-name mjd-round (list nil-or-elements jpl-horizons-element-text error)
	       :jpl-orbit-cache jpl-orbit-cache))
	    (values nil-or-elements  jpl-horizons-element-text error))))))
	      
