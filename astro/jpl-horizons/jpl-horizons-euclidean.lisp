#|
   Return Euclidean coordinates in JPL horizons

|#

(in-package jpl-horizons)

;; we fix headers for a OBSERVABLE type ephem, but not for a VECTORS type
(defun %parse-jpl-euclidean-ephem-block-as-hash (block)
  (declare (type string block))
  (let* ((header-list ;; the header line is not contiguous with the
		      ;; data, so we make it
	   '("JDTDB" "Calendar Date (TDB)" "X" "Y" "Z" "VX" "VY" "VZ" "LT" "RG" "RR"))
	 ;; hash of key=field_name, val=data_vector
	 (data-hash (make-hash-table :test 'equalp))
	 (data-block (subseq block (+ 6 (search "$$SOE" block))))
	 (csv (with-input-from-string (s data-block)
		(fare-csv:read-csv-stream s)))
	 (nlines (length  csv)))

    (loop for header in header-list
	  for i from 0
	  for key in header-list
	  do (setf (gethash key data-hash) (make-array nlines :initial-element 0)))
    
    (loop
      for line in csv
      for iline from 0
      do (loop
	   for key in header-list
	   for value-string in line
	   do (setf (aref (gethash key data-hash) iline)
		    (or (ignore-errors (parse-integer value-string))
			(ignore-errors (jk-parse-float:parse-float value-string))
			value-string))))

    (values data-hash block)))




(defun get-jpl-horizons-ephem-euclidean
    (object-name &key
		   (mjd-start 57031.333333333336d0)
		   (mjd-end   57031.334027777775d0)
		   (dt        "1 m")
		   (location "@sun")
		   (ref-plane :equatorial)
		   (observatory *default-observatory*) ;; 500=geocenter
		   (resolve-ambiguous-object t)
		   (use-file-locking t)
		   (ntries 1))
  "Return a hash table with string keys JD,X,Y,Z,VX,VY,VZ,LT,RG,RR, and
'Calendar Date (TDB)' using the VECTORS ephemeris class at JPL.

Final location sent is OBSERVATORY@LOCATION  
 (@ is added if not present), or just @LOCATION if OBSERVATORY is nil.

Sensible locations are '@sun' (default), or '@ssb'

REF-PLANE can be :ECLIPTIC :EQUATORIAL :BODY-EQUATOR, and defines
  the reference X,Y,Z plane for the output coordinates.

Output units are KM and KM/S - it seems it cannot be changed in the
web interface and CGI 'output_units' has no effect.

Location can be default @sun, or @399 for center of earth."

  (declare (type (member :ecliptic :equatorial :body-equator) ref-plane))
  
  (let* ((eph-center
	    (%parse-jpl-observatory-and-location location observatory))
	  (lockfile (format nil "~A/.JPL_HORIZONS_LISP_LOCKFILE"
			    file-io:*home-directory*))
	 (dont-lock (not use-file-locking))
	 (ref-plane-string (cond ((eq ref-plane :ecliptic)    "ECLIPTIC")
				 ;; we will convert ourselves
				 ((eq ref-plane :equatorial)  "FRAME");"ECLIPTIC")
				 ((eq ref-plane :body-equator) "BODY EQUATOR")
				 (t
				  (error "Invalid REF-PLANE ~A" ref-plane)))))
		 		 

    (loop
      for itry from 0
      for done-tries = (= itry ntries) 
      do
	 (assert (<= itry ntries)) ;; paranoid
	 ;; this is ugly because every block has a return
	 (multiple-value-bind (text/nil returncode/err)
	     (progn ; ignore-errors
	      ;; limit simultaneous access from this machine to prevent JPL
	      ;; lockout
	      (file-io:with-lock-file
		  (lockfile
		   :timeout 60
		   :on-timeout :grab
		   :dont-lock dont-lock)
		(sleep 1)
		(let ((http-params
			`(;;("batch" . "1")
			  ("format" . "text")
			  ("COMMAND" . ,(format nil "'~A'"
						(fix-jpl-object-name object-name)))
			  ("CENTER" . ,eph-center)
			  ("REF_PLANE" . ,ref-plane-string)
			  ;;("MAKE_EPHEM" . "YES")
			  ("TABLE_TYPE" . "VECTORS")
			  ;;("output_units" . "KM-S");; nothing else works
			  ;;                          - this vanished in Sep 2021
			  ("START_TIME" . ,(%mjd-to-jpl-ut mjd-start))
			  ("STOP_TIME" .  ,(%mjd-to-jpl-ut mjd-end))
			  ("STEP_SIZE" . ,(format nil "'~A'" dt)) ;; like "1 d"
			  ("CSV_FORMAT" . "YES")
			  ("OBJ_DATA"   . "NO")
			  )))

		  (drakma:http-request
		   *jpl-horizons-orbit-form-page* 
		   :method :post
		   :parameters http-params
		   ))))
	   
	   (cond
	     ;; failed in drakma
	     ((not text/nil)
	      (when done-tries
		(return (values nil text/nil returncode/err))))
	     ;; ambiguous object
	     ((and resolve-ambiguous-object
		   (resolve-ambiguous-jpl-horizons-id text/nil :desig object-name))
	      (sleep 2)
	      (return
		(get-jpl-horizons-ephem-euclidean
		 (resolve-ambiguous-jpl-horizons-id text/nil :desig object-name)
		 :mjd-start mjd-start :mjd-end mjd-end
		 :dt dt
		 :location location
		 :observatory observatory
		 :resolve-ambiguous-object nil
		 :use-file-locking nil))) ;; if recursing, don't use lockfile again
	     ;; invalid result
	     ((not (and (search "$$SOE" text/nil)
			(search "$$EOE" text/nil)))
	      (when done-tries
		(let ((first-line (with-input-from-string (s text/nil) (read-line s nil nil))))
		  (return (values nil first-line nil)))))
	     ;;
	     (t
	      (let ((hash
		      (%parse-jpl-euclidean-ephem-block-as-hash
		       (subseq text/nil
			       (search "$$SOE" text/nil) ;; grab header line
			       (search "$$EOE" text/nil)))))

		(return (values hash text/nil)))))))))

