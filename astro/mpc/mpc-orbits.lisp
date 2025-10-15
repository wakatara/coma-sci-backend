

(in-package mpc)

(defparameter *mpc-orbit-form-page*
  "https://www.minorplanetcenter.net/cgi-bin/mpeph2.cgi")

;; page for new (confirmation page) orbits, append ?Obj=XXXXX&orb=y
(defparameter *mpc-obsorbs-page*
  "https://cgi.minorplanetcenter.net/cgi-bin/showobsorbs.cgi")

(defun get-mpc-elements (object-name &key (parse-elements t) mjd (get-prelim-elements t))
  "Try to get the orbital elements of OBJECT-NAME as
  ORBITAL-ELEMENTS:COMET-ELEM, returning
 (VALUES NIL-OR-ELEMENTS MPC-ONELINE-ELEMENT-TEXT ERROR).  If
PARSE-ELEMENTS is NIL then NIL-OR-ELEMENTS is NIL and just the
MPC-ONELINE-ELEMENT-TEXT is returned.

MJD argument will try to get an orbit with an epoch near(er) the date
of the MJD.

GET-PRELIM-ELMENTS means to to call get-mpc-obsorbs-elements to try to
get preliminary elements, if normal elements fail."
  (multiple-value-bind (elements text/nil err)
      (get-mpc-standard-elements object-name :parse-elements parse-elements :mjd mjd)
    (cond (elements
	   (values elements text/nil err))
	  ((not get-prelim-elements)
	   (values nil text/nil err))
	  (t
	   (multiple-value-bind (elementsp text/nilp errp)
	       (get-mpc-obsorbs-elements object-name :parse-elements parse-elements :mjd mjd)
	     (cond (elementsp
		    (values elementsp text/nilp errp))
		   (t
		    (values nil text/nil err
			    nil text/nilp errp))))))))


(defun get-mpc-standard-elements (object-name &key (parse-elements t) mjd)
  "Try to get the orbital elements of OBJECT-NAME as
  ORBITAL-ELEMENTS:COMET-ELEM, returning
 (VALUES NIL-OR-ELEMENTS MPC-ONELINE-ELEMENT-TEXT ERROR).  If
PARSE-ELEMENTS is NIL then NIL-OR-ELEMENTS is NIL and just the
MPC-ONELINE-ELEMENT-TEXT is returned.

MJD argument will try to get an orbit with an epoch near(er) the date of the MJD."
  (flet ((get-start-date (mjd)
	   (if (not mjd)
	       ""
	       (multiple-value-bind (y m d)
		   (astro-time:mjd-to-calendar-date mjd)
		 (format nil "~A ~A ~A" y m d)))))
    (multiple-value-bind (text/nil returncode/err)
	(ignore-errors
	 (drakma:http-request
	  *mpc-orbit-form-page*  
	  :method :post
	  :parameters `(("TextArea" . ,object-name)
			("ty" . "e")
			;; ephem start date, which seems to correspond to NEAREST ephem elements
			("d" . ,(get-start-date mjd))
			("oed" . ,(get-start-date mjd)) ;; both of these needed? not intuitive
			("i" . "") ("u" . "d")
			("uto" . "0") ("c" . "")
			("long" . "") ("lat" . "") ("alt" . "")
			("raty" . "a") ("s" . "t")
			("m" . "m")
			("igd" . "y") ("ibh" . "y") ("fp" . "y")
			("adir" . "S") ;;("oed" . "")
			("e" . "-1")
			("resoc". "") ("tit" . "") ("bu" . "") ("ch" . "c")
			("ce" . "f") ("js" . " f")
			)))
      (cond
	;; failed in drakma
	((not text/nil)
	 (values nil text/nil returncode/err))
	;; invalid result
	((or (search "<html>" text/nil)
	     (search "ERROR" text/nil))
	 (values nil text/nil nil))
	((not parse-elements)
	 (values nil text/nil nil))
	(t
	 (multiple-value-bind (elements parsing-error)
	     (ignore-errors  (orbital-elements-parse:parse-mpc-elem-string text/nil :id object-name))
	   (cond ((not elements) ;; we had an error
		  (values nil text/nil  parsing-error))
		 (t
		  (values elements text/nil nil)))))))))


;; encode with a %20 for spaces
(defun %url-encode (string)
  (with-output-to-string (s)
    (loop for char across string
	  do (if (alphanumericp char)
		 (write-char char s)
		 (format s "\%~2,'0x" (char-code char))))))


	  
	    

(defun get-mpc-obsorbs-elements (object-name &key (parse-elements t) mjd)
  "Get the elements from the OBSORBS page, with preliminary elements from confirmation pages."
  (declare (ignore mjd))
  (let ((url (format nil "~A?Obj=~A&orb=y" *mpc-obsorbs-page*
		     (%url-encode object-name))))
        (multiple-value-bind (text/nil returncode/err)
	    (ignore-errors
	     (drakma:http-request  url  :method :get))
	  (cond
	    ;; failed in drakma
	    ((or (not text/nil)
		 (not (search "Object" text/nil)))
	     (values nil text/nil returncode/err))
	    ((not parse-elements)
	     text/nil)
	    (t
	     (with-input-from-string (s text/nil)
	       (read-line s) ;; <html> line
	       (read-line s) ;; headers
	       (let ((orbit-line (read-line s)))
		 (multiple-value-bind (elements parsing-error)
		     (ignore-errors  (orbital-elements-parse:parse-mpc-elem-string orbit-line :id object-name))
		   (cond ((not elements) ;; we had an error
			  (values nil orbit-line parsing-error))
			 (t
			  (values elements orbit-line nil)))))))))))
  






(defclass mpc-orbit-cache (jk-cache:cache)
  ((day-span :initarg :day-span :initform 10 :accessor mpc-orbit-cache-day-span)))

(defun build-mpc-orbit-cache (&key
				(day-span 10)
				(expire-days 10)
				(nmax 10000))
  "Build a MPC orbit cache with orbits valid for DAY-SPAN, and expiring in
EXPIRE-DAYS, with NMAX entries."
  (make-instance 'mpc-orbit-cache
		 :day-span    day-span
		 :expire-time (* 24 3600 expire-days)
		 :nmax nmax))

(defvar *default-mpc-orbit-cache* (build-mpc-orbit-cache))


(defmethod retrieve-cached-orbit (object-name mjd
				  &key (mpc-orbit-cache *default-mpc-orbit-cache*))
  (let ((key (list object-name (round (/ mjd (mpc-orbit-cache-day-span mpc-orbit-cache))))))
    (jk-cache:retrieve-item key mpc-orbit-cache)))

(defmethod save-orbit-to-cache  (object-name mjd results
				 &key (mpc-orbit-cache *default-mpc-orbit-cache*))
  (let ((key (list object-name (round (/ mjd (mpc-orbit-cache-day-span mpc-orbit-cache))))))
    (jk-cache:cache-item key results mpc-orbit-cache)))



(defun get-mpc-elements-with-caching (object-name &key
						  (mjd nil)
						  (sleep-time 0)
						  (mpc-orbit-cache *default-mpc-orbit-cache*))
  "Try to get the orbital elements of OBJECT-NAME as
  ORBITAL-ELEMENTS:COMET-ELEM, returning
 (VALUES NIL-OR-ELEMENTS  MPC-ONELINE-ELEMENT-TEXT ERROR).  

Unlike GET-MPC-ELEMENTS, this will cache the elements in a hash table.

SLEEP-TIME is time to sleep before sending a request to MPC, to avoid a flood of requests."

  (let* ((mjd (* 1d0 (or mjd (astro-time:ut-to-mjd (get-universal-time)))))
	 (day-span (mpc-orbit-cache-day-span mpc-orbit-cache))
	 (mjd-round ;; round the MJD to be in the middle of the day span interval
	   (* day-span (round (/ mjd day-span))))
	 (results ;; '(NIL-OR-ELEMENTS MPC-TEXT ERROR-STRING)
	   (retrieve-cached-orbit object-name mjd :mpc-orbit-cache mpc-orbit-cache)))
    
    (if results
	;; return old results from hash
	(values (first results) (second results) (third results))
	;; put new results in hash table
	(progn
	  (sleep sleep-time)
	  (multiple-value-bind (nil-or-elements  mpc-oneline-element-text error)
	       (get-mpc-elements object-name :parse-elements t
						      :mjd mjd-round)
	    (when nil-or-elements ;; cache only real orbits, not possibly temporary errors
	      (save-orbit-to-cache
	       object-name mjd-round (list nil-or-elements mpc-oneline-element-text error)
	       :mpc-orbit-cache mpc-orbit-cache))
	    (values nil-or-elements  mpc-oneline-element-text error))))))
  
  
