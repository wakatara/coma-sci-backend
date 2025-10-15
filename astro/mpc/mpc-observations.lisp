(in-package mpc)

(defparameter *mpc-observation-page-dir* "https://www.minorplanetcenter.net/db-search")
(defparameter *mpc-observation-form-page* (format nil "~A/~A" *mpc-observation-page-dir* "show_object"))


(defvar *return-headers* nil)
(defvar *return-text* nil)

(defun %get-mpc-object-info-page (object-name)
  (multiple-value-bind (text/nil returncode/err headers)
      	(ignore-errors
	 (drakma:http-request
	  *mpc-observation-form-page* 
	  :method :get
	  ;; need CLOSE=T because drakma fails to get next
	  ;; status line when a redirect occurs (to an alias
	  ;; object name)
	  :close t
	  :additional-headers '(("Cache-Control" . "no-cache"))
	  :user-agent "Mozilla"
	  :parameters `(("object_id" . ,object-name)
			("utf8" . "&#x2713;")  ;; this appears in web form 
			("Submit"    . "show"))))
    (cond
      ((not text/nil)     ;; failed in drakma
       (error "DRAKMA error getting mpc object info page: ~A"  returncode/err))
    ;; invalid result
      (t
       (setf *return-text* text/nil
	     *return-headers* headers)
       (values text/nil returncode/err)))))

#+nil ;; no longer get 2nd raw text page but scrape html instead
(defun %get-text-data-url-from-info-page (http-text)
  (let* ((n1 (or (search "These data are available for" http-text)
		 (error "Can't find file in the show_object page - no data available string")))
	 (n2 (or (search "href=\"" http-text :start2 n1)
		 (error "Can't find file in the show_object page - no href")))
	 (n3 (+ n2 6))
	 (n4 (or (search "\"" http-text :start2 n3)
		 (error "no closing quote"))))
    (format nil "~A/~A"
	    *mpc-observation-page-dir*
	    (subseq http-text n3 n4))))

#+nil
(defun %get-raw-mpc-textpage-for-object (object-name)
  (let ((data-url ;; transient URL of raw data packaged in main page
	  (%get-text-data-url-from-info-page
	   (%get-mpc-object-info-page object-name))))
    (format t "data-url=~A~%" data-url)
    ;; the page might take a little while to create after main page
    ;; is created, so wait in a loop
    (loop with failcode = 0
	  for sleeptime in '(2 2 4)
	  for itry from 1
	  ;;for url = (format nil "~A?try=~A" data-url itry) ;; suppress caching
	  do (sleep sleeptime)
	     (multiple-value-bind (text retcode)
		 (drakma:http-request data-url
				      :additional-headers '(("Cache-Control" . "no-cache")))
	       (format t "Returned CODE ~A for itry ~A~%" retcode itry)
	       (when (and text (= retcode 200))
		 (return (values text retcode)))
	       (setf failcode retcode))
	  finally
	     (error "Failed to retrieve data URL=~A  - HTTP code ~A"  data-url failcode))))

;; parse date in format "YYYY MM DD.xxxx"
(defun parse-mpc-date (mpc-date-string)
  (let* ((split (string-utils:split-string mpc-date-string " "))
	 (year (parse-integer (first split)))
	 (month (parse-integer (second split)))
	 (day   (jk-parse-float:parse-float (third split)))) ;; day with fractional part
    (multiple-value-bind (iday fday) ;; split into integer and fractional day
	(floor day)
      ;; compute mjd
      (+ (astro-time:calendar-date-to-mjd year month iday 0 0 0)
	 fday))))


;; extract the observations from primary HTML instead of finding the reference to the
;; secondary text page (which isn't there for automated request ... WEIRD!)
(defun %parse-observation-table-from-html (html-text)
  (let* ((html-tags
	   (cl-html-parse-walker:convert-html-list-to-html-tags
	    (cl-html-parse:parse-html html-text)))
	 ;; search for the table after <H2>Observations</H2>
	 (table-tag
	   (html-scrape:find-tag 
	    html-tags
	    :tag-name :table
	    :return :first 
	    :test-func
	    ;; test func makes a note it saw <H2>Observations</H2>
	    ;; and returns the first table after that
	    (let ((saw-title nil))
	      (lambda (thing)
		(when
		    (and (chpwalk:html-tag-p thing)
			 (eq (chpwalk:html-tag-name thing) :h2)
			 (search "Observations"
				 (first (chpwalk:html-tag-contents thing))))
		  (setf saw-title t))
		(cond ((and saw-title
			    (chpwalk:html-tag-p thing)
			    (eq (chpwalk:html-tag-name thing) :table))
		       thing)
		      (t
		       nil))))))
	 ;; and parse the table
	 (table-arr
	   (when table-tag
	     (html-scrape:scrape-html-table-into-array table-tag ))))

    (when (or (not table-arr)
	      (not (eql (array-dimension table-arr 1) 6)) ;; 6 columns
	      (not (and (equalp (first (aref table-arr 0 0)) "Date (UT)")
			(equalp (first (aref table-arr 0 1)) "J2000 RA"))))
      (error "Invalid data in table extracted from HTML"))
    (loop for i from 1 below (array-dimension table-arr 0)
	  for date-str = (first (aref table-arr i 0))
	  for ra-str   = (first (aref table-arr i 1))
	  for dec-str  = (first (aref table-arr i 2))
	  for magfilt-str  = (first (aref table-arr i 3)) ;; magnitude <space> filter
	  for obs-str   =    (first (aref table-arr i 4))
	  for ref-str   =    (first (aref table-arr i 5)) ;; not used
							  ;;
	  for date = (parse-mpc-date date-str)
	  for ra = (ra-dec:hms-string->deg ra-str)
	  for dec = (ra-dec:dms-string->deg dec-str)
	  for mf-pair = (string-utils:split-string magfilt-str " ")
	  for mag = (ignore-errors (jk-parse-float:parse-float (first mf-pair) :output-type 'single-float))
	  for filt-str = (second mf-pair)
	  for filt = (when (and filt-str
				(not (position #\& filt-str)) ;; not html nonsense
				(<= (length filt-str) 2)) ;; we think all filters are one char?
		       filt-str)
	  for obs = (first (string-utils:split-string obs-str " "))
	  collect (list
		   date
		   ra
		   dec
		   mag
		   filt
		   obs))))
		     
		     
		    
	    

;; NOTE - no longer parse it from 2nd .txt file, but instead scrape it from HTML
;; because the .txt file was not being reliably generated



;; format for raw text file is described at https://www.minorplanetcenter.net/iau/info/OpticalObs.html
;; parse an mjd line and return
;; '(mjd ra dec mag band-or-nuclear/total-string obscode)
#+nil
(defun %parse-mpc-observation-line (line)
  (declare (type string line))
  (let* ((date-string  (subseq line 15 32))
	 (ra-string    (subseq line 32 44))
	 (dec-string   (subseq line 44 56))
	 (mag-string   (subseq line 65 70))
	 (band-or-nuclear-string  (subseq line 70 71)) ;; band, or T/N for comets (Total/Nuclear)
	 (obscode      (subseq line 77 80))
	 ;;
	 (whole-mjd (astro-time:parse-ut-date-and-time-string-to-mjd
		     (concatenate 'string 
				  (substitute #\- #\space (subseq date-string 0 (position #\. date-string)))
				  "T00:00:00")))
	 (frac-date  (jk-parse-float:parse-float (subseq date-string (position #\. date-string))))
	 (mjd (+ whole-mjd (* frac-date 1d0)))
	 (ra (ra-dec:hms-string->deg ra-string))
	 (dec (ra-dec:dms-string->deg dec-string))
	 (mag (ignore-errors ;; MAG can be nil
	       (jk-parse-float:parse-float mag-string :output-type 'single-float)))
	 )

    (list mjd ra dec  mag band-or-nuclear-string obscode)))


#+nil ;; old version that gets secondary txt page
(defun get-mpc-observations-for-object (object-name)
  "Given an OBJECT-NAME, retrieve all MPC observations of the object, returning
a list of (MJD RA2000 DEC2000 MAG BAND-OR-NUCLEAR/TOTAL-STRING OBSCODE)
or NIL for failure to parse.  MAG can be NIL."
  (multiple-value-bind (text returncode)
      (ignore-errors (%get-raw-mpc-textpage-for-object object-name))
    (cond ((not text)
	   (error "Error getting mpc observations: ~A" returncode))
	  ((not (eql returncode 200))
	   (error "Returncode is ~A not 200" returncode))
	  (t
	   (with-input-from-string (s text)
	     (loop for line = (read-line s nil nil)
		   until (not line)
		   collect (ignore-errors (%parse-mpc-observation-line line))))))))
    
	
(defun get-mpc-observations-for-object (object-name)
  "Given an OBJECT-NAME, retrieve all MPC observations of the object, returning
a list of (MJD RA2000 DEC2000 MAG BAND-OR-NUCLEAR/TOTAL-STRING OBSCODE)
or NIL for failure to parse.  MAG can be NIL."
  (%parse-observation-table-from-html
   (%get-mpc-object-info-page object-name)))
  
    
  
