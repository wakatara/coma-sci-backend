

(defpackage small-body-name-retrieve-data
  (:use #:cl)
  (:export
   #:retrieve-data))


(in-package small-body-name-retrieve-data)

(defparameter *data-dir*
  (concatenate
   'string
   (namestring
    (asdf:system-source-directory (asdf:find-system "small-body-name")))
   "data/"))


(defparameter *asteroid-csv*
  (concatenate 'string *data-dir* "asteroid_names.csv"))
(defparameter *numbered-comet-file*
  (concatenate 'string *data-dir* "numbered_comets.txt"))
(defparameter *periodic-comet-csv*
  (concatenate 'string *data-dir* "periodic_comets.csv"))
(defparameter *all-comet-file*
  (concatenate 'string *data-dir* "AllCometEls.txt"))

(defparameter *date-stamp-file*
  (concatenate 'string *data-dir* "download-date.txt"))

 

;; list of all comets, with names from column 102-158
;; names are like "C/2023 H1 (PANSTARRS)" or
;; "1P/Halley"
(defparameter *comet-url*
  "https://www.minorplanetcenter.net/iau/MPCORB/CometEls.txt")

;; unfortunately, this is HTML
(defparameter *asteroid-name-url*
  "https://minorplanetcenter.net/iau/lists/MPNames.html")

;; all the numbered comets are not in the comet-url
;; this too is text
(defparameter *numbered-comet-url*
  "https://www.minorplanetcenter.net/iau/lists/PeriodicCodes.html")

(defparameter *all-comet-els-url*
  "https://www.minorplanetcenter.net/iau/MPCORB/AllCometEls.txt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unfortunately, the numbered comet URL is HTML with tags so we have
;; to parse it - like
;;  460P  PANSTARRS                                 P/2016 BA14
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %read-numbered-comet-html (htext)
  (declare (type string htext))
  (with-input-from-string (s htext)
    ;; eat everything until asteroid text
    (loop for line = (read-line s nil nil)
	  until (or (not line)
		    (equalp (small-body-name::%space-trim line) "<pre>"))
	  finally (when (or (not line)
			    ;; clear two more lines
			    (not (read-line s nil nil))
			    (not (read-line s nil nil)))
		    (error
		     "premature end of line in HTML numbered comet text before <pre>")))
    
    
    (flet ((parse-ast-line (line num)
	     (declare (type string line))
	     (when (< (length line) 58)
	       (error "Short comet line #~A <~A>~%" num line))
	     (let ((id (string-trim " " (subseq line 0 7)))
		   (name   (string-trim " " (subseq line 7 49))))
	       ;(format t "LINE=<~A> number=<~A> name=<~A~%" line number name)
	       (list id name))))
	   
      (loop with out-list = nil
	    for line = (read-line s nil nil)
	    for i from 0
	    until (or (not line)
		      (and (find #\< line) ;; for speed
			   (search "</pre>" line)))
	    for (id name) = (parse-ast-line line i)
	    do (push (concatenate 'string id
				  (if name "/" "")
				  (if name name ""))
		     out-list)
	    finally (if (not line) ;; loop returned nil before </pre>
			(error
			 "Premature end of line in HTML numbered comet text before <pre>")
			(return out-list))))))

(defun download-numbered-comets (&key (numbered-comet-url *numbered-comet-url*))
  (multiple-value-bind (data status-code)
      (drakma:http-request numbered-comet-url)
    (when (not (= 200 status-code))
      (error "Asteroid download failed from ~A" numbered-comet-url))
    (let ((comet-list (%read-numbered-comet-html data)))
      
      (with-open-file (sout *numbered-comet-file* :direction :output
						  :if-exists :supersede)
	(dolist (comet comet-list)
	  (write-line comet sout))))))


(defun download-non-numbered-comets (&key (comet-url *comet-url*))
  (multiple-value-bind (data status-code)
      (drakma:http-request comet-url)
    (when (not (= 200 status-code))
      (error "Comet download failed from ~A" comet-url))
    (let ((numbered-comet-list nil) ;; "1P/Halley"
	  (periodic-comet-list nil)) ;; ("C/2023 H1"  "PANSTARRS")
      (with-input-from-string (s data)
	(loop for line = (read-line s nil nil)
	      until (not line)
	      for name = (string-trim " " (subseq line 102 153))
	      for numbered? =  (digit-char-p (aref name 0))
	      when numbered?
		do (push name numbered-comet-list)
	      when (not numbered?)
		do (let* ((n-paren (or (position #\( name)))
			  (desig (string-trim " " (subseq name 0 n-paren)))
			  (com-name (when n-paren
				      (string-trim "() "
						   (subseq name n-paren)))))
		     (push (list desig com-name)
			   periodic-comet-list))))

      #+nil ;; don't take numbered comets from here because it is incomplete
      (with-open-file (sout *numbered-comet-file* :direction :output
						  :if-exists :supersede)
	(dolist (name numbered-comet-list)
	  (write-line name sout)))
      
      (with-open-file (sout *periodic-comet-csv* :direction :output
						 :if-exists :supersede)
	(dolist (pair periodic-comet-list)
	  (if (second pair) ;; if it has a common name
	      (format sout "~S,~S~%" (first pair) (second pair))
	      (format sout "~S,~%" (first pair))))))))
	  

(defun download-all-comet-els (&key (comet-url *all-comet-els-url*))
   (multiple-value-bind (data status-code)
      (drakma:http-request comet-url)
    (when (not (= 200 status-code))
      (error "Comet download failed from ~A" comet-url))
     (with-open-file (sout *all-comet-file* :direction :output :if-exists :supersede)
       (write-string data sout))))


(defun download-comets ()
  (download-numbered-comets)
  (download-non-numbered-comets)
  (download-all-comet-els))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unfortunately, the asteroid url is HTML with tags so we have to
;; parse it - like
;; <a name="d">  (5956) d'Alembert                    d'Alembert</a>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %read-asteroid-html (htext)
  (declare (type string htext))
  (with-input-from-string (s htext)
    ;; eat everything until asteroid text
    (loop for line = (read-line s nil nil)
	  until (or (not line)
		    (equalp (small-body-name::%space-trim line) "<pre>"))
	  finally (when (not line)
		    (error
		     "premature end of line in HTML asteroid text before <pre>")))
    (flet ((remove-tags (str) ;; function to remove html tag from lne
	     (declare (type string str))
	     (if (not (find #\< str)) ;; no tag in line
		  str
		  (with-output-to-string (s)
		    (loop with in-tag = nil
			  for c across str
			  if (eql c #\<)
			    do (setf in-tag t)
			  else if (eql c #\>)
				 do (setf in-tag nil)
			  else
			    do (when (not in-tag) (write-char c s))))))
	   (parse-ast-line (line num)
	     (declare (type string line))
	     (when (< (length line) 30)
	       (error "Short asteroid line #~A <~A>~%" num line))
	     (let ((number (string-trim " ()" (subseq line 0 9)))
		   (name   (string-trim " " (subseq line 9 30))))
	       ;(format t "LINE=<~A> number=<~A> name=<~A~%" line number name)
	       (when (not (ignore-errors (parse-integer number)))
		 (error "Failed to read asteroid line #~A <~A>" num line))
	       (list number name))))
	   
      (loop with out-list = nil
	    for line = (read-line s nil nil)
	    for i from 0
	    until (or (not line)
		      (and (find #\< line) ;; for speed
			   (search "</pre>" line)))
	    for clean-line = (remove-tags line)
	    for number+name = (parse-ast-line clean-line i)
	    do (push number+name out-list)
	    finally (if (not line) ;; loop returned nil before </pre>
			(error
			 "Premature end of line in HTML asteroid text before <pre>")
			(return out-list))))))



(defun download-asteroids (&key (asteroid-url *asteroid-name-url*))
  (multiple-value-bind (data status-code)
      (drakma:http-request asteroid-url)
    (when (not (= 200 status-code))
      (error "Asteroid download failed from ~A" asteroid-url))
    (let ((asteroid-list (%read-asteroid-html data)))
      
      (with-open-file (sout *asteroid-csv* :direction :output
					   :if-exists :supersede)
	(dolist (pair asteroid-list)
	  (format sout "~S,~S~%" (first pair) (second pair)))))))


(defun maybe-backup-file (file)
  (let ((bak-name (format nil "~A.OLD" file)))
    (when (probe-file file)
      (when (probe-file bak-name)
	(delete-file bak-name))
      (rename-file file bak-name))))

(defun restore-backup-file (file)
  (let ((bak-name (format nil "~A.OLD" file)))
    (when (probe-file bak-name)
      (when (probe-file file)
	(delete-file file))
      (rename-file bak-name file))))

(defun make-datestamp-file ()
  (with-open-file (sout *date-stamp-file* :direction :output :if-exists :supersede)
    (format sout "Downloaded at ~A~%"
	    (astro-time:ut-to-date-string (get-universal-time)))))


(defun retrieve-data ()
  "Refresh the comet and asteroid files from MPC, saving old ones to .OLD.
Create a datestamp file."
  (maybe-backup-file *asteroid-csv*)
  (maybe-backup-file *numbered-comet-file*)
  (maybe-backup-file *periodic-comet-csv*)
  (maybe-backup-file *all-comet-file*)
  (let ((success nil))
    (unwind-protect
	 (progn
	   (download-comets)
	   (download-asteroids)
	   (make-datestamp-file)
	   (setf success t))
      (progn
	(when (not success)
	  (format t "WARNING - process failed; restored old files if present.")
	  (restore-backup-file *asteroid-csv*)
	  (restore-backup-file *numbered-comet-file*)
	  (restore-backup-file *periodic-comet-csv*))))))
