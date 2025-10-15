(in-package mpc)

;; checker for NEOs
(defparameter *mpc-neo-checker-url*
  "https://www.minorplanetcenter.net/cgi-bin/neocheck.cgi")

;; checker for all MPC
(defparameter *mpc-checker-url*
  "https://www.minorplanetcenter.net/cgi-bin/mpcheck.cgi")

(defstruct mpc-candidate
  checker-type ;; :MPC or :NEO
  asteroid-number name ra dec mag orbit
  ra-offset
  dec-offset
  dra/dt ddec/dt comment) 

;; get the part of the text corresponding to candidates
(defun %extract-mpc-checker-html-table-text (html)
  (block retblock
    (cl-html-parse-walker:walk-html
     (cl-html-parse:parse-html html)
     (lambda (&key string tag-name tag-keywords tag-contents after-contents)
       (declare (ignorable string tag-name tag-keywords tag-contents after-contents))
       (when (and (eq tag-name :pre)
		   (search "R.A.   Decl.  R.A.  Decl." (third tag-contents)))
	 (return-from retblock (third tag-contents)))
       t))
    nil))

(defun %parse-mpc-checker-candidate-line (line checker-type)
  (declare (type string line))
  (flet ((parse-vel (str) ;; parse 45+ as +45.0
	   (ignore-errors
	    (* (if (position #\+ str) 1 -1)
	       (jk-parse-float:parse-float str :junk-allowed t))))
	 (parse-offset (str) ;; parse 45+ as +45.0
	   (ignore-errors
	    (* 60 ;; convert to arcsec
	       ;; N and E are positive, S and W are negative
	       (if (or (position #\E str) (position #\N str)) 1 -1)
	       (jk-parse-float:parse-float (string-trim "NSEW " str)
					:junk-allowed t)))))
    
    (when (> (length line) 79)
      (make-mpc-candidate
       :checker-type checker-type
       :asteroid-number
       (ignore-errors (parse-integer (string-trim " ()" (subseq line 0 9))))
       :name (string-trim " " (subseq line 9 25))   :ra   (ra-dec:hms-string->deg (subseq line 25 36))
       :dec  (ra-dec:dms-string->deg (subseq line 36 46))
       :mag  (ignore-errors (jk-parse-float:parse-float (subseq line 47 52)))
       :ra-offset (parse-offset (subseq line 51 58))
       :dec-offset (parse-offset (subseq line 58 65))
       :dra/dt (parse-vel (subseq line 65 72))
       :ddec/dt (parse-vel (subseq line 72 79))
       :orbit  (string-trim " " (subseq line 79 85))
       :comment (string-trim " " (subseq line 87))))))
				 
		  

(defun %parse-mpc-checker-html-table-text (oblock checker-type)
  (declare (type string oblock))
  (with-input-from-string (s oblock)
    (read-line s nil nil) ;; get rid of 2 empty lines
    (read-line s nil nil)
    (loop for line = (read-line s nil nil)
	  until (not line)
	  for obj =  (%parse-mpc-checker-candidate-line line checker-type)
	  when obj collect obj)))
	  
       
  


(defun run-mpc-checker (ra dec mjd checker-type
			    &key (radius/arcmin 5)
			      (max-mag 25) (observatory "MKO")
			      (timeout 60))
  "Run the MPC checker; checker-type is NEO or MPC.  Returns a list
of MPC-CANDIDATEs.   RA and DEC can be decimal degrees or string."
  (declare (type (member :mpc :neo) checker-type))
  (let* ((ra-string
	   (ra-dec:ra->ra-string ra :rounding 1
				    :separator-strings #(" " " " "")))
	 (dec-string	   (ra-dec:dec->dec-string dec :rounding 0
					:separator-strings #(" " " " "")))
	 (obs (or (observatories:get-observatory observatory)
		  (error "Could not find observatory ~S" observatory)))
	 (obscode (observatories:observatory-obscode obs))
	 (checker-url (if (eq checker-type :neo)
			  *mpc-neo-checker-url*
			  *mpc-neo-checker-url*)))
		  
	
    (multiple-value-bind (year month day hr min sec)
	(astro-time:mjd-to-calendar-date mjd)
      (let* ((day-float-string
	       (format nil
		       "~,2F" ;; 2 decimal points allowed
		       (+ day
			  (/ hr 24d0)
			  (/ min (* 24d0 60d0))
			  (/ sec (* 24d0 3600d0)))))
	     (parameters 
	       `(("which" . "pos") ;; this one position
		 ("ra"    . ,ra-string)
		 ("decl"   . ,dec-string)
		 ("radius" . ,(format nil "~A" radius/arcmin))
		 ("year"  . ,(format nil "~A" year))
		 ("month" . ,(format nil "~A" month))
		 ("day"   . ,day-float-string)
		 ("TextArea" . "") ;; no list of objects
		 ("oc"    . ,obscode)
		 ("limit" . ,(format nil  "~,1F" max-mag))
		 ("sort"  . "d") ;; by distance
		 ("mot"   . "h") ;; motion in arcsec/hr
		 ("tmot"  . "s") ;; separate motions
		 ("pdes"  . "u") ;; unpacked form; otherwise "p"
		 ;; f=all; t=flagged as needing obs; n=numbered
		 ;; u=unnumbered; N=nearly numberable unnumbered
		 ("needed" . "f")
		 ("ps"     . "n") ;; hidden parameter
		 ;; html src says "l" is compact list, but this doesn't work
		 ("type"   . "p"))))
	(let* ((html-text
		 (drakma:http-request 
		  checker-url
		  :method :post
		  :parameters parameters
		  :connection-timeout timeout
		  ))
	       (table-text
		 (%extract-mpc-checker-html-table-text html-text))
	       (candidate-list
		 (when table-text
		   (%parse-mpc-checker-html-table-text table-text
						       checker-type))))

	  candidate-list)))))
		   
