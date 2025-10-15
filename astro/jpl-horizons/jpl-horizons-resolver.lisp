

;; when a response gives multiple options for the object identity, pick the right one
(in-package jpl-horizons)


#|
parse-resolver-jpl-response: take a jpl response like 


 Matching small-bodies: 

    Record #  Epoch-yr  >MATCH DESIG<  Primary Desig  Name  
    --------  --------  -------------  -------------  -------------------------
     900855     1978    101P           101P            Chernykh
     900856     1992    101P           101P            Chernykh
     900857     2006    101P           101P            Chernykh
     900858     2006    101P           101P            Chernykh
     900859     2005    101P-B         101P-B          Chernykh

and return 

((900857 2006 "101P" "101P            Chernykh")
 (900858 2006 "101P" "101P            Chernykh")
 (900859 2005 "101P-B" "101P-B          Chernykh")
 (900856 1992 "101P" "101P            Chernykh")
 (900855 1978 "101P" "101P            Chernykh"))


But sometimes, the YEAR is NIL because it's an asteroid, eg Kopff:

    Record #  Epoch-yr  Primary Desig  >MATCH NAME<
    --------  --------  -------------  -------------------------
        1631            1936 UC         Kopff
    90000323    1906    22P             Kopff
    90000324    1919    22P             Kopff
    90000325    1926    22P             Kopff
    90000326    1932    22P             Kopff




|#


(defun jpl-response-has-multiple-bodies (jpl-response)
  (and (search "Matching small-bodies:" jpl-response)
       (search "Record #  Epoch-yr" jpl-response)))


;; extract xxxx in MATCH = xxxx"
(defun %match-name-in-jpl-response (jpl-response)
  (let ((n-name (search "NAME =" jpl-response)))
    (when n-name
      (with-output-to-string (s)
	(loop for i from (+ n-name 7) below (length jpl-response)
	      for c = (aref jpl-response i)
	      until (char= c #\;)
	      do (write-char c s))))))

;; FAIL-ON-MULTIPLE-IDS causes a failure if IDS differ - this can happen
;; if an object matches an asteroid and a comet
;; return '(("ID" year "primary-desig" "name") ...)
;; sorted by increaesing year
(defun parse-resolver-jpl-response (jpl-response &key
						   (desig nil) 
						   (fail-on-multiple-ids t))
  "Return '((\"ID\" year \"primary-desig\" \"name\") ...) sorted by
increasing year."
  (when (and (search "Matching small-bodies:" jpl-response)
	     (search "Record #  Epoch-yr" jpl-response))
    (let ((substr (subseq jpl-response (search "Record #  Epoch-yr" jpl-response))))
      (with-input-from-string (s substr)
	(read-line s nil nil)
	(read-line s nil nil)
	(loop for line =  (read-line s nil nil)
	      until (or (not line) (< (length line) 10))
	      for id = (string-trim " "  (subseq line 0 12))
	      for yr = (ignore-errors (parse-integer (subseq line 12 21)))
	      for primary-desig = (string-trim " " (subseq line 21 38))
	      for first-primary-desig = primary-desig then first-primary-desig
	      for name = (string-trim " " (subseq line 38))
	      when (or (not desig)
		       (equalp desig primary-desig))
	      collect (list id yr primary-desig name) into rlist
	      finally
		 (let* ((srlist
			  (sort rlist
				;; special sort because year can be nil
				(lambda (yr1 yr2) 
				  (< (or yr1 0) (or yr2 0)))
				:key 'second)))
		   (when fail-on-multiple-ids
		     (let ((desigs (remove-duplicates (mapcar 'third srlist)
						      :test 'equalp)))
		       (when (> (length desigs) 1)
			 (error "Object is ambiguous; multiple JPL designations for name ~A: ~S"
				(%match-name-in-jpl-response jpl-response)
				desigs))))
		   (return srlist)))))))
				 

			
		       


(defun resolve-ambiguous-jpl-horizons-id (jpl-response &key (desig nil) (mjd nil))
  "Given a JPL response, return the first (latest) object, unless MJD is given,
in which case return the closest year to the MJD

If DESIG is set, throw out all objects for which the PRIMARY-DESIG (third JPL field)
is not exactly DESIG.  For example, if we ask for 57P, it discards 57P-A."
  (let ((resplist (parse-resolver-jpl-response jpl-response :desig desig)))
    (cond ((not mjd)
	   (first (first resplist)))
	  (t ;; find closest one
	   (loop with closest-resp = nil
		 with best-dt = most-positive-double-float
		 for resp in resplist
		 for yyyy = (second resp)
		 ;; we GUESS that the ephem is good for YYYY-06-01, but JPL
		 ;; doesn't offer sub-year resolution		 
		 for mjd-resp = (astro-time:calendar-date-to-mjd yyyy 06 01 0 0 0)
		 for dt = (abs (- mjd mjd-resp))
		 do
		    (when (< dt best-dt)
		      (setf best-dt dt)
		      (setf closest-resp resp))
		 finally
		    (when closest-resp
		      (return (first closest-resp))))))))
	  
	  

