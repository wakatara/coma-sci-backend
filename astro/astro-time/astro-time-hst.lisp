#|

Routine for parsing the peculiar YYYY.DD:HH:MM:SS format that HST uses

|#



(in-package astro-time)

(defun parse-hst-date-string-to-mjd (date)
  "Convert an HST date string like YYYY.DDD:HH:MM:SS to an MJD,
returning (VALUES MJD UT-STRING)"
  (declare (type string date))
  
  (flet ((bad-date-err ()
	   (error "HST date string ~A is not forma YYYY.DDD:HH:MM:SS"
		  date)))
    
    (loop with template = "dddd.ddd:dd:dd:dd"
	    initially (when (not (= (length date) (length template)))
			(bad-date-err))
	  for ct across template and cd across date
	  do (cond ((and (eql ct #\d) (digit-char-p cd)) t)
		   ((and (eql ct cd)) t)
		   (t (bad-date-err)))))
    (let* ((day (parse-integer date  :start 5 :junk-allowed t))
	 (year (parse-integer date :start 0 :junk-allowed t))
	 (hour-string (subseq date 9))
	 (start-of-year
	    (format nil "~A-01-01T~A" year hour-string))
	 (mjd (+ day
		 -1 ;; 
		 (astro-time:parse-ut-date-and-time-string-to-mjd
		  start-of-year)))
	 (ut-string (astro-time:mjd-to-ut-string mjd)))
    (values mjd ut-string)))


(defun mjd-to-hst-date-string (mjd)
  "Convert an MJD to an HST data string like YYYY.DDD:HH:MM:SS"
  (multiple-value-bind (y m d hr min sec) (astro-time:mjd-to-calendar-date mjd)
    (declare (ignore m d))
    (let* ((mjd-start-of-year (astro-time:calendar-date-to-mjd y 1 1 0 0 0))
	   (day-of-year (1+ (floor (- mjd mjd-start-of-year)))))
      (format nil "~A.~3,'0d:~2,'0d:~2,'0d:~2,'0d"
	      y day-of-year hr min (round sec)))))
      
