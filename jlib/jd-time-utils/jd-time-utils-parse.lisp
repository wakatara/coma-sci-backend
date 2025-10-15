
(in-package jd-time-utils)


(defun %compute-fractional-year (year ut-dbl)
  (declare (type (integer 0 10000) year)
	   (type double-float ut-dbl))
  (let ((ut-yr-start (encode-universal-time/extended 0 0 0 1 1   year  :timezone 0))
	;; we define the year as ending as 1 second past YYYY-12-31:23:59:59
	;; to avoid any trickery with the leap second resetting at the start of the
	;; next year.  Not sure if this is right thing to do.
	(ut-yr-end   (+ 1 (encode-universal-time/extended 59 59 23 31 12  year :timezone 0))))
    (+ year
       (/ (- ut-dbl ut-yr-start)
	  (- ut-yr-end ut-yr-start)))))

(defun parse-date-time-string (string &key
					(date-convention nil)
					(century-change-year 50)
					(try-standard-formats t)
					(output-timezone 0))
  "Parse a STRING representating a date and time.

If TRY-STANDARD-FORMATS first favoring standard conventions
in CL-DATE-TIME-PARSER.

Then resort to DATE-FORMAT which is one of 
  :MM-DD-YYYY :DD-MM-YYYY :YYYY-MM-DD :MM-DD-YY :DD-MM-YY. 
In cases of XX-XX-YY where the year has 2 digits, use CENTURY-CHANGE-YEAR 
to decide whether it is 2000+YY or 1900+YY.

The standard RFC formats are (from CL-DATE-TIME-PARSER)

  Thu, 23 Jul 2013 19:42:23 GMT (RFC1123),
  Thu Jul 23 19:42:23 2013 (asctime),
  Thursday, 23-Jul-13 19:42:23 GMT (RFC1036),
  2013-07-23T19:42:23Z (RFC3339),
  20130723T194223Z (ISO8601:2004), etc.

The resultant DATE-TIME object is always returned with OUTPUT-TIMEZONE
which defaults to 0 (GMT/UT)"
  (declare (type (member nil :mm-dd-yyyy :dd-mm-yyyy :yyyy-mm-dd
					 :mm-dd-yy :dd-mm-yy)
                 date-convention)
           (type string string))
  (block retblock
    (let ((ut nil) (frac nil)
          (timezone 0)
	  year month day)

      ;; try our DATE-CONVENTION method first
      (when date-convention 
	(let* ((s2 (substitute #\- #\/ string)) ;; replace / with -
	       (digits
		 (ignore-errors (mapcar 'parse-integer
					(split-sequence:split-sequence #\- s2)))))
	  ;; all our date formats consist of 3 numbers
	  (when (= (length digits) 3) 
	    (cond ((eq date-convention :mm-dd-yyyy) 
		   (setf year (third digits))
		   (setf month (first digits))
		   (setf day   (second digits)))
		  ;;
		  ((eq date-convention :mm-dd-yy) ;; 2 digit year - yuck
		   ;; YY<30 is 20YY else 19YY
		   (setf year (let ((y (third digits)))
				(cond ((< y century-change-year)
				       (+ y 2000))
				      (t
				       (+ y 1900)))))
		   (setf month (first digits))
		   (setf day   (second digits)))
		  ;;
		  ((eq date-convention :dd-mm-yyyy)
		   (setf year (third digits))
		   (setf day   (first digits))
		   (setf month (second digits)))
		  ;;
		  ((eq date-convention :dd-mm-yy) ;; 2 digit year - yuck
		   ;; YY<30 is 20YY else 19YY
		 (setf year (let ((y (third digits)))
			      (cond ((< y century-change-year)
				     (+ y 2000))
				    (t
				     (+ y 1900)))))
		   (setf day   (first digits))
		   (setf month (second digits)))
		  ;;
		  ((eq date-convention :yyyy-mm-dd)
		   (setf year  (first digits))
		   (setf day   (third digits))
		   (setf month (second digits)))))))
      ;; now if it failed try the RFC ways in CL-DATE-TIME-PARSER
      (if (or (not day) (not month) (not year)
	      (not (< 0 day 32))
	      (not (< 0 month 13)))
	  ;; if our :YYYY-MM-DD etc parsing failed, do the fancy RFC ways
	  (if (not try-standard-formats)
	      (return-from retblock nil) ;; no more to do - can't parse
	      ;; else parse using cl-date-time-parser
	      (progn
		(multiple-value-setq (ut frac)
		  (ignore-errors
		   (cl-date-time-parser/jd-time-utils:parse-date-time string)))
		(if (numberp frac) (setf frac (float frac 1d0)))
		(when (not ut)
		  (return-from retblock nil))))
	  ;;
	  ;; else our way succeeded so convert it to a UT
	  (progn (setf frac 0d0)
		 (setf ut (ignore-errors
			   (encode-universal-time/extended
			    0 0 0 day month year :timezone timezone)))
		 (when (not ut) (return-from retblock nil))))

      (let ((nanoseconds (floor (* (or frac 0) 1d9))))
	(build-date-time-struct-from-ut
	 ut :timezone output-timezone :nanoseconds nanoseconds)))))

    
