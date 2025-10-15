
(in-package jd-time-utils)

(defstruct julian-time
  (day          0 :type (unsigned-byte 64))
  (second       0 :type (unsigned-byte 64))
  (nanosecond   0 :type (unsigned-byte 64)))

(defstruct date-time
  (year   0 :type (integer 0 10000))
  (month  0 :type (integer 0 12)) 
  (day    0 :type (integer 0 31))
  (hour   0 :type (integer 0 23))
  (minute 0 :type (integer 0 59))
  (second 0 :type (integer 0 59))
  ;; lisp universal time, and fractional seconds
  (fractional-second 0d0 :type double-float)
  (day-of-week 0 :type (integer 0 7))
  (ut 0 :type (signed-byte 60))
  (timezone 0.0 :type (real -24 24))
  (float-year 0d0 :type double-float) 
  (julian-time nil :type (or null julian-time)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to compare two DATE-TIMEs, and to return time difference.
;;
;; we DO NOT allow DT1, DT2 to be strings as well as DATE-TIME because
;; the parsing of a string  has various options.  It is the caller's job
;; to call the parser as they see fit.
;;
(defun date-time= (dt1 dt2)
  "= for UT:DATE-TIME DT1,DT2, including fractional seconds."
  (declare (type (or string date-time) dt1 dt2))
  (and (= (date-time-ut dt1) (date-time-ut dt2))
       (= (date-time-fractional-second dt1) (date-time-fractional-second dt2))))

(defun date-time> (dt1 dt2)
  "> for UT:DATE-TIME DT1,DT2, including fractional seconds."
  (declare (type (or string date-time) dt1 dt2))
  (or (> (date-time-ut dt1) (date-time-ut dt2))
      (and
       (= (date-time-ut dt1) (date-time-ut dt2))
       (> (date-time-fractional-second dt1) (date-time-fractional-second dt2)))))

(defun date-time< (dt1 dt2)
  "< for UT:DATE-TIME DT1,DT2, including fractional seconds."
  (declare (type (or string date-time) dt1 dt2))
  (or (< (date-time-ut dt1) (date-time-ut dt2))
      (and
       (= (date-time-ut dt1) (date-time-ut dt2))
       (< (date-time-fractional-second dt1) (date-time-fractional-second dt2)))))
  
(defun date-time>= (dt1 dt2)
  ">= for UT:DATE-TIME DT1,DT2, including fractional seconds."
  (or (date-time> dt1 dt2)
      (date-time= dt1 dt2)))

(defun date-time<= (dt1 dt2)
  "<= for UT:DATE-TIME DT1,DT2, including fractional seconds."
  (declare (type (or string date-time) dt1 dt2))
  (or (date-time< dt1 dt2)
      (date-time= dt1 dt2)))

(defun date-time- (dt1 dt2 &key (units :seconds))
  "Returns difference  DT1-DT2 in UNITS=:SECONDS or :DAYS.  Must
fit into double-float number.  

WARNING: this is not the true clock-time difference because of leap seconds."
  (declare (type (or string date-time) dt1 dt2)
	   (type (member :seconds :days) units))
  (let* ((dt-isec   (* 1d0 (- (date-time-ut dt1) (date-time-ut dt2))))
	 (dt-fsec   (* 1d0 (- (date-time-fractional-second dt1)
			      (date-time-fractional-second dt2))))
	 (dt-sec (+ dt-isec dt-fsec)))
    (cond ((eq units :seconds)
	   dt-sec)
	  ((eq units :days)
	   (/ dt-sec #.(* 24d0 60 60 ))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		

(defun build-date-time-struct-from-ut (ut &key (timezone 0)
					  (nanoseconds 0))
  (declare (type (signed-byte 60) ut))
  (let ((frac (* nanoseconds 1d-9)))
    (multiple-value-bind (jd jd-sec jd-nanosec)
	(universal-time-to-julian-time ut nanoseconds)
      (multiple-value-bind  (sec min hr date mon yr dow)
	  (decode-universal-time/extended ut :timezone timezone)
	(make-date-time
	 :year yr :month mon :day date :hour hr :minute min :second sec
	 :day-of-week dow
	 :float-year (%compute-fractional-year
		      yr 
		      (+ ut (* 1d-9  frac)))
	 :fractional-second frac
	 :ut ut
	 :julian-time (make-julian-time :day jd :second jd-sec :nanosecond jd-nanosec)
	 :timezone timezone)))))


(defun change-date-time-timezone (date-time timezone)
  "Change the DATE-TIME structure to TIMEZONE, which must be in -24 to
24.  Note that DATE-TIME-FRACTIONAL-YEAR is always in timezone 0."
  (declare (type date-time date-time)
	   (type (real -24 24) timezone))
  (multiple-value-bind (second minute hour day month year dow)
      (decode-universal-time (date-time-ut date-time) (rational timezone))
    (make-date-time
     :year year :month month :day day :hour hour :minute minute :second second
     :day-of-week dow
     :fractional-second (date-time-fractional-second date-time)
     :timezone (float timezone 1.0)
     :ut (date-time-ut date-time)
     :julian-time (date-time-julian-time date-time)
     :float-year (date-time-float-year date-time))))



