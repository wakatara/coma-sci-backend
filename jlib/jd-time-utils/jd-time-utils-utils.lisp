
(in-package jd-time-utils)

(defun gregorian-date-to-jd (second minute hour date month year
			     &key (gregorian-transition t))
 "Convert calendar to Julian Day.
Return (JD N-SECONDS N-NANO-SEC).

If GREGORIAN-TRANSITION is set, then dates before Oct 15, 1582 are
treated according to Julian calendar, which is necessary, for example,
for getting year 1AD correct."
  
  (declare (type (integer -10000000000 10000000000) year)
	   (type (integer 1 12) month)
	   (type (integer 1 31) date)
	   (type (integer 0 23) hour)
	   (type (integer 0 60) minute)
	   (type (real 0 60) second)
	   (optimize speed))

  
  (let ((igreg #.(+ 15 (* 31 (+ 10  (* 12 1582)))))
	(jy 0) (jm 0) (julday 0) (ja 0)
	(nsec 0) (nnsec 0))
    (declare (type (signed-byte 60) igreg jm julday ja nsec nnsec)
	     (type (integer #.(- (expt 10 12))
			    #.(+ (expt 10 12)))
		   jy))

    (cond 
      ((> month 2) 
       (setf jy year) 
       (setf jm (1+ month)))
      (t 
       (setf jy (1- year)) 
       (setf jm (+ month 13)))) 
    
    (setf julday (+ (+ (+ (floor (* 365.25d0 jy)) 
			  (floor (* 30.6001d0 jm))) 
		       date)
		    1720995))
    
    (when gregorian-transition
      (when (>= (+ date (* 31 (+ month (* 12 year)))) igreg)
	(setf ja (floor (* 0.01d0 jy)))
	(setf julday (+ (- (+ julday 2) ja) (floor (* 0.25d0 ja))))))

    (cond ((integerp second)
	   (setf nsec (+ (* 3600 hour) (* 60 minute) second)))
	  ((floatp second)
	   (multiple-value-bind (fullsec fracsec)
	       (floor (if (typep second 'single-float)
			  (float second 1d0)
			  second))
	     (setf nsec (+ (* 3600 hour) (* 60 minute) fullsec))
	     (setf nnsec (floor (* #.(expt 10 9) fracsec)))))
	  (t ;; the math at this point can involve rationals
	   (locally 
	       (declare (optimize (speed 1)))
	     (multiple-value-bind (fullsec fracsec)
		 (floor second)
	       (setf nsec (+ (* 3600 hour) (* 60 minute) fullsec))
	       (setf nnsec (floor (* #.(expt 10 9) fracsec)))))))
      
    (values julday nsec nnsec)))

(defun gregorian-date-to-jd-seconds (second minute hour date month year
				     &key (gregorian-transition t))
  "Return Julian seconds as (values nseconds n-nanonseconds)."
  (multiple-value-bind (jd nsec nnsec)
      (gregorian-date-to-jd second minute hour date month year
			    :gregorian-transition gregorian-transition)
    (values (+ (* jd #.(* 24 3600))
	       nsec)
	    nnsec)))


(defconstant +jd-1900+ 2415021) ;; JD of start of Lisp epoch 1900-00-00T00:00:00
(defconstant +jd-1900-sec+ (* +jd-1900+ 24 3600)) ;; in seconds

(defun universal-time-from-calendar-date
    (second minute hour date month year
     &key (gregorian-transition t))
  "Use JD calculation to compute a Lisp 1900-based universal time
integer, which will be negative before 1900.  After 1900, it should match
(encode-universal-time ..) for time zone zero.

Returns (VALUES SECONDS NANOSECONDS)"
  (multiple-value-bind (jul-sec jul-nanosec)
      (gregorian-date-to-jd-seconds second minute hour date month year
				    :gregorian-transition gregorian-transition)
    
    (values (- jul-sec +jd-1900-sec+)
	    jul-nanosec)))


(defun get-local-timezone-at-ut (lisp-ut)
  "Get this location's time zone at a particular lisp UT, using 1900 AD for 
LISP-UT<0.  Uses builtin time functions."
  (if (not (minusp lisp-ut))
      (nth-value 8 (decode-universal-time (get-universal-time)))
      ;; for UT<0, get the timezone at UT=0
      (nth-value 8 (decode-universal-time 0))))

(defun get-local-timezone-at-calendar-date (second minute hour date month year)
  "Get this location's time zone at a particular date, using 1900 AD for earlier
times.  Uses builtin time functions."
  (let ((ut (if (>= year 1900)
		(encode-universal-time second minute hour date month year)
		;; for Year<1900, get the timezone at 1900-01-01
		(encode-universal-time 0 0 0 1 1 1900))))
    (get-local-timezone-at-ut ut)))



  




(defun jd-to-jd-seconds (jd jd-seconds-of-day jd-nanoseconds)
  "Convert JD JD-SECONDS-OF-DAY JD-NANOSECONDS to Julian Day seconds
with 0 at January 1, 4713 BC in proleptic Julian calendar.

Return (VALUES JD-SECONDS JD-NANOSECONDS) where JD-NANOSECONDS is
appropriately truncated."
  (declare (type (signed-byte 60) jd jd-seconds-of-day jd-nanoseconds))
  (let ((%sec 0)
	(%nanosec 0))
    (declare (type (unsigned-byte 60) %sec %nanosec)
	     (optimize speed))
    
    (multiple-value-bind (sec2 nanosec2)
	(floor jd-nanoseconds #.(expt 10 9))
      (setf %sec (+ jd-seconds-of-day sec2))
      (setf %nanosec  nanosec2))

    (values
     (+ (* jd 24 3600) %sec)
     %nanosec)))


(defun jd-to-universal-time (jd jd-seconds jd-nanoseconds)
  "Convert JD, JD-SECONDS and JD-NANOSECONDS to Lisp universal time
seconds as (VALUES SECONDS NANOSECONDS)."
  (multiple-value-bind (jds jdns)
      (jd-to-jd-seconds jd jd-seconds jd-nanoseconds)
    (values
     (- jds +jd-1900-sec+)
     jdns)))



(defun jd-to-gregorian-date (jday seconds nanoseconds  &key (gregorian-transition t))
  "convert a Julian day back into a calendar day, returning 

 (VALUES
   SECONDS MINUTES HOURS DATE MONTH YEAR DAY-OF-WEEK NANOSECONDS)

If GREGORIAN-TRANSITION is set, then dates before Oct 15, 1582 are
treated according to Julian calendar, which is necessary, for example,
for getting year 1AD correct."


  (declare (type (signed-byte 60) jday seconds nanoseconds))

  (let ((igreg 2299161)
	(julian 0)  (id 0) (iyyy 0) (jb 0) (jc 0) 
	(ja 0) (jd 0) (je 0) (jalpha 0) (mm 0)
	(hr 0) (min 0) (sec 0)
	(day-of-week 0)
	(%jday 0) (%secs 0) (%nanosec 0))
    (declare (type (signed-byte 60) igreg mm id iyyy jb jc ja jalpha day-of-week)
	     (type (signed-byte 60) %jday %secs %nanosec)
	     (optimize speed))

    ;; normalize nanoseconds to 0 to 10^9-1
    (multiple-value-bind (sec2 nanosec2)
	(floor nanoseconds #.(expt 10 9))
      (setf %secs (+ seconds sec2))
      (setf %nanosec  nanosec2))
    ;; Normalize seconds  to 0 to 24*3600-1
    (multiple-value-bind (day2 sec2)
	(floor %secs #.(* 24 3600))
      (setf %jday (+ jday day2))
      (setf %secs sec2))

    (setf julian %jday) ;; the true JD
    ;; handle the gregorian transition date
    (cond
      ((and gregorian-transition
	    (>= julian igreg) )
       (setf jalpha (floor
		     (* (- (- julian 1867216) 0.25d0) #.(/ 36524.25d0))))
       (setf ja (- (+ (+ julian 1) jalpha) (floor (* 0.25d0 jalpha)))))
      ;;
      (t
       (setf ja julian)))

    (setf sec %secs)
    (setf jb (+ ja 1524)) 
    (setf jc (floor
	      (+ 6680
		 (* (- (- jb 2439870) 122.0999d0) #.(/ 365.25d0)))) )
    (setf jd (+ (* 365 jc) (floor (* 0.25d0 jc)))) 
    (setf je (floor (* (- jb jd) #.(/ 30.6001d0))))
    (setf id (- (- jb jd) (floor (* 30.6001d0 je)))) 
    (setf mm (1- je)) 
    (if (> mm 12) (setf mm (- mm 12))) 
    (setf iyyy (- jc 4715)) 
    (if (> mm 2) (setf iyyy (1- iyyy))) 
    (if (<= iyyy 0) (setf iyyy (1- iyyy)))

    (setf hr (truncate sec 3600))
    (decf sec (* hr 3600))
    (setf min (truncate (the unsigned-byte sec) 60))
    (decf sec (* min 60))

    (assert (<= 0 hr  23))
    (assert (<= 0 min 59))
    (assert (<= 0 sec 59))
    (assert (<= 0 %nanosec #.(1- (expt 10 9))))
    (setf day-of-week (mod julian 7))
    (values sec min hr id mm iyyy day-of-week %nanosec)))

(defun jd-seconds-to-gregorian-date (jd-sec jd-nanosec &key (gregorian-transition t))
  "Convert JD-SECONDS and JD-NANOSECONDS to a date"
  (jd-to-gregorian-date 0 jd-sec jd-nanosec :gregorian-transition t))

(defun universal-time-to-julian-time (ut-sec nanoseconds)
  (let* ((jd-sec (+ ut-sec +jd-1900-sec+)) ;; seconds in jd epoch
	 (%jday 0) (%secs 0) (%nanosec 0))

    ;; normalize nanoseconds to 0 to 10^9-1
    (multiple-value-bind (sec2 nanosec2)
	(floor nanoseconds #.(expt 10 9))
      (setf %secs (+ jd-sec sec2))
      (setf %nanosec  nanosec2))
    ;; Normalize seconds  to 0 to 24*3600-1
    (multiple-value-bind (day2 sec2)
	(floor %secs #.(* 24 3600))
      (setf %jday day2)
      (setf %secs sec2))
    ;;
    (values %jday %secs %nanosec)))
    
    



(defun encode-universal-time/extended (second minute hour date month year
				       &key (timezone 0)
					 (gregorian-transition t))
  "Extended version of ENCODE-UNIVERSAL-TIME using JD to perform computations.

Differences relative to standard function:

  - TIMEZONE is 0 by default, and NIL does not mean use local time zone.
  - GREGORIAN-TRANSITION means switch from Gregorian to Julian calendar 
      on Oct 15, 1582.
  - NANOSECONDS keyword gives nanosecond resolution.
  - Returns (VALUES SECONDS NANOSECONDS)."   


  (declare (type (rational -24 24) timezone))
  (let ((timezone-sec (floor (* timezone 3600))))
    (multiple-value-bind (jul-sec jul-nsec)
	(gregorian-date-to-jd-seconds second minute hour date month year
				      :gregorian-transition gregorian-transition)
      
      (values (- (+ jul-sec timezone-sec) +jd-1900-sec+)
	      jul-nsec))))


(defun decode-universal-time/extended (lisp-ut &key (timezone 0)
						 (nanoseconds 0)
						 (gregorian-transition t))
    "Extended version of DECODE-UNIVERSAL-TIME using JD to perform computations
returning

 (VALUES
   SECOND MINUTE HOUR DATE MONTH YEAR DAY-OF-WEEK NANOSECONDS)

Differences relative to standard function:

  - TIMEZONE is 0 by default, and NIL does not mean use local time zone.
       Time zones increase to West in Lisp convention
  - GREGORIAN-TRANSITION means switch from Gregorian to Julian calendar 
      on Oct 15, 1582.
  - NANOSECONDS keyword gives nanosecond resolution.
  - Returns (VALUES SECOND MINUTE HOUR DATE MONTH YEAR DAY-OF-WEEK 
             NANOSECONDS ;; instead of daylight savings T or NIL
             TIMEZONE)" 

  (let* ((timezone-sec (floor (* timezone 3600)))
	 (jd-sec (+ lisp-ut (- timezone-sec) +jd-1900-sec+)))
    (multiple-value-bind (second minute hour date month year dow nanoseconds)
	(jd-seconds-to-gregorian-date
	 jd-sec nanoseconds
	 :gregorian-transition gregorian-transition)
      (values 
       second minute hour date month year dow
       ;; NB: no daylight savings information
       nanoseconds timezone))))

  
  
