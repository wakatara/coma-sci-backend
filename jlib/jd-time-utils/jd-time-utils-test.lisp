


(defpackage jd-time-utils/test
  (:use #:cl #:jd-time-utils)
  (:export
   #:test-encode/decode-ut
   #:test-encode/decode-random-dates

   ))

(in-package jd-time-utils/test)


;; compare these JD-based routines to built-in universal-time functions

(defun test-encode/decode-ut (second minute hour date month year &key (timezone 0))
  "Throw an error if result from our routines fails to match encode-universal-time
and decode-universal-time going from date to UT back to date."
  (multiple-value-bind (ut-orig)
      (encode-universal-time
       second minute hour date month year timezone)
    (multiple-value-bind (ut-new)
	(jd-time-utils:encode-universal-time/extended
	 second minute hour date month year :timezone timezone)
      (when (not (= ut-orig ut-new))
	(error "encode failure at ~A with UT-ORIG=~A UT-NEW=~A"
	       (list second minute hour date month year :timezone timezone)
	       ut-orig ut-new))
      (multiple-value-bind (sec0 min0 hr0 date0 mon0 year0 dow0 ignore0 tz0)
	  (decode-universal-time ut-new timezone)
	(declare (ignore ignore0)) ;; daylight savings
	(multiple-value-bind (sec1 min1 hr1 date1 mon1 year1 dow1 ignore1 tz1)
	    (decode-universal-time/extended ut-new :timezone timezone)
	  (declare (ignore ignore1)) ;; daylight savings
	  (when (not (and (= sec0 sec1)
			  (= min0 min1)
			  (= hr0  hr1)
			  (= date0 date1)
			  (= mon0  mon1)
			  (= year0 year1)
			  (= dow0 dow1)
			  (= tz0 tz1)))
	    (error "encode failure at orig: ~A  new: ~A with UT-ORIG=~A UT-NEW=~A"
		   (list sec0 min0 hr0 date0 mon0 year0 dow0 tz0)
		   (list sec1 min1 hr1 date1 mon1 year1 dow1 tz1)
		   ut-orig ut-new)))))))

(defun test-encode/decode-random-dates (&key (n-iter 1000000))
  "Run test-encode/decode-ut for n-iter iterations on random dates after 1900."
  (loop for i below n-iter
	for year = (+ 1900 (random 200))
	do
	   (test-encode/decode-ut
	    (random 60) ;; 1-59
	    (random 60)
	    (random 24)
	    (1+ (random 31))
	    (1+ (random 12))
	    year
	    ;; time zone can't nudge it before 1900-01-01T00:00:00
	    :timezone (if (> year 1900)
			   (+ -24 (random 49))
			   0))))

