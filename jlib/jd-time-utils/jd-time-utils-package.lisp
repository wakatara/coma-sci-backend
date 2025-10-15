
(defpackage jd-time-utils
  (:use #:cl)
  (:export
   ;; date-time object
   #:date-time #:date-time-p
   #:date-time-year #:date-time-month #:date-time-day
   #:date-time-hour #:date-time-minute #:date-time-second
   #:date-time-fractional-second #:date-time-day-of-week
   #:date-time-ut #:date-time-float-year
   #:julian-time #:julian-time-p
   #:julian-time-day   #:julian-time-second #:julian-time-nanosecond
   ;; comparisons
   #:date-time= #:date-time> #:date-time< #:date-time>= #:date-time<=
   ;; time difference
   #:date-time-

   ;; time-utils-utils.lisp
   #:gregorian-date-to-jd
   #:gregorian-date-to-jd-seconds
   #:universal-time-from-calendar-date
   #:get-local-timezone-at-ut
   #:get-local-timezone-at-calendar-date
   #:jd-to-jd-seconds      ;; JD to JD-SECONDS from start of JD epoch
   #:jd-to-universal-time  ;; JD to UT-SECONDS
   #:jd-to-gregorian-date
   #:jd-seconds-to-gregorian-date
   #:universal-time-to-julian-time 
   ;; the main CL-resembling functions - note that timezone defaults to 0
   ;;    rather than local, and daylight savings is not returned
   #:encode-universal-time/extended
   #:decode-universal-time/extended

   #:build-date-time-struct-from-ut
   #:change-date-time-timezone
   #:parse-date-time-string
   
   ))
