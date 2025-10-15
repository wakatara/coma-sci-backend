
(defpackage astro-time
  (:use #:common-lisp)
  (:export

   #:calendar-date-to-jd
   #:calendar-date-to-mjd
   #:jd-to-calendar-date
   #:mjd-to-calendar-date
   
   #:calendar-date-to-date-string 
   #:ut-to-date-string
   #:JD-to-ut-string 
   #:MJD-to-ut-string 
   #:parse-ut-date-and-time-string
   #:parse-ut-date-and-time-string-to-jd
   #:parse-ut-date-and-time-string-to-mjd
   #:to-mjd
   #:mjd-to-jd  #:jd-to-mjd
   #:ut-to-jd   #:ut-to-mjd
   #:jd-to-ut   #:mjd-to-ut
   #:mjd-to-decimal-date-string
   #:number-of-days-in-year
   #:decimal-year-to-mjd
   #:mjd-to-decimal-year
   #:lisp-ut-to-unix-time
   #:unix-time-to-lisp-ut
   #:unix-time-to-date-string
   ;; routines for converting to TAI/TT (atomic) time
   #:tai-minus-utc
   #:mjdutc-to-mjdtai
   #:mjdutc-to-mjdtt
   #:mjd-true-time-span
   ;; date of form dd/mm/yy or dd/mm/yyyy
   #:parse-dd/mm/yy-string
   ;; date of form yy/mm/dd or yyyy/dd/mm
   #:parse-yy/mm/dd-string
   ;; date of form mm/dd/yy or mm/dd/yyyy
   #:parse-mm/dd/yy-string

   ;; astro-time-hst.lisp
   #:parse-hst-date-string-to-mjd
   #:mjd-to-hst-date-string
   ))
