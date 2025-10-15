
(asdf:defsystem jd-time-utils
  :depends-on (#:split-sequence
	       ;; for borrowed version of cl-date-time-parser
	       #:bordeaux-threads ;; for locking of timezones
	       #:alexandria #:split-sequence
	       #:anaphora
	       #:cl-ppcre
	       ;; local-time uses encode-universal-time
	       ;;   only at load time, so OK for times before 1900
	       #:local-time 
	       ;; any version of parse-float will work if it has
	       ;;    (parse-float:parse-float string)
	       #:parse-float)
  :components
  ((:file "jd-time-utils-package" :depends-on ())
   (:file "jd-time-utils-utils"   :depends-on ("jd-time-utils-package"))
   (:file "jd-time-utils-struct"  :depends-on ("jd-time-utils-utils"))
   ;; borrowed cl-date-time-parser, modified to use our version of
   ;;   encode-universal-time
   (:file "jd-time-utils-cl-date-time-parser"
    :depends-on ("jd-time-utils-utils"))
   (:file "jd-time-utils-parse"
    :depends-on ("jd-time-utils-cl-date-time-parser"
		 "jd-time-utils-utils"))))




(asdf:defsystem jd-time-utils/test
  :depends-on (jd-time-utils)
  :components
  ((:file "jd-time-utils-test")))
