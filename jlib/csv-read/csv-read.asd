

;; asdf file for csv-read

(asdf:defsystem csv-read
    :depends-on (jk-parse-float file-io fare-csv jd-time-utils string-utils) 
    ;;
    :components
  ((:file "csv-read-package")
   (:file "csv-read" :depends-on ("csv-read-package"))
   (:file "csv-write" :depends-on ("csv-read-package"))))




    
