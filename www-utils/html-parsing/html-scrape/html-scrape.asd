(asdf:defsystem html-scrape
  :depends-on (drakma cl-html-parse-walker
		      html-entities ;; for decoding &lt; etc
		      parse-float
		      jd-time-utils ;; to parse times
		      )
  :components
  ((:file "html-scrape-package")
   (:file "extract"  :depends-on ("html-scrape-package"))
   (:file "find-tag"  :depends-on ("html-scrape-package"))
   (:file "scrape-tables" :depends-on ("html-scrape-package" "extract"))

   ))
    
