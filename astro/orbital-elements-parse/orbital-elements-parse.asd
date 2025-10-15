

(asdf:defsystem orbital-elements-parse
  :depends-on (orbital-elements
	       string-utils
	       jk-parse-float
	       mpc-packed-desig
	       slalib-ephem) ;; for conversions from asteroidal to cometary
  :components
  ((:file "orbital-elements-parse-package" :depends-on ())
   (:file "orbital-elements-parse-mpc"
    :depends-on ("orbital-elements-parse-package"))
   (:file "orbital-elements-parse-jpl"
    :depends-on ("orbital-elements-parse-package"))
   (:file "orbital-elements-parse-findorb"
    :depends-on ("orbital-elements-parse-package"))
   (:file "orbital-elements-parse-general"
    :depends-on ("orbital-elements-parse-package"
		 "orbital-elements-parse-mpc"
		 "orbital-elements-parse-jpl"
		 "orbital-elements-parse-findorb"))))
   

  
