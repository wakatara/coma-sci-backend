
(asdf:defsystem mpc
  :depends-on (ra-dec numio drakma orbital-elements-parse
		      bordeaux-threads jk-parse-float observatories
		      astro-time cl-html-parse-walker cxml-jtk re
		      jk-cache string-utils html-scrape)
    ;;
    :components
    ((:file "mpc-package" :depends-on ())
     (:file "mpc-orbits" :depends-on ("mpc-package"))
     (:file "mpc-reporting-80char" :depends-on ("mpc-package"))
     (:file "mpc-reporting-xml" :depends-on ("mpc-package"))
     (:file "mpc-observations" :depends-on ("mpc-package"))
     (:file "mpc-checkers"   :depends-on ("mpc-package"))
     ))
