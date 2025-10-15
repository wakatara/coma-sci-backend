(asdf:defsystem astro-time
    :depends-on (numio cl-date-time-parser)
    ;;
    :components
  ((:file "astro-time-package")
   (:file "astro-time")
   (:file "astro-time-hst")))
