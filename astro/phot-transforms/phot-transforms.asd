(asdf:defsystem phot-transforms
    ;;  
    :depends-on (infix)
    ;;
    :components
    ((:file "phot-transforms-package")
     (:file "sdss" :depends-on ("phot-transforms-package"))
     (:file "ps1" :depends-on ("phot-transforms-package"))
     (:file "atlas" :depends-on ("phot-transforms-package" "ps1"))))
