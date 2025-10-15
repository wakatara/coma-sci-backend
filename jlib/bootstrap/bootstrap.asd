

;; asdf file for bootstrap


(asdf:defsystem bootstrap
    ;; needs jutils.asd to work
    :depends-on (fastmedian)
    ;;
    :components
    ((:file "bootstrap")))




    
