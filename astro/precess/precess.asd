

;; asdf file for precess


(asdf:defsystem precess
    ;; needs jutils.asd to work
    :depends-on (jutils)
    ;;
    :components
    ((:file "precess")))




    