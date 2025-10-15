

;; asdf file for infix


(asdf:defsystem infix
    ;; needs jutils.asd to work
    :depends-on (jutils)
    ;;
    :components
    ((:file "infix")))




    