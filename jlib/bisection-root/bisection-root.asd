

;; asdf file for bisection-root


(asdf:defsystem bisection-root
    ;; needs jutils.asd to work
    :depends-on (jutils)
    ;;
    :components
    ((:file "bisection-root")))




    