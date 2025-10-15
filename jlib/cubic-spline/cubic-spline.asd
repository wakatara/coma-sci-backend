

;; asdf file for cubic-spline


(asdf:defsystem cubic-spline
    ;; needs jutils.asd to work
    :depends-on (jutils)
    ;;
    :components
    ((:file "cubic-spline")))




    