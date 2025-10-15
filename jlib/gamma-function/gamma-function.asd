

;; asdf file for gamma-function


(asdf:defsystem gamma-function
    ;; needs jutils.asd to work
    :depends-on (jutils)
    ;;
    :components
    ((:file "gamma-function")))




    