

;; asdf file for ra-dec


(asdf:defsystem ra-dec
    ;; needs jutils.asd to work
    :depends-on (jutils)
    ;;
    :components
    ((:file "ra-dec")))




    