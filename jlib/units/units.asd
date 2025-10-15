

;; asdf file for units


(asdf:defsystem units
    ;; needs jutils.asd to work
    :depends-on (jutils)
    ;;
    :components
    ((:file "units")))




    