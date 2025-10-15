

;; asdf file for sky-project


(asdf:defsystem sky-project
    ;; needs jutils.asd to work
    :depends-on (jutils)
    ;;
    :components
    ((:file "sky-project")))




    