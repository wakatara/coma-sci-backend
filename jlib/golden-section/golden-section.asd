

;; asdf file for golden-section


(asdf:defsystem golden-section
    ;; needs jutils.asd to work
    :depends-on (jutils)
    ;;
    :components
    ((:file "golden-section")))




    