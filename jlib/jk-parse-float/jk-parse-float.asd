

(asdf:defsystem jk-parse-float
    ;; needs jutils.asd to work
    :depends-on (float-utils) 
    ;;
    :components
    ((:file "jk-parse-float")))
