

(asdf:defsystem pconfig
    ;; needs jutils.asd to work
    :depends-on (bordeaux-threads) 
    ;;
    :components
    ((:file "pconfig")))
