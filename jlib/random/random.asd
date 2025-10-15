

;; asdf file for random


(asdf:defsystem random
    ;; needs jutils.asd to work
    :depends-on (jutils matrix bordeaux-threads)
    ;;
    :components
    ((:file "random")))




    
