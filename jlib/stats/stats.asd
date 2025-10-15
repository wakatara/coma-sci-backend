

;; asdf file for stats


(asdf:defsystem stats
    ;; needs jutils.asd to work
    :depends-on (jutils gamma-function bisection-root)
    ;;
    :components
    ((:file "stats-package" :depends-on ())
     (:file "stats-utils" :depends-on ("stats-package"))
     (:file "stats-tests" :depends-on ("stats-package" "stats-utils"))
     (:file "prob-dists"  :depends-on ("stats-package" "stats-utils"))
     (:file "outliers"    :depends-on ("stats-package" "stats-utils"))
     ))
     




    
