

;; asdf file for three-vector


(asdf:defsystem three-vector
    ;; needs jutils.asd to work
    :depends-on (jutils)
    ;;
    :components
    ((:file "three-vector")))




    