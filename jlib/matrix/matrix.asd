

;; asdf file for matrix


(asdf:defsystem matrix
    ;; needs jutils.asd to work
    :depends-on (jutils)
    ;;
    :components
    ((:file "matrix")))




    