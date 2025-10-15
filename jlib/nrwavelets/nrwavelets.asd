;; asdf file for stats


(asdf:defsystem nrwavelets
    ;; needs jutils.asd to work
    :depends-on (cffi waaf-cffi)
    ;;
    :components
    ((:file "nrwavelets")))


