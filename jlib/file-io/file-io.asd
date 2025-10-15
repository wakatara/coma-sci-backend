

;; asdf file for file-io


(asdf:defsystem file-io
    ;; needs jutils.asd to work
    :depends-on (#+sbcl sb-posix jutils string-utils readable-arrays)
    ;;
    :components
    ((:file "file-io")))




    
