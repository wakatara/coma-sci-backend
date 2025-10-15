
(asdf:defsystem suprime-cam-fix
    :depends-on (cfitsio imutils fastmedian file-io instrument-id)
    :components
    ((:file "suprime-cam-fix" :depends-on ())))
