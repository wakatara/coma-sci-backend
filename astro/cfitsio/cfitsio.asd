

;; asdf file for cfitsio


(asdf:defsystem cfitsio
    ;; needs jutils.asd to work
  :depends-on (cl+ssl ;; apparently, a bad version of cl+ssl on mac
		      ;; can be loaded
		      ;; by cfitsio if allowed to go first
	       jutils float-utils waaf-cffi wcs trivial-garbage
	       bordeaux-threads)
    ;;
    :components
    ((:file "cfitsio-package")
     (:file "cfitsio" :depends-on ("cfitsio-ffi" "cfitsio-package"))
     (:file "cfitsio-ffi" :depends-on ("cfitsio-package"))
     (:file "cfitsio-wcs" :depends-on ("cfitsio"))
     (:file "cfitsio-wcs-extras" :depends-on ("cfitsio-wcs"))
     (:file "cfitsio-extras" :depends-on ("cfitsio" "cfitsio-wcs"))))





    
