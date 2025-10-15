

;; asdf file for astro-obj


(asdf:defsystem astro-obj
    ;; needs jutils.asd to work
    :depends-on (jutils three-vector sky-project astro-coords)
    ;;
    :components
    ((:file "astro-obj-package" :depends-on ())
     (:file "astro-obj" :depends-on ("astro-obj-package"))
     (:file "sky-bounds" :depends-on ("astro-obj" "astro-obj-package"))
     (:file "obj-binning" :depends-on ("astro-obj" "sky-bounds" "astro-obj-package"))
     (:file  "simple-match" :depends-on ("obj-binning" "sky-bounds" "astro-obj-package"))))



   




    





    
