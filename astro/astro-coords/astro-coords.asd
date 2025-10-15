

;; asdf file for astro-coords


(asdf:defsystem astro-coords
    ;; needs jutils.asd to work
    :depends-on (alexandria ra-dec matrix precess quaternion three-vector)
    ;;
    :components
  ((:file "astro-coords-package")
   (:file "utils" :depends-on ("astro-coords-package"))
   (:file "coord-xforms" :depends-on ("utils"))
   (:file "astro-coords" :depends-on ("coord-xforms"))))





    
