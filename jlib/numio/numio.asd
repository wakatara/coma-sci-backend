

;; asdf file for numio


(asdf:defsystem numio
    ;; needs jutils.asd to work
    :depends-on (jk-parse-float) 
    ;;
    :components
  ((:file "numio-package")
   (:file "numio" :depends-on ("numio-package"))))




    
