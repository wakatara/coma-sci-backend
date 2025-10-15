

;; asdf file for linterp


(asdf:defsystem linterp
  ;; needs jutils.asd to work
  :depends-on (jutils)
  ;;
  :components
  ((:file "linterp")    ;; double-float version
   (:file "linterpf")   ;; single float version
   (:file "linterpf-complex") ;; complex single float version
   ))




    
