

;; asdf file for powell


(asdf:defsystem powell
  :depends-on (md-optim) ;; ubser-structure for multidim optimization
  ;;
  :components
  ((:file "powell")))




    
