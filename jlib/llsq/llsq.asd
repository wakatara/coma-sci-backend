(asdf:defsystem llsq
  :depends-on ()
  ;;
  :components
  ((:file "llsq-package")
   (:file "llsq" :depends-on ("llsq-package"))
   (:file "simple-regressions" :depends-on ("llsq-package"))
   (:file "orthogonal-fit" :depends-on ("llsq-package"))
   ))

(asdf:defsystem llsq/test
  :depends-on (llsq random)
  ;;
  :components
  ((:file "llsq-test")))
