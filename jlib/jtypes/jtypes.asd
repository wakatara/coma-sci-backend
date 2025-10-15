
(asdf:defsystem jtypes
    :depends-on ()
    ;;
    :components
    ((:file "jtypes-package")
     (:file "arrays" :depends-on ("jtypes-package"))
     (:file "numbers" :depends-on ("jtypes-package"))))

  