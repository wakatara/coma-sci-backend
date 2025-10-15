
(asdf:defsystem orbital-mech
    :depends-on ()
    ;;
    :components
    ((:file "orbital-mech-package" :depends-on ())
     (:file "orbital-mech" :depends-on ("orbital-mech-package"))
     ))


