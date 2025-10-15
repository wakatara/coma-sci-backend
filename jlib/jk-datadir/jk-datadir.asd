(asdf:defsystem jk-datadir
  :depends-on (cl-fad)
  :components  ((:file "jk-datadir-package" :depends-on ())
		(:file "jk-datadir" :depends-on ("jk-datadir-package"))))

  
