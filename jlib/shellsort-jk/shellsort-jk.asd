
(asdf:defsystem shellsort-jk
  :depends-on ()
  :components
  ((:file "shellsort-jk" :depends-on ())))

(asdf:defsystem shellsort-jk/tests
  :depends-on (shellsort-jk)
  :components
  ((:file "shellsort-jk-tests" :depends-on ())))
