;; WITH-ARRAY-AS-FOREIGN  (WAAF) test code
;;
;;  for various array types, allocate foreign memory, and copy data to
;;  foreign memory, and back


(asdf:defsystem waaf-cffi-tests
  :depends-on (waaf-cffi)
  :components 
  ((:file "waaf-cffi-tests")))