;; WITH-ARRAY-AS-FOREIGN  (WAAF)
;;
;;   macros to allow lisp arrays to be copied to foreign memory
;;   and passed to foreign functions as pointers, using CFFI


(asdf:defsystem waaf-cffi
  :depends-on (cffi)
  :components 
  ((:file "waaf-cffi")))