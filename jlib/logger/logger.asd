
;; very simple logging

(asdf:defsystem logger
  :depends-on (bordeaux-threads)
  :components
  ((:file "logger" :depends-on ())))



    
