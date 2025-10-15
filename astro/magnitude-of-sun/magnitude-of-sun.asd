
(asdf:defsystem magnitude-of-sun
  :depends-on ()
  :components
  ((:file "magnitude-of-sun" :depends-on ())))
  
  


(asdf:defsystem magnitude-of-sun/atlas
  :depends-on (magnitude-of-sun)
  :components
  ((:file "compute-atlas" :depends-on ())))
  
  
