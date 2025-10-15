
(asdf:defsystem isochrones-isodynes
  :depends-on (kepler-orbit slalib-ephem units)
  :components
  ((:file "isochrones-isodynes-package")
   (:file "isochrones-isodynes" :depends-on ("isochrones-isodynes-package"))))


    
