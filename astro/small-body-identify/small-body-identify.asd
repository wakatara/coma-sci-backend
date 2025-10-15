

(asdf:defsystem small-body-identify
  :depends-on (astorb orbital-elements-parse astro-coords lparallel random kdtree-jk/latlon)
  :components
  ((:file "sbid-package")
   (:file "sbid-data" :depends-on ("sbid-package")) ;; astorb elements and mpc comets
   (:file "sbid-coords" :depends-on ("sbid-package"))
   (:file "sbid-search" :depends-on ("sbid-package" "sbid-coords" "sbid-data"))

   ))


(asdf:defsystem small-body-identify/test
  :depends-on (small-body-identify)
  :components
  ((:file "sbid-test")
   ))
