
(asdf:defsystem astro-catalog-tests
  :depends-on (astro-catalog ds9 sky-project pgplot wcs)
  :components
  ((:file "tests-package" :depends-on ())
   (:file "plot-catalog"  :depends-on ("tests-package"))
   (:file "plot-fits-file"  :depends-on ("tests-package"))
   ))
