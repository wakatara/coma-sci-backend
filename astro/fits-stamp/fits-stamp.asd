
(asdf:defsystem fits-stamp
  :depends-on (pgplot cfitsio float-utils fastmedian slalib-ephem)
  :components
  ((:file "fits-stamp")))
