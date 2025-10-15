

(asdf:defsystem  gmos-proc
  :depends-on (imutils wcs cfitsio astro-time cl-fad file-io
		       instrument-id jtypes fastmedian string-utils)
  :components
  ((:file "gmos-proc-package" :depends-on ())
   (:file "gmos-proc-assemble-chips" :depends-on ("gmos-proc-package"))
   (:file "gmos-mosaic" :depends-on ("gmos-proc-package"))))


		       
