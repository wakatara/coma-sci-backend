(asdf:defsystem refcat
  :depends-on  (astro-coords
		pconfig
		bordeaux-threads
		drakma ;; for web retrieval
		chipz) ;; for decompression if files are .gz or .bz2
  :components
  ((:file "refcat-package" :depends-on ())
   (:file "refcat-parse"   :depends-on ("refcat-package"))
   (:file "refcat-search"  :depends-on ("refcat-package"))

   ))
