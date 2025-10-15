

;; asdf file for terapix


(asdf:defsystem terapix
    :depends-on (wcs cfitsio jutils file-io instrument-id
		 astro-catalog imutils cl-fad xmls numio string-utils
		 stats bootstrap pconfig astro-obj sky-project
		 astro-coords cubic-spline md5 )
    ;;
    :components
    ((:file "terapix-package")
     (:file "conf-files" :depends-on ("terapix-package"))
     (:file "parse-scamp" :depends-on  ("terapix-package"))
     (:file "run-programs"  :depends-on ("conf-files" "terapix-package"
						      "parse-scamp"))
     (:file "ez-astrometry"  :depends-on ("run-programs" "terapix-package"))
     (:file "ez-astrometry-scan"  :depends-on ("ez-astrometry"))
     (:file "sextractor" :depends-on ("terapix-package"))
     (:file "weight-images" :depends-on ("terapix-package"))
     (:file "sextractor-cog" :depends-on ("terapix-package" "sextractor"))
     ))




    
