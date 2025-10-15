
(asdf:defsystem jpl-horizons
  :depends-on (numio drakma orbital-elements-parse
		     bordeaux-threads
		     fare-csv astro-time jk-parse-float
		     file-io observatories string-utils
		     jk-cache
		     )
  ;;
  :components
  ((:file "jpl-horizons-package" :depends-on ())
   (:file "jpl-horizons-resolver" :depends-on ("jpl-horizons-package"))
   (:file "jpl-horizons-orbits"
    :depends-on ("jpl-horizons-package" "jpl-horizons-resolver"))
   (:file "jpl-horizons-orbit-file"
    :depends-on ("jpl-horizons-package" "jpl-horizons-resolver" "jpl-horizons-orbits"))   
   (:file "jpl-horizons-ephem"
    :depends-on ("jpl-horizons-package" "jpl-horizons-orbits" "jpl-horizons-resolver"))
   (:file "jpl-horizons-euclidean"
    :depends-on ("jpl-horizons-package" "jpl-horizons-orbits" "jpl-horizons-resolver"
					"jpl-horizons-ephem"))
   ))
