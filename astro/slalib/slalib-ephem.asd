

(asdf:defsystem slalib-ephem
  :depends-on (slalib three-vector sky-project ra-dec
		      numio astro-coords astro-time
		      observatories orbital-elements)
  :components
  ((:file "slalib-ephem-package" :depends-on ())
   (:file "slalib-ephem" :depends-on ("slalib-ephem-package"))
   (:file "slalib-ephem-misc" :depends-on ("slalib-ephem-package" "slalib-ephem"))
   (:file "slalib-ephem-proj"
    :depends-on ("slalib-ephem"))))
