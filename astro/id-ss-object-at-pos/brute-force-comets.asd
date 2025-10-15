(asdf:defsystem brute-force-comets
  :depends-on (small-body-name slalib-ephem astro-time
			       jpl-horizons mpc file-io)
  :components ((:file "brute-force-comets")))
