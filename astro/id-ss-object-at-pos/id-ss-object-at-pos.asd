

(asdf:defsystem id-ss-object-at-pos
  :depends-on (instrument-id mpc jpl-horizons astro-time 
			     small-body-name slalib-ephem
			     astro-coords landolt
			     jk-cache
			     bordeaux-threads
			     brute-force-comets)
  :components
   ((:file "id-ss-object-at-pos" :depends-on ())))
