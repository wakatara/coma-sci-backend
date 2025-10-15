
(asdf:defsystem landolt
  :depends-on (jk-parse-float ra-dec astro-coords astro-time)
  :components ((:file "landolt" :depends-on ())))

