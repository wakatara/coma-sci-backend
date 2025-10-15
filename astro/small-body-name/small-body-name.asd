
(asdf:defsystem small-body-name
  :depends-on (string-utils mpc-packed-desig)
  :components
  ((:file "small-body-name")))


(asdf:defsystem small-body-name/retrieve-data
  :depends-on (small-body-name drakma astro-time)
  :components
  ((:file "small-body-name-retrieve-data")))




