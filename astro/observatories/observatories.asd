

(asdf:defsystem observatories
  :depends-on (jk-parse-float ra-dec)
  :components
  ((:file "observatories-package" :depends-on ())
   (:file "observatories-utils"   :depends-on ("observatories-package"))
   (:file "observatories-mpc"   :depends-on ("observatories-utils"))
   (:file "observatories-main-data"   :depends-on ("observatories-utils"
						   "observatories-mpc"))))
   
   
