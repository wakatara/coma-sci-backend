

;; asdf file for quaternion


(asdf:defsystem quaternion
    :components
  ((:file "quaternion-package")
   (:file "quaternion" :depends-on ("quaternion-package"))
   (:file "quaternion-struct" :depends-on ("quaternion" "quaternion-package"))
   (:file "quaternion-utils"
    :depends-on ("quaternion-package" "quaternion" "quaternion-struct"))))




    
