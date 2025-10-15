
(asdf:defsystem hypersuprime-cam-fix
    :depends-on (cfitsio imutils fastmedian file-io instrument-id)
    :components
  ((:file "hypersuprime-cam-fix-package" :depends-on ())
   (:file "hypersuprime-cam-fix-bad-regions" :depends-on ("hypersuprime-cam-fix-package"))
   (:file "hypersuprime-cam-fix" :depends-on ("hypersuprime-cam-fix-package"
					      "hypersuprime-cam-fix-bad-regions"))))

   
