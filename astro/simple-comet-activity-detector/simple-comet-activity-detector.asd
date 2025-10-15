

(asdf:defsystem simple-comet-activity-detector
  :depends-on (imutils stats random nintegrate)
  :components
  ((:file "scad-package" :depends-on ())
   (:file "scad-coma-fraction" :depends-on ("scad-package"))
   (:file "scad" :depends-on ("scad-package" "scad-coma-fraction"))))



(asdf:defsystem simple-comet-activity-detector/test
  :depends-on (simple-comet-activity-detector csv-read pgplot cfitsio)
  :components
  ((:file "scad-test" :depends-on ())))
