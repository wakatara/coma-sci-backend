


;; quiet astorb loading message in case we need astorb
(defparameter cl-user::astorb-quiet t) 
 
(asdf:defsystem coma-sci-backend
  :depends-on (yason alexandria imutils instrument-id imred slalib-ephem
		     small-body-name observatories fits-stamp jk-parse-float
		     mpc jpl-horizons units phot-calib terapix orbphot
		     small-body-identify isochrones-isodynes
		     simple-comet-activity-detector
		     id-ss-object-at-pos
		     shift-and-add a-f-rho asteroids
		     orbital-mech
		     hunchentoot ;; web server
		     drakma      ;; web client for testing
		     cl-ppcre    ;; regexp
		     )
  :components
  ((:file "coma-json-package" :depends-on ())
   (:file "constants" :depends-on ("coma-json-package"))
   (:file "main" :depends-on ("coma-json-package" "web-service"))
   (:file "structs" :depends-on ("coma-json-package" "constants"))
   (:file "utils" :depends-on ("coma-json-package" "structs"))
   (:file "dispatcher" :depends-on ("coma-json-package" "structs"))
   (:file "web-service" :depends-on ("coma-json-package" "dispatcher" "structs"))
   ;; individual functions
   (:file "hello" :depends-on ("coma-json-package" "structs"))
   (:file "name-fix" :depends-on ("coma-json-package" "structs"))
   (:file "describe-fits" :depends-on ("coma-json-package" "utils" "name-fix"))
   (:file "fits-headers" :depends-on ("coma-json-package" "utils"))
   (:file "orbits" :depends-on ("coma-json-package" "utils"))
   ;;
   (:file "ephem" :depends-on ("coma-json-package" "utils"))
   (:file "ephem-jpl-ephem" :depends-on ("coma-json-package" "utils" "ephem"))
   (:file "ephem-orbit" :depends-on ("coma-json-package" "utils" "ephem"))
   ;;
   (:file "get-orbit-xyz" :depends-on ("coma-json-package" "utils"))
   ;;
   (:file "calib-wcs" :depends-on ("coma-json-package" "utils"))
   (:file "calib-phot" :depends-on ("coma-json-package" "utils"))
   (:file "calib-qualities" :depends-on ("coma-json-package" "utils"))
   (:file "describe-fits-calib" :depends-on ("coma-json-package" "utils"))
   (:file "calibrate"
    :depends-on ("coma-json-package"
		 "utils"
		 "calib-wcs" "calib-phot" "calib-qualities"))
   (:file "make-stamp" :depends-on ("coma-json-package" "utils"))
   (:file "id-object-at-location" :depends-on ("coma-json-package" "utils"))
   (:file "find-known-objects" :depends-on ("coma-json-package" "utils"))
   (:file "isochrones-isodynes" :depends-on ("coma-json-package" "utils"))
   (:file "orbital-precision" :depends-on ("coma-json-package" "utils"))
   (:file "photometry"
    :depends-on ("coma-json-package" "utils" "orbital-precision"))
   (:file "afrho"
    :depends-on ("coma-json-package" "utils"))
   (:file "shift-and-add" :depends-on ("coma-json-package" "utils"))
   (:file "object-position-in-image"
    :depends-on ("coma-json-package" "utils" "orbital-precision"))
   (:file "filter-image" :depends-on ("coma-json-package" "utils"))
   (:file "reduce-images" :depends-on ("coma-json-package" "utils"))
   (:file "split-images-into-compatible-sets" :depends-on ("coma-json-package" "utils"))
   (:file "color-convert-utils" :depends-on ("coma-json-package" "utils"))
   (:file "color-convert-transforms" :depends-on ("coma-json-package"
						  "utils"
						  "color-convert-utils"))
   (:file "color-convert" :depends-on ("color-convert-utils" "coma-json-package" "utils"))
   ))
