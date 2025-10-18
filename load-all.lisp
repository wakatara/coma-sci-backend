;;;; load-all.lisp
;;;;
;;;; Loads all coma-json-server files in correct order for buildapp

;; First load all dependency systems using ASDF
;; The coma-sci-backend system has already registered all directories
(format t "~%Loading dependency systems via ASDF...~%")
(require :asdf)

;; Load all jlib dependencies
(asdf:load-system :jk-datadir)
(asdf:load-system :file-io)

;; Load all astro dependencies needed by coma-json-server
(asdf:load-system :cfitsio)
(asdf:load-system :wcs)
(asdf:load-system :astro-time)
(asdf:load-system :astro-coords)
(asdf:load-system :astro-obj)
(asdf:load-system :slalib-ephem)
(asdf:load-system :astorb)
(asdf:load-system :small-body-name)
(asdf:load-system :small-body-identify)
(asdf:load-system :jpl-horizons)
(asdf:load-system :mpc)
(asdf:load-system :orbital-elements)
(asdf:load-system :instrument-id)
(asdf:load-system :imutils)
(asdf:load-system :terapix)
(asdf:load-system :phot-transforms)
(asdf:load-system :landolt-standards)
(asdf:load-system :powell)

(format t "All dependency systems loaded!~%~%")

;; Now load the package definition
(load (merge-pathnames "coma-json-server/coma-json-package.lisp" *load-truename*))

;; Load all other files in the coma-json-server directory
(let ((json-dir (merge-pathnames "coma-json-server/" *load-truename*)))
  (dolist (file '("constants.lisp"
                  "structs.lisp"
                  "utils.lisp"
                  "dispatcher.lisp"
                  "web-service.lisp"
                  "hello.lisp"
                  "status.lisp"
                  "fits-headers.lisp"
                  "describe-fits.lisp"
                  "describe-fits-calib.lisp"
                  "extract-extension.lisp"
                  "which-extension.lisp"
                  "filter-image.lisp"
                  "calibrate.lisp"
                  "calib-wcs.lisp"
                  "calib-phot.lisp"
                  "calib-qualities.lisp"
                  "legacy-zeropoint.lisp"
                  "photometry.lisp"
                  "make-stamp.lisp"
                  "reduce-images.lisp"
                  "split-images-into-compatible-sets.lisp"
                  "shift-and-add.lisp"
                  "ephem.lisp"
                  "ephem-orbit.lisp"
                  "ephem-jpl-ephem.lisp"
                  "orbits.lisp"
                  "get-orbit-xyz.lisp"
                  "orbital-precision.lisp"
                  "name-fix.lisp"
                  "id-object-at-location.lisp"
                  "find-known-objects.lisp"
                  "object-position-in-image.lisp"
                  "isochrones-isodynes.lisp"
                  "afrho.lisp"
                  "color-convert.lisp"
                  "color-convert-utils.lisp"
                  "color-convert-transforms.lisp"
                  "main.lisp"))
    (let ((filepath (merge-pathnames file json-dir)))
      (when (probe-file filepath)
        (format t "Loading ~A...~%" file)
        (load filepath)))))

(format t "~%All coma-json-server files loaded successfully!~%")
