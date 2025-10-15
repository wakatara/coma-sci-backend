

(defpackage astro-catalog-tests
  (:use #:cl #:astro-catalog)
  (:export
   ;; plot-catalog.lisp
   #:plot-catalog
   #:plot-tiling
   ;; plot-fits-file.lisp
   #:ds9-plot-catalog-over-fits-image
   ))
