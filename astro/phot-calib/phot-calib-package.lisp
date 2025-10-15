
(defpackage phot-calib
  (:use #:cl)
  (:export
   ;; plot-calib.lisp
   ;;
   #:phot-calib-result               #:phot-calib-result-p
   #:phot-calib-result-catalog
   #:phot-calib-result-zpmag         #:phot-calib-result-zpmag-err
   #:phot-calib-result-zpmag-inst    #:phot-calib-result-zpmag-inst-err
   #:phot-calib-result-mag-5-sigma   #:phot-calib-result-mag-10-sigma
   #:phot-calib-result-nstars
   #:phot-calib-result-ok
   #:phot-calib-result-std-filter
   #:phot-calib-result-matches
   #:phot-calib-result-match-file
   ;;
   ;;
   #:pcobj #:make-pcobj #:pcobj-p #:pcobj-x
   #:pcobj-y #:pcobj-alpha #:pcobj-delta #:pcobj-epoch #:pcobj-mag
   #:pcobj-id #:pcojb-mag-err
   #:delete-all-photcalib-headers
   #:transfer-phot-calib-headers
   ;; curve of growth photometry
   #:calibrate-image-using-catalog/cog
   #:calibrate-image-with-catalog-type/cog 
   ;; MAG_AUTO photometry - this is the best one because large
   ;; PHOT_AUTOPARAMS>(5,5) captures 99%+ of the light while handling
   ;; trailed objects nicely
   #:calibrate-image-using-catalog/mag-auto
   #:calibrate-image-with-catalog-type/mag-auto 
   ;; straightforward aperture photometry
   #:calibrate-image-using-catalog/ap
   #:calibrate-image-with-catalog-type/ap
   ;; diagnostic routine to make a plot of the image vs catalog match
   ;; files produced by above routines
   #:plot-calibration-match-file
   ;; 
   ;;
   ;; diagnostics.lisp
   #:plot-calibration-match-file
   ;;
   ;; image-image-calib.lisp
   #:calibrate-image-with-image/mag-auto 
   ;;
   ;;
   ;; mag-trans.lisp - translation functions to convert a catalog
   ;; magnitude to a desired magnitude
   #:*allowed-filters*
   #:get-mag-trans-func-for-catalog-type 
   #:get-mag-trans-func-for-catalog
   #:mag-trans-func-sdss-to-usdss
   #:mag-trans-func-sdss-to-bsdss
   #:mag-trans-func-sdss-to-rsdss
   #:mag-trans-func-sdss-to-isdss
   #:mag-trans-func-sdss-to-uj 
   #:mag-trans-func-sdss-to-bj
   #:mag-trans-func-sdss-to-vj
   #:mag-trans-func-sdss-to-rc
   #:mag-trans-func-sdss-to-ic
   #:mag-trans-func-sdss-to-usdss
   #:mag-trans-func-ps1-to-bsdss
   #:mag-trans-func-ps1-to-rsdss
   #:mag-trans-func-ps1-to-isdss
   #:mag-trans-func-ps1-to-zsdss
   #:mag-trans-func-ps1-to-uj 
   #:mag-trans-func-ps1-to-bj
   #:mag-trans-func-ps1-to-vj
   #:mag-trans-func-ps1-to-rc
   #:mag-trans-func-ps1-to-ic
   ))


(in-package phot-calib)
