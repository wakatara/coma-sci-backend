

(defpackage simple-comet-activity-detector
  (:nicknames #:scad)
  (:use #:cl)
  (:export
   #:scadresult #:scadresult-p
   #:scadresult-active-p
   #:scadresult-x0
   #:scadresult-y0
   #:scadresult-fratio
   #:scadresult-fratio-err
   #:scadresult-coma-frac
   #:scadresult-coma-frac-err
   #:scadresult-flux-total
   #:scadresult-flux-coma
   #:scadresult-flux-nucleus
   #:scadresult-fwhm
   ;;
   #:is-object-active
   ))
 
