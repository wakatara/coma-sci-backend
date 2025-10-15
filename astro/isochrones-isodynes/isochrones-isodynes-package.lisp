
(defpackage isochrones-isodynes
  (:nicknames :isocisod)
  (:use #:cl)
  (:export
   #:build-isochrone
   #:build-isodyne

   ;; the ISO structure holds both isochrones and isodynes
   #:iso #:iso-p
   #:iso-type  ;; :ISOCHRONE or :ISODYNE
   #:iso-dt-vec    #:iso-beta-vec
   #:iso-ra-vec    #:iso-dec-vec
   #:iso-xsky-vec  #:iso-ysky-vec 

   ))
