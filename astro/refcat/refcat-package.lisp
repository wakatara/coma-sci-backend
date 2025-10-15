

(defpackage refcat
  (:use #:cl)
  (:export
   ;; each line of refcat #:refcat-entry
   #:refcat-ra #:refcat-dec       ;; deg
   #:refcat-plx #:refcat-dplx     ;; mas
   #:refcat-pmra #:refcat-dpmra   ;; mas/yr
   #:refcat-pmdec #:refcat-dpmdec #:refcat-gaia #:refcat-dgaia
   #:refcat-bp #:refcat-dbp #:refcat-rp #:refcat-drp #:refcat-teff
   #:refcat-agaia #:refcat-dupvar #:refcat-ag #:refcat-rp1 #:refcat-r1
   #:refcat-r10 #:refcat-g #:refcat-dg #:refcat-gchi #:refcat-gcontrib
   #:refcat-r #:refcat-dr #:refcat-rchi #:refcat-rcontrib #:refcat-i
   #:refcat-di #:refcat-ichi #:refcat-icontrib #:refcat-z #:refcat-dz
   #:refcat-zchi #:refcat-zcontrib #:refcat-nstat #:refcat-j
   #:refcat-dj #:refcat-h #:refcat-dh #:refcat-k #:refcat-dk

   #:+refcat-epoch+
   
   #:read-refcat-entries-for-position
   #:update-refcat-entry-to-mjd
   #:have-refcat

   ))

