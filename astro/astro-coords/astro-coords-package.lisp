

(defpackage astro-coords
  (:nicknames :ac)
  (:use :common-lisp)
  (:export
   #:galactic-II->b1950   #:b1950->galactic-II
   #:galactic-II->j2000   #:j2000->galactic-II
   #:equatorial-to-ecliptic
   #:ecliptic-to-equatorial
   #:ecliptic-angle-at-ecliptic-longitude 
   #:galactic-II->galactic-xyz
   #:radec->galactic-xyz
   #:observed-motion->galactic-xyz-motion
   #:observed-motion->ra-dec-xyz-motion
   #:xyz-galactic->xyz-ra-dec
   #:xyz-ra-dec->xyz-galactic
   #:subtract-solar-motion-from-v-los--ra-dec
   #:subtract-solar-motion-from-v-los--galactic-coords 
   #:sky-angle
   #:sky-angles
   #:sky-angles-slew
   #:regularize-equatorial-coordinates
   #:convert-declination-ha-to-alt-az
   #:equatorial-to-ecliptic-xyz
   #:ecliptic-to-equatorial-xyz
   ;;
   ;; some generally useful routines
   #:convert-lon-lat-using-quaternion
   #:regularize-equatorial-coordinates
   #:angle->xyz
   #:xyz->angle
   ;;
   ;; coord-transforms.lisp - the newer quaternionic forms of xforms
   #:+quaternion-J2000-to-galactic-ii+
   #:+quaternion-galactic-ii-to-J2000+
   #:+quaternion-equatorial-J2000-to-ecliptic-J2000+
   #:+quaternion-ecliptic-J2000-to-equatorial-J2000+
   #:xform-lat-lon-to-lat-lon
   #:xform-xyz-to-xyz
   #:xform-vector-to-vector
   #:xform-lat-lon-to-xyz
   
   ))
