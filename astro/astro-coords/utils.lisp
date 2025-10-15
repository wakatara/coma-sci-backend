

(in-package astro-coords)

(declaim (type double-float +pi/180+ +180/pi+))
(defconstant +pi/180+ #.(/ pi 180))
(defconstant +180/pi+ #.(/ 180 pi))


(declaim (inline deg2rad rad2deg))
(defun deg2rad (deg)  (* +pi/180+ deg))
(defun rad2deg (rad)  (* +180/pi+ rad))

(declaim (inline angle->xyz xyz->angle
		 regularize-equatorial-coordinates))




;; use the ones in quaternion package
(defun regularize-equatorial-coordinates (lon lat)
  (quaternion:regularize-equatorial-coordinates lon lat))
;;
(defun angle->xyz (longitude latitude &optional (r 1d0))
  (quaternion:lon-lat->xyz longitude latitude r))
;;
(defun xyz->angle (x y z)
  (quaternion:xyz->lon-lat x y z))

  
