
(in-package quaternion)


(defconstant +pi/180+ #.(/ pi 180))
(defconstant +180/pi+ #.(/ 180 pi)) 

(declaim (inline deg2rad rad2deg))
(defun deg2rad (deg)  (* +pi/180+ deg))
(defun rad2deg (rad)  (* +pi/180+ rad))

(declaim (inline lon-lat->xyz xyz->lon-lat
		 regularize-equatorial-coordinates
		 convert-lon-lat-using-quaternion))




(defun regularize-equatorial-coordinates (longitude latitude &key (lon-type :one-sided))
  "Given a LONGITUDE  and a LATITUDE (zero at equator)
both in degrees, regularize them so that LAT is in [-90,90] and 
LON is in [0,360) or (-180,180].  Return (VALUES LAT-FIXED LON-FIXED).

If LON-TYPE is :ONE-SIDED, THEN USE THE (0,360) convention, otherwise
if :TWO-SIDED use (-180,180]."
  (declare (type (double-float -1d12 1d12) longitude latitude)
	   (type (member :one-sided :two-sided) lon-type)
	   (optimize speed))
  (locally (declare (optimize (speed 3) (safety 0)))
    (let ((latitude (nth-value 1 (floor latitude 360))))
      ;; latitude now in [0,360)
      (cond ((<= latitude 90)
	     t)			;; ok, do nothing
	    ((<= 90 latitude 180) ;; swung past north pole
	     (setf latitude (- 180 latitude))
	     (decf longitude 180))
	    ((<= 180 latitude 270) 
	     (setf latitude (- 180 latitude))
	     (decf longitude 180))
	    (t
	     (decf latitude 360)))
      (setf longitude (nth-value 1 (floor longitude 360)))
      (if (and (eq lon-type :two-sided)
	       (> longitude 180))
	  (decf longitude 360))
      (values longitude latitude))))



(defun lon-lat->xyz (longitude latitude &optional (r 1d0))
  "convert LONGITUDE (eg RA) and LATITUDE (eg Dec)  angles, in
degrees, to x,y,z

Formula is 
    X = cos(latitude) sin(longitude)
    Y = cos(latitude) cos(longitude)
    Z = sin(latitude)
"
  (declare (type (double-float -1d6 1d6) latitude longitude)
	   (type double-float r)
	   (optimize speed))
  ;; need to optimize safety=0 to rid of all consing
  (locally (declare (optimize (speed 3) (safety 0)))
    (let* ((latrad  (* latitude  +pi/180+))
	   (lonrad  (* longitude +pi/180+))
	   (cos-latitude   (cos  latrad))
	   (sin-latitude   (sin  latrad))
	   (cos-longitude  (cos  lonrad))
	   (sin-longitude  (sin  lonrad)))
      (declare
       (type double-float latrad lonrad
	     cos-latitude sin-latitude cos-longitude sin-longitude)
       (dynamic-extent latrad lonrad
		       cos-latitude sin-latitude cos-longitude sin-longitude))
      (values (* r (* cos-longitude cos-latitude))
	      (* r (* sin-longitude cos-latitude))
	      (* r sin-latitude)))))
 
(defun xyz->lon-lat (x y z  &key (lon-type :one-sided))
  "convert X Y Z to  LONGITUDE(eg RA) and LATITUDE (eg Dec)  angles, in
degrees, to x,y,z

Returns (VALUES LONGITUDE LATITUDE RADIUS) in degrees

Formula is 
    X = cos(latitude) sin(longitude)
    Y = cos(latitude) cos(longitude)
    Z = sin(latitude)

If LON-TYPE is :ONE-SIDED, THEN USE THE [0,360) convention, otherwise
if :TWO-SIDED use (-180,180]."
  (declare (type (double-float -1d6 1d6) x y z)
	   (optimize speed))
  ;; need to optimize safety=0 to rid of all consing
  (locally (declare (optimize (speed 3) (safety 0)))
    (let* ((r (sqrt (+ (expt x 2) (expt y 2) (expt z 2))))
	   (r% (if (zerop r) 1d0 r))
	   (lat (asin (/ z r%)))
	   (lon (atan y x)))
      (declare (type double-float r r% lat lon)
	       (dynamic-extent r% lat lon))
      ;; a quirk of sbcl - this allows some stack allocation
      (setf lat (* lat +180/pi+)
	    lon (* lon +180/pi+))
      (multiple-value-bind (lonr latr)
	  (regularize-equatorial-coordinates lon lat :lon-type lon-type)
	(values lonr latr r)))))
   


(defun rotate-lon-lat-using-quaternion (q lon lat &key (lon-type :one-sided))
  "User a quaternion Q to convert LON,LAT to a new LON,LAT
by converting to xyz coordinates, rotating, and converting back
to angles.

If LON-TYPE is :ONE-SIDED, THEN USE THE [0,360) convention, otherwise
if :TWO-SIDED use (-180,180]."
  (declare (type double-float lon lat)
	   (type quaternion q)
	   (optimize speed))
  ;; need to optimize safety=0 to rid of all consing
  (locally (declare (optimize (speed 3) (safety 0)))
    (multiple-value-bind (x y z)
	(lon-lat->xyz lon lat)
      (declare (type double-float x y z)
	       (dynamic-extent x y z))
      (multiple-value-bind (xx yy zz)
	  (quaternion:quaternion-rotate
	   x y z
	   (quaternion:quaternion-r q)  (quaternion:quaternion-i q)
	   (quaternion:quaternion-j q)  (quaternion:quaternion-k q))
	(multiple-value-bind (lon lat r)
	    (xyz->lon-lat xx yy zz :lon-type lon-type)
	  (when (> (abs (- r 1d0)) 0.00001d0) (error "Normalization error"))
	  (values lon lat))))))
