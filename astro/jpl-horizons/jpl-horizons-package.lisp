

(defpackage jpl-horizons 
    (:use #:cl)
    (:export
     ;; jpl-horizons-orbits.lisp
     #:get-jpl-horizons-elements
     #:get-jpl-horizons-elements-with-caching
     #:build-jpl-orbit-cache
     ;; jpl-horizons-ephem.lisp
     #:jpl-ephem #:jpl-ephem-p
     #:jpl-ephem-location #:jpl-ephem-observatory
     #:jpl-ephem-mjd
     #:jpl-ephem-ra  #:jpl-ephem-dec
     #:jpl-ephem-ra-3sigma-err  #:jpl-ephem-dec-3sigma-err
     #:jpl-ephem-dra/dt #:jpl-ephem-ddec/dt
     #:jpl-ephem-rhelio #:jpl-ephem-drhelio/dt
     #:jpl-ephem-delta #:jpl-ephem-ddelta/dt
     #:jpl-ephem-phase-angle
     #:jpl-ephem-total-mag #:jpl-ephem-nucleus-mag #:jpl-ephem-apparent-mag
     ;;
     #:get-jpl-horizons-ephem
     #:get-jpl-horizons-ephem/closest-orbit  ;; composite ephem using closest JPL orbit in time
     #:get-jpl-radecr-and-rates-for-observatory
     ;;
     #:get-jpl-horizons-ephem-euclidean
   ))   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities follow
(in-package jpl-horizons)

(defun get-jpl-major-body-name (name)
  (cond ((equalp name "Sun") "10")
	((equalp name "Barycenter") "0")
	((equalp name "Solar System Barycenter") "0")
	((equalp name "SSB") "0")
	;;
	((equalp name "Mercury") "199")
	((equalp name "Venus")   "299")
	((equalp name "Earth")   "399")
	((equalp name "Moon")    "301")
	((equalp name "Mars")    "499")
	((equalp name "Jupiter") "599")
	((equalp name "Saturn")  "699")
	((equalp name "Uranus")  "799")
	((equalp name "Neptune") "899")
	((equalp name "Pluto")   "999")
	;;
	((equalp name "Mercury Barycenter") "1")
	((equalp name "Venus Barycenter")   "2")
	((equalp name "Earth Barycenter")   "3")
	((equalp name "EMB")                "3")
	((equalp name "Mars Barycenter")    "4")
	((equalp name "Jupiter Barycenter") "5")
	((equalp name "Saturn Barycenter")  "6")
	((equalp name "Uranus Barycenter")  "7")
	((equalp name "Neptune Barycenter") "8")
	((equalp name "Pluto Barycenter")   "9")
	;;
	((equalp name "Mercury_Barycenter") "1")
	((equalp name "Venus_Barycenter")   "2")
	((equalp name "Earth_Barycenter")   "3")
	((equalp name "Mars_Barycenter")    "4")
	((equalp name "Jupiter_Barycenter") "5")
	((equalp name "Saturn_Barycenter")  "6")
	((equalp name "Uranus_Barycenter")  "7")
	((equalp name "Neptune_Barycenter") "8")
	((equalp name "Pluto_Barycenter")   "9")))
  


;; JPL doesn't understand asteroids with parens like "(123)" so we strip
;; them
(defun fix-jpl-object-name (object-name)
  (let ((oname2 (string-trim " " object-name)))
    (or (get-jpl-major-body-name object-name)
	(concatenate 'string
	 ;; adding a ';" at the end is for MINOR bodies only
	 ;; see https://ssd-api.jpl.nasa.gov/doc/horizons.html
	 (if (and (plusp (length oname2))
		  (char= (aref oname2 0) #\()
		  (char= (aref oname2 (1- (length oname2))) #\)))
	     (string-trim " " (subseq oname2 1 (1- (length oname2))))
	     oname2)
	 ";"))))  ;; if not a major body, needs semicolon to say we want exactly this
	       

;; lockfile to prevent multiple processes from accessing JPL web
;; simultaneously
(defparameter *jpl-lockfile*
  (format nil "~A/.JPL_HORIZONS_LISP_LOCKFILE"
	  file-io:*home-directory*))

;; internal lock to use as well, on top of lockfile
(defparameter *jpl-lock* (bordeaux-threads:make-recursive-lock
			  "JPL-web-access-lock"))

(defmacro with-jpl-locking
    ((&key
	(jpl-lockfile *jpl-lockfile*)
	(lockfile-timeout 60)
	;; HARD-TIMEOUT aborts using BT:WITH-TIMEOUT.
	;; This is risky but should not happen often,
	;; if at all, because we have connect-timeout on JPL call.
	;; It will happen only when the read/write gets stuck.
	(hard-timeout 180)
	(dont-file-lock nil))
     &body body)

  `(bordeaux-threads:with-recursive-lock-held (*jpl-lock*)
     ;; WITH-TIMEOUT throws an error, but we will be inside IGNORE-ERRORS
     (bordeaux-threads:with-timeout (,hard-timeout)
       (file-io:with-lock-file (,jpl-lockfile
				:timeout ,lockfile-timeout
				:on-timeout :grab
				:dont-lock ,dont-file-lock)
	 ;; the body will be drakma:http-request
	 ,@body))))
       
		    
