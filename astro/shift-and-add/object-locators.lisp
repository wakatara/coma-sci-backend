#|

Define a LOCATOR class that allows one to compute the location of target object in a fits file

Then 
  (get-object-location-in-fits-file locator saaplan fits-file)
returns (values ra dec) for the object

and 
 (get-radec-shift-for-fits-and-fits-ref locator saaplan fits-file fits-file-ref)
returns (values delta-ra delta-dec) to shift the object's position in fits-file back to the one
in fits-file-ref.

====================================
======== TYPES of locator ==========
====================================

orbit-locator - uses an orbit to compute the position using wcs

 (build-orbit-locator orbit orbit) makes an orbit locator


----

rate-locator - uses the rate, and the ra,dec,mjd of a reference fits file


 (build-rate-locator dra-cosdec/dt ddec/dt 
                     fits-ref-file ra-ref dec-ref 
                     :mjd-keyword xxx)



|#


(in-package shift-and-add)

(defclass locator () ;; parent of locator class
  ())


(defgeneric get-object-location-in-fits-file (locator saaplan fits-file &key coords)
									  
  (:documentation "Given a LOCATOR and a FITS-FILE, return
 (VALUES RA DEC).  COORDS can be :radec or :pix."))

(defmethod get-object-location-in-fits-file
    ((locator locator) (saaplan saaplan) fits-file &key (coords :radec))
  (error "Cannot call get-object-location-in-fits-file for parent LOCATOR class."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get the shift we need
(defun get-radec-shift-for-fits-and-fits-ref (saaplan fits-file fits-ref-file)
  (multiple-value-bind (ra dec)
      (get-object-location-in-fits-file
       (saaplan-locator saaplan) saaplan fits-file :coords :radec)
    (multiple-value-bind (ra-ref dec-ref)
	(get-object-location-in-fits-file
	 (saaplan-locator saaplan) saaplan fits-ref-file :coords :radec)
      (let* ((dra (- ra ra-ref))
	     (ddec (- dec dec-ref))
	     (cos-dec (cos (* (/ pi 180) 0.5 (+ dec dec-ref))))
	     (dra-cos-dec (* cos-dec dra)))
	;; now return the quantities that must be added to the 
	;; position of the image - if the object moved forward, then
	;; the shift must be negative.
	(values (- dra-cos-dec) (- ddec))))))

(defun %convert-radec-to-pix-for-fits-file (ra dec fits-file) 
  (let ((wcs (cf:read-wcs fits-file
			  :extension (instrument-id:get-image-extension-for-onechip-fits
				      fits-file))))
    (wcs:wcs-convert-ra-dec-to-pix-xy wcs ra dec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; locator that uses an orbit
(defclass orbit-locator (locator)
  ((orbit  :initarg :orbit :accessor orbit-locator-orbit)))

(defmethod get-object-location-in-fits-file
    ((locator orbit-locator) (saaplan saaplan) fits-file &key (coords :radec))
  (declare (type (member :radec :pix) coords))
  (let* ((observatory (or (saaplan-observatory saaplan)
			  (instrument-id:get-observatory-for-fits fits-file)))
	 (mjd-keyword (saaplan-mjd-keyword saaplan))
	 (mjd (or (if mjd-keyword
		      (cf:read-fits-header fits-file mjd-keyword)
		      (instrument-id:get-mjd-mid-for-fits fits-file))
		  (error "MJD header ~A not found in ~A" mjd-keyword fits-file))))
    (multiple-value-bind (ra dec)
	(slalib-ephem:compute-radecr-from-comet-elem-for-observatory
	 (orbit-locator-orbit locator) mjd observatory)
      (cond ((eq coords :radec)
	     (values ra dec))
	    ((eq coords :pix)
	     (%convert-radec-to-pix-for-fits-file ra dec fits-file))))))


(defun build-orbit-locator (orbit)
  (make-instance 'orbit-locator :orbit orbit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rate-locator (locator)
  ((dra-cosdec/dt :initarg :dra-cosdec/dt :accessor rate-locator-dra-cosdec/dt) ;; arcsec/hr
   (ddec/dt       :initarg :ddec/dt       :accessor rate-locator-ddec/dt)       ;; arcsec/hr
   ;; reference fits file in which we know location, and the
   ;; mjd, position, and time of this fits file
   (fits-ref      :initarg fits-ref       :accessor rate-locator-fits-ref)
   (mjd-ref       :initarg mjd-ref        :accessor rate-locator-mjd-ref)
   (ra-ref        :initarg ra-ref         :accessor rate-locator-ra-ref)
   (dec-ref       :initarg dec-ref         :accessor rate-locator-dec-ref)))


(defmethod get-object-location-in-fits-file
    ((locator rate-locator) (saaplan saaplan) fits-file &key (coords :radec))
  (declare (type (member :radec :pix) coords))
  (let* ((mjd-keyword (saaplan-mjd-keyword saaplan))
	 (mjd (or (if mjd-keyword
		      (cf:read-fits-header fits-file mjd-keyword)
		      (instrument-id:get-mjd-mid-for-fits fits-file))
		  (error "MJD header ~A not found in ~A" mjd-keyword fits-file)))
	 (mjd-ref     (rate-locator-mjd-ref locator))
	 (dmjd        (- mjd mjd-ref))
	 (dt/hr       (* dmjd 24d0)) ;; hours
	 (dra-cos-dec (* (rate-locator-dra-cosdec/dt locator) dt/hr))
	 (ddec        (* (rate-locator-ddec/dt locator) dt/hr))
	 (ra-ref      (rate-locator-ra-ref locator))
	 (dec-ref     (rate-locator-dec-ref locator)))
    (multiple-value-bind (ra dec)
	(astro-coords:sky-angles-slew ra-ref dec-ref dra-cos-dec ddec :units :arcsec)
      (cond ((eq coords :radec)
	     (values ra dec))
	    ((eq coords :pix)
	     (%convert-radec-to-pix-for-fits-file ra dec fits-file))))))


(defun build-rate-locator (dra-cosdec/dt ddec/dt fits-ref-file ra-ref dec-ref
			   &key (mjd-keyword nil))
  (make-instance
   'rate-locator
   :dra-cosdec/dt dra-cosdec/dt
   :ddec/dt       ddec/dt
   :fits-ref      fits-ref-file
   :ra-ref        (* 1d0 ra-ref)
   :dec-ref       (* 1d0 dec-ref)
   :mjd-ref (or (if mjd-keyword
		    (cf:read-fits-header fits-ref-file mjd-keyword)
		    (instrument-id:get-mjd-mid-for-fits fits-ref-file))
		(error "MJD header ~A not found in ~A"
		       mjd-keyword fits-ref-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
