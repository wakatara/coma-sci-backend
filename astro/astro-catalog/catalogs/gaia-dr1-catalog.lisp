
;; routines to download astronomical catalogs



(in-package astro-catalog)

(defconstant +gaia-dr1-epoch+ 57023.0d0) ;; 2015.0 MJD
  


(defun read-gaia-dr1-catalog-vizquery (ra-deg dec-deg radius-deg &key)
  (multiple-value-bind (data-vec keys-or-error)
      (ignore-errors
       (run-vizquery-and-parse/multisites ra-deg dec-deg
					  (* radius-deg 60.0) "I/337"
				'(("Source" :id string "")
				  ("RA_ICRS" :ra double-float 1d99)
				  ("DE_ICRS" :dec double-float 1d99)
				  ;; at epoch 2015
				  ("e_RA_ICRS" :ra-err double-float 1d99)
				  ("e_DE_ICRS" :dec-err double-float 1d99)
				  ;; proper motions
				  ("pmRA"     :pmra  double-float 1d99)
				  ("pmDE"     :pmdec  double-float 1d99)
				  ;;
				  ("<Gmag>" :g  double-float 0d0)
				  ("<FG>"   :g-flux  double-float 0d0)
				  ("e_<FG>"  :g-flux-err  double-float 0d0))
				;; gaia is not at cfa (yet?)
				:vizquery-site-list '("fr" "ca" "cn" "jp")))

    (if (typep keys-or-error 'error)
	(error keys-or-error)
	(values data-vec keys-or-error))))
  




(defun read-gaia-dr1-catalog
    (ra-deg dec-deg radius-deg &key   (method :vizquery))
  "Query GAIA DR1 catalog at RA-DEG,DEC-DEG in circle of size RADIUS-DEG, using
METHOD of :WEB or :VIZQUERY."
  (cond ((eq method :vizquery)
	 (read-gaia-dr1-catalog-vizquery 
	  ra-deg dec-deg radius-deg))
	((eq method :web)
	 (error "Cannot get 2MASS catalog over web"))
	(t
	 (error "Unknown METHOD ~A - must be :WEB or :VIZQUERY" method))))


(defclass gaia-dr1-catalog (astro-catalog)
  ( ;; the default position error for objects in 2mass
   (defult-position-error :accessor default-position-error
     :initarg :default-position-error :initform #.(/ 0.10 3600d0))))



(defmethod object-mag ((acat gaia-dr1-catalog) mag-name i &key (error-if-not-exist t))
  (cond ((eq mag-name :g)
	 (let* ((mag  (get-value acat :g i))
		(flux (get-value acat :g-flux i))
		(fluxerr (get-value acat :g-flux-err i))
		(mag-err
		  (if (and (< 0 flux 1d80)
			   (< 0 fluxerr 1d80))
		      (/ fluxerr flux)
		      99d0))
		(is-bad (not (<= 0 mag 25))))
	   (values mag mag-err is-bad)))
	;; no such mag
	(t
	 (if error-if-not-exist
	     (error "Magnitude type ~A unknown in ~A" mag-name acat)
	     nil))))
	
	

(defun read-gaia-dr1-catalog-object (ra-deg dec-deg radius-deg &key
					       (method :vizquery))
  (multiple-value-bind (data fields)
      (read-gaia-dr1-catalog ra-deg dec-deg radius-deg 
				       :method method)
    (mark-astro-catalog-ok ;; use method to mark objects that are OK
     (make-instance 'gaia-dr1-catalog
		    :n (if data 
			   (length (aref data 0))
			   0)
		    :ra-center (float ra-deg 1d0) :dec-center (float dec-deg 1d0) 
		    :radius-deg (float radius-deg 1d0)
		    :data data
		    :fields fields
		    :available-mags '(:g)
		    :%map (make-map fields)))))




(defmethod object-proper-motions ((acat gaia-dr1-catalog) (i fixnum))
  (values (get-value acat :pmra i)
	  (get-value acat :pmdec i)))


(defmethod object-ra-dec ((acat gaia-dr1-catalog) (i fixnum)  &key mjd)
  (declare (ignorable mjd))
  (let ((ra  (get-value acat :ra i))
	(dec (get-value acat :dec i))
	(dra/dt (get-value acat :pmra i))    ;; mas/yr
	(ddec/dt (get-value acat :pmdec i))) ;; mas/yr

    ;; shift to epoch mjd
    (when mjd
      (let* ((years-difference (/ (- mjd +gaia-dr1-epoch+) 365.25d0))
	     (delta-ra  (* years-difference 1d-3 dra/dt))
	     (delta-dec (* years-difference 1d-3 ddec/dt)))
	(multiple-value-setq (ra dec)
	  (astro-coords:sky-angles-slew ra dec delta-ra delta-dec :units :arcsec))))
    
    (values ra dec 
	    (object-ra-err  acat i)
	    (object-dec-err  acat i)
	    (if mjd t nil)))) ;; NIL means 'no adjustment by MJD made'

(defmethod object-ra  ((acat gaia-dr1-catalog) (i fixnum)  &key mjd)
  (multiple-value-bind (ra dec dra ddec mjd-adj-p)
      (object-ra-dec acat i :mjd mjd)
    (declare (ignorable dec ddec))
    (values ra dra mjd-adj-p)))


(defmethod object-dec  ((acat gaia-dr1-catalog) (i fixnum)  &key mjd)
  (multiple-value-bind (ra dec dra ddec mjd-adj-p)
      (object-ra-dec acat i :mjd mjd)
    (declare (ignorable ra dra))
    (values dec ddec mjd-adj-p)))
