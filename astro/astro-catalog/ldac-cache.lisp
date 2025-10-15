
#|

caching for LDAC fits files

For a directory or directories of LDAC catalogs, run

  (defparameter *ldac-cache* (build-ldcac-cache "/some/dir"))
   or
  (defparameter *ldac-cache* (build-ldcac-cache '("/some/dir1" "/some/dir2")))

Then search for a suitable LDAC catalog containing all stars
in a circle of size radius/deg around ra,dec:

  (defparameter *ldac-cat*
    (find-ldac-cat-in-ldac-cache *ldac-cache* 'usno-b1-catalog 
                                  ra dec radius/deg))

The (ldcac-cat-filename *ldac-cat*) may then be fed, for example, 
to Astromatic/terapix swarp.

|#



(in-package astro-catalog)

;; an object representing an LDAC catalog fits file
(defclass ldac-cat ()
  ((filename :initarg :filename :initform nil
	     :accessor ldac-cat-filename)
   (ra0 :initarg :ra0 :initform 0d0 :accessor ldac-cat-ra0)
   (dec0 :initarg :dec0 :initform 0d0 :accessor ldac-cat-dec0)
   ;; radius in deg
   (radius :initarg :radius :initform 0d0 :accessor ldac-cat-radius)
   (catalog-type :initarg :catalog-type :initform nil
		 :accessor ldac-cat-catalog-type)))

;; a collection of such LDAC-CACHE files.
(defclass ldac-cache ()
  ((directory-list :initarg :directory-list :initform nil
		   :accessor ldac-cache-directory-list)
   ;; file suffixes to search
   (file-suffixes :initarg :file-suffixes
		  :initform '("fits" "cat")
		  :accessor ldac-cache-file-suffixes)
   (ldac-cat-list :initarg :ldcac-cat-list :initform nil
		  :accessor ldac-cache-ldac-list)))


    


;; tests if a catfile is a valid fits file that's an LDAC table file
;; returns either an LDAC-CAT OR (VALUES NIL ERROR)
(defun ldac-file-p (catfile)
      (ignore-errors
       (block nil
	 (cf:with-open-fits-file (catfile ff)
	   ;;
	   (cf:move-to-extension ff 2)
	   (when (not (equalp (cf:fits-file-current-hdu-name ff)
			      "LDAC_IMHEAD"))
	     (return-from nil (values nil "NO LDAC_IMHEAD")))
	   ;;
	   (cf:move-to-extension ff 3)
	   (when (not (equalp (cf:fits-file-current-hdu-name ff)
			      "LDAC_OBJECTS"))
	     (return-from nil (values nil "NO LDAC_OBJECTS")))
	   ;;
	   (when (not (every (lambda (header)
			       (find header
				     (cf:fits-file-current-table-columns
				      ff) :test 'equalp))
			     '("X_WORLD" "Y_WORLD" "ERRA_WORLD"
			       "ERRB_WORLD" "MAG" "MAGERR")))
	     (return-from nil (values nil "Object tables not found")))

	   (cf:move-to-extension ff 1)
	   (let* ((cat-type (cf:read-fits-header ff "CAT_TYPE"))
		  ;; cat must be a symbol in the astro-catalog package
		  (cat-symbol (if cat-type
				  (find-symbol(string-upcase cat-type)
					      :astro-catalog)))
		  (ra (cf:read-fits-header ff "RA"))
		  (dec (cf:read-fits-header ff "DEC"))
		  (equinox (cf:read-fits-header ff "EQUINOX"))
		  (radius/deg (cf:read-fits-header ff "RADIUS")))
	     (when (not (and cat-type ra dec equinox radius/deg))
	       (return-from nil
		 (values nil 
			 "Headers cat-type ra dec equinox radius not present")))
	     (when (not (= equinox 2000))
	       (return-from nil
		 (values nil "Equinox must be 2000")))
	     (when (not cat-symbol)
	       (return-from nil
		 (values nil (format nil "Unknown catalog type: ~A" cat-type))))

	     (values
	      (make-instance 'ldac-cat
			     :filename (namestring (truename catfile))
			     :ra0 ra
			     :dec0 dec
			     :radius radius/deg
			     :catalog-type cat-symbol)
	      nil))))))
	  

(defun refresh-ldac-cache (ldc)
  "Refresh ldac-cats in ldac-cache object ldc"
  (declare (type ldac-cache ldc))
  (setf (ldac-cache-ldac-list ldc) nil) ;; clear old list
  (loop for dir in (ldac-cache-directory-list ldc)
	for truedir = (namestring (truename (probe-file dir)))
	when truedir
	  do (loop
	       for suffix in (ldac-cache-file-suffixes ldc)
	       for glob = (format nil "~A*.~A" truedir
				   (string-left-trim "." suffix))
	       for catlist = (mapcar 'namestring (directory glob))
	       do (loop 
		    for cat in catlist
		    for ldac = (ldac-file-p cat)
		    when ldac
		      do (push (ldac-file-p cat)
			       (ldac-cache-ldac-list ldc)))))
  ldc)
					     
(defun build-ldac-cache (directory-or-directory-list
			 &key
			   (file-suffixes '("fits" "cat")))
  (let ((ldc (make-instance
	      'ldac-cache
	      :file-suffixes file-suffixes
	      :directory-list (if (listp directory-or-directory-list)
				  directory-or-directory-list
				  (list directory-or-directory-list)))))
    (refresh-ldac-cache ldc)))
			    

(defun find-fully-overlapping-ldac-cat-in-ldac-cache
    (ldac-cache catalog-type ra dec r/deg)
  "Find an LDAC-CAT object in LDAC-CACHE, of class CATALOG-TYPE 
 (eg, ASTRO-CATALOG:USNO-B1-CATALOG), such that the the LDAC-CAT
contains all stars within radius R/DEG of RA,DEC, assumed J2000."
  (declare (type ldac-cache ldac-cache)
	   (type real ra dec r/deg))
  (loop for ldcat ;; get the ldcats of the correct type
	  in (loop for ldcat in (ldac-cache-ldac-list ldac-cache)
		   when (eq catalog-type (ldac-cat-catalog-type ldcat))
		     collect ldcat)
	for rcat = (ldac-cat-radius ldcat)
	for ra-cat = (ldac-cat-ra0 ldcat) 
	for dec-cat = (ldac-cat-dec0 ldcat)
	for dist = (astro-coords:sky-angle
		    (float ra-cat 1d0) (float dec-cat 1d0)
		    (float ra 1d0) (float dec 0d0)
		    :units :degrees)
	when (< (+ dist r/deg) rcat)
	  do (return ldcat)))

(defun find-all-overlapping-ldac-cats-in-ldac-cache
    (ldac-cache catalog-type ra dec r/deg)
  "Find all LDAC-CAT objects in LDAC cache that have some
objects falling within R/DEG of RA,DEC. If there is one catalog that
completely covers the desired circle, return only that one catalog."
  (declare (type ldac-cache ldac-cache)
	   (type real ra dec r/deg))
  (loop for ldcat ;; get the ldcats of the correct type
	  in (loop for ldcat in (ldac-cache-ldac-list ldac-cache)
		   when (eq catalog-type (ldac-cat-catalog-type ldcat))
		     collect ldcat)
	for rcat = (ldac-cat-radius ldcat)
	for ra-cat = (ldac-cat-ra0 ldcat) 
	for dec-cat = (ldac-cat-dec0 ldcat)
	for dist = (astro-coords:sky-angle
		    (float ra-cat 1d0) (float dec-cat 1d0)
		    (float ra 1d0) (float dec 0d0)
		    :units :degrees)
	;; if one catalog contains the entired desired circle, return
	;; it
	if (<  (+ dist r/deg) rcat)
	  do (return (list ldcat))
	;; else collect it
	else
	  if (< dist (+ r/deg rcat))
	    collect ldcat))
	   
	



(defun make-astro-catalog-from-overlaps-in-ldac-cache
  (ldac-cache catalog-type ra dec r/deg
   &key
     (error-on-fail t))
  (declare (type ldac-cache ldac-cache)
	   (type real ra dec r/deg))
  "Create a new ASTRO-CATALOG object from all LCAC-CAT
objects that are within  R/DEG of RA,DEC, combining
on uniqueness of object ID.

If ERROR-ON-FAIL is nil, it ignores read failures.  This feature is
for large batch jobs that can't afford to fail on one or two
catalogs."
  (let* ((ldcat-list (find-all-overlapping-ldac-cats-in-ldac-cache
		      ldac-cache catalog-type ra dec r/deg))
	 (catalog-list
	   (loop with outlist = nil
		 for ldcat in ldcat-list
		 do (if error-on-fail
			(push (read-catalog-from-fits-ldac
			       (ldac-cat-filename ldcat))
			      outlist)
			(multiple-value-bind (cat err)
			    (ignore-errors
			     (read-catalog-from-fits-ldac
			      (ldac-cat-filename ldcat)))
			  (declare (ignore err))
			  (when cat (push cat outlist))))
		 finally (return outlist)))
	 (combined-cat
	   (when catalog-list
	     (merge-catalogs-around-ra-dec catalog-list ra dec r/deg))))
    combined-cat))


(defun make-ldac-fits-from-overlaps-in-ldac-cache
    (ldac-cache catalog-type
     output-ldac-fits
     ra dec r/deg
     &key
       (error-on-fail t)
       (overwrite t)
       (mag-to-use nil)
       (mjd nil))
  (declare (type ldac-cache ldac-cache)
	   (type real ra dec r/deg))
  "Create a new LDAC fits file from all LCAC-CAT
objects that are within  R/DEG of RA,DEC, combining
on uniqueness of object ID.

Returns the final catalog, as well as creating
OUTPUT-LDAC-FITS

MJD projects the final catalog to this MJD, using proper motions, if
applicable to this catalog type.

If ERROR-ON-FAIL is nil, it ignores read failures.
MAG-TO-USE is which MAG is to be the main mag of the output."
  
  (let ((combined-cat (make-astro-catalog-from-overlaps-in-ldac-cache
		       ldac-cache catalog-type ra dec r/deg
		       :error-on-fail error-on-fail)))
    (write-catalog-to-fits-ldac
     combined-cat output-ldac-fits
     :overwrite overwrite
     :mag-to-use mag-to-use
     :mjd mjd)
    combined-cat))

;; parse a terapix LDAC filename into ("catalog-type" ra0 dec0 r/deg)
;; not really useful because the RA coordinates are very inaccurate
#+nil 
(defun parse-terapix-ldac-filename (filename)
  (let* ((filekey (file-io:file-basename
		   (file-io:file-minus-dir
		    (namestring filename))))
	 (numu (count #\_ filekey)))
    (when (= numu 2)
      (ignore-errors 
       (let* ((nu1 (position #\_ filekey)) ;; first underscore
	      (nu2 (position #\_ filekey :start (1+ nu1)))) ;; 2nd underscore
	 (let* ((catalog-type (subseq filekey 0 nu1))
		(rah (parse-integer filekey
				    :start (+ nu1 1) :end (+ nu1 3)))
		(ram (parse-integer filekey
				    :start (+ nu1 3) :end (+ nu1 5)))
		(signchar (aref filekey (+ nu1 5)))
		(decsign (cond ((eql signchar #\-) -1)
			       ((eql signchar #\+) +1)
			       (t (error "Invalid sign"))))
		(decd (parse-integer filekey
				     :start (+ nu1 6) :end (+ nu1 8)))
		(decm (parse-integer filekey
				     :start (+ nu1 8) :end (+ nu1 10)))
		(r/arcmin (parse-integer filekey
					 :start (+ nu2 2)))
		;;
		(ra  (ra-dec:hms->deg rah ram 0d0))
		(dec (ra-dec:dms->deg decsign decd decm 0d0))
		(r/deg (/ (max 0d0 (- r/arcmin 0.5d0)) ;; because of rounding
			 60d0)))
	   

	   (values catalog-type ra dec r/deg)))))))



