#|

Merge different catalogs, creating a map from the primary
to the secondaries

When accessing a field of index I of a merged catalog, we first try to
get it from the primary catalog, and if nothing is returned,
we translate I into the matching J of each secondary catalog, in order

|#

(in-package astro-catalog)

;; a class representing merged catalogs of several types - only the RA,DEC
;; are merged
(defclass merged-catalog (astro-catalog)
  ((primary-catalog :accessor merged-catalog-primary-catalog
		    :initarg :primary-catalog)
   ;; list of merged catalogs
   (merged-catalogs :accessor merged-catalog-merged-catalogs
		    :initarg :merged-catalogs)
   ;; list of index vectors 
   (index-vectors :accessor merged-catalog-index-vectors
		  :initarg :index-vectors :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a macro that tries to run body on sub-catalog one-cat-var and index j-var
;; where one-cat is first the primary, then the secondaries; and j
;; is first i, then the mapping of i
(defmacro %merged-catalogs-get-ith-field (merged-cat one-cat-var i j-var &body body)
  `(flet ((%do-body (,one-cat-var ,j-var) ,@body)) 
     (or (%do-body (merged-catalog-primary-catalog ,merged-cat i)) ;; try the primary
	 (loop for %cat in (merged-catalog-merged-catalogs ,merged-cat)
	       for %ivec in (merged-catalog-index-vectors ,merged-cat)
	       for %j = (aref %ivec ,i)
	       ;; handle up to 4 multiple values
	       do (multiple-value-bind (%val1 %val2 %val3 %val4) 
		      (%do-body %cat %j)
		    (when %val1
		      (return (values %val1 %val2 %val3 %val4))))))))
		    
(defmethod get-astro-catalog-vector ((mcat merged-catalog) field &key (error-if-not-exist t))
  (or
   ;; try getting it from primary catalog
   (get-astro-catalog-vector
    (merged-catalog-primary-catalog mcat) field :error-if-not-exist nil)
   ;; else loop through secondary catalogs
   (loop for indexvec in (merged-catalog-index-vectors mcat)
	 for cat in (merged-catalog-merged-catalogs mcat)
	 for vec = (get-astro-catalog-vector cat field :error-if-not-exist nil)
	 when vec ;; map the child cat's vector to parent's using indexvec
	   do
	      (let ((truevec ;; map to number of first catalog
		      (make-array (length indexvec) :element-type t
						    :initial-element nil)))
		(loop for j below (length indexvec)
		      for i across indexvec
		      when (not (minusp i)) ;; a valid index
			do  (setf (aref truevec j) (aref vec i)))
		;;
		(return truevec)))
   ;; or throw error
   (if error-if-not-exist (error "Field ~A not found in ~A" field mcat))))
   

(defmethod get-value ((mcat merged-catalog) field i &key (error-if-not-exist t)
						(default-value nil))
  (block retblock
    
    (let ((val1  (get-value (merged-catalog-primary-catalog mcat) field i
			    :default-value '%no-field-in-catalog
			    :error-if-not-exist nil)))
	  ;; we allow NIL to be a valid returned field
	  (if (not (eq val1 '%no-field-in-catalog))
	      (return-from retblock val1)))
		 
    (loop for indexvec in (merged-catalog-index-vectors mcat)
	  for acat in (merged-catalog-merged-catalogs mcat)
	  for k = (gethash field (%map acat)) ;; the data vector
	  for datavec = (if k  (aref (astro-catalog-data acat) k))
	  for j = (aref indexvec i) 
	  do
	     (cond ((not k)
		    nil)	     ;; nothing to do for this catalog
		   ((minusp j) ;; field OK, but no data for this index I
		    (if error-if-not-exist
			(error "Field ~A index ~A  has no value in ~A in ~A"
			       field i mcat acat)
			(return-from retblock default-value)))
		   (t
		    (return-from retblock (aref datavec i))))
	  finally
	     (if error-if-not-exist
		 (error "Field ~A does not exist in any sub-catalog of ~A" field mcat)
		 (return-from retblock default-value)))))


(defmethod object-mag  ((mcat merged-catalog) mag-name i &key (error-if-not-exist t))
  (block retblock
    (multiple-value-bind (mag mag-err invalid)
	(object-mag (merged-catalog-primary-catalog mcat) mag-name i
		    :error-if-not-exist nil)
      (when mag
	(return-from retblock (values mag mag-err invalid))))
    
    (loop for indexvec in (merged-catalog-index-vectors mcat)
	  for acat in (merged-catalog-merged-catalogs mcat)
	  for j = (aref indexvec i)
	  for is-in-cat = (find mag-name (available-mags acat))
	  do (cond ((not is-in-cat)
		     nil) ;; ignore this catalog
		    ((minusp j) ;; if no mag, return T=INVALID for last field
		     (return-from retblock (values 99.0 99.0 t)))
		    (t
		     (return-from retblock (object-mag acat mag-name i))))
	  finally
	      (if error-if-not-exist
		  (error "Magnitude type ~A does not exist in any sub-catalog of ~A"
			 mag-name mcat)
		 (return-from retblock NIL)))))
    
(defmethod is-valid-field ((mcat merged-catalog) field)
  (member field (astro-catalog-fields mcat)))


(defmethod object-type ((mcat merged-catalog) i)
  (object-type (merged-catalog-primary-catalog mcat) i))

(defmethod object-ra ((mcat merged-catalog) i &key mjd)
  (object-ra (merged-catalog-primary-catalog mcat) i :mjd mjd))

(defmethod object-dec ((mcat merged-catalog) i &key mjd)
  (object-dec (merged-catalog-primary-catalog mcat) i :mjd mjd))

(defmethod object-ra-err ((mcat merged-catalog) i)
  (object-ra-err (merged-catalog-primary-catalog mcat) i))

(defmethod object-dec-err ((mcat merged-catalog) i)
  (object-dec-err (merged-catalog-primary-catalog mcat) i))

(defmethod object-id ((mcat merged-catalog) i)
  (object-id (merged-catalog-primary-catalog mcat) i))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((cat merged-catalog) stream)
  (format stream "#<merged-catalog: ~A~{+~A~} N=~A RA=~,4F Dec=~,4F R=~,3F deg>"
	  (type-of (merged-catalog-primary-catalog cat))
	  (mapcar 'type-of (merged-catalog-merged-catalogs cat))
	  (astro-catalog-n cat)
	  (ra-center cat)
	  (dec-center cat)
	  (radius-deg cat)))


(defun merge-different-catalogs (primary-catalog other-catalogs &key (tol/arcsec 0.5d0))
  "Given a PRIMARY-CATALOG and one more more OTHER-CATALOGS (a list or
an individual catalog), create a new merged-catalog object by matching
on RA,Dec.  Querying the catalog for a value will first query the
primary, and, if that fails, it will query the secondaries.

This is useful, for example, for correlating an optical and IR catalog
into a joint catalog, so that one can retrieve optical and IR mags
from the same catalog."

  (declare (type astro-catalog primary-catalog)
	   (type (or list astro-catalog) other-catalogs))
  
  (let* ((%other-catalogs (if (listp other-catalogs)
			      other-catalogs
			      (list other-catalogs)))
	 (all-cats (cons primary-catalog %other-catalogs))
	 (mcat (make-instance 'merged-catalog
			      :primary-catalog primary-catalog
			      :ra-center (ra-center primary-catalog)
			      :dec-center (dec-center primary-catalog)
			      :radius-deg (radius-deg primary-catalog)
			      :n (astro-catalog-n primary-catalog)
			      :merged-catalogs %other-catalogs
			      :default-position-error nil
			      :data nil
			      :fields (apply 'concatenate
				       'list
				       (mapcar 'astro-catalog-fields all-cats))
			      :available-mags (apply 'concatenate
					       'list
					       (mapcar 'available-mags all-cats))
			      :%map nil
			      :index-vectors
			      (loop with n = (astro-catalog-n primary-catalog)
				    for i below (length %other-catalogs)
				    collect (make-array n :element-type 'fixnum
							  :initial-element -1))))
	 (aobj-list-primary
	  (loop for i below (astro-catalog-n primary-catalog)
		collect (aobj:make-obj
			 :id i
			 :alpha (object-ra primary-catalog i)
			 :delta (object-dec primary-catalog i)))))
	;;
	(loop for ocat in %other-catalogs
	      for ovec in (merged-catalog-index-vectors mcat)
	      for aobj-list-other
		= (loop for i below (astro-catalog-n ocat)
			collect (aobj:make-obj
				 :id i
				 :alpha (object-ra ocat i)
				 :delta (object-dec ocat i)))
	      for obj-pairs = (aobj:pairwise-match-astro-objects
			       aobj-list-primary aobj-list-other :tol/arcsec tol/arcsec)
	      do (loop for pair in obj-pairs
		       do (setf (aref ovec (aobj:obj-id (first pair)))
				(aobj:obj-id (second pair)))))

    mcat))
				  
			 
					     
    
    
