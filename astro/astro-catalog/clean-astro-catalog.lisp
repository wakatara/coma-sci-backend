
;; general methods to clean an astro catalog to make it suitable
;; for photomery/astrometry


(in-package astro-catalog)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add another column of IS-OK to catalog, T by default

(defgeneric mark-astro-catalog-ok (astro-catalog)
  (:documentation "Add a column :IS-OK, which is T where a catalog is OK"))


(defun add-astro-catalog-ok-column (acat &optional (default-value t))
  (when (gethash :is-ok (%map acat))
    (error "Catalog already has IS-OK column"))
  (when (astro-catalog-data acat)
    (loop with data = (astro-catalog-data acat)
	  with nd = (length data)	 ;; number of cols
	  with nc = (length (aref data 0)) ;; number of rows
	  with data-new = (make-array (1+ nd))
	  with ok-column = (make-array nc :initial-element default-value)
	  for i below nd do (setf (aref data-new i) (aref data i))
	  finally (setf (aref data-new nd) ok-column)
		  (setf (astro-catalog-fields acat)
			(append (astro-catalog-fields acat) (list :is-ok)))
		  (setf (astro-catalog-data acat) data-new)
		  (setf (gethash :is-ok (%map acat)) nd)
		  (return ok-column))))

;; for generic catalog, say that all entries are OK unless overriden
(defmethod mark-astro-catalog-ok ((acat astro-catalog))
  (add-astro-catalog-ok-column acat)
  acat) ;; return ACAT - important

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  purge-astro-catalog-by-is-ok removes all the objects that
;;  don't have IS-OK set

(defgeneric purge-astro-catalog-by-is-ok (astro-catalog)
  (:documentation "Remove all objects with IS-OK = NIL and return
a NEW catalog.  Original catalog is unchanged."))

(defmethod purge-astro-catalog-by-is-ok ((acat astro-catalog))
  (loop
    with data = (astro-catalog-data acat)
    with nd = (length (astro-catalog-data acat))
    with ok-vec = (get-astro-catalog-vector acat :is-ok)
    with ntot = (length ok-vec)
    with nok = (count-if 'identity ok-vec)
    with data-new = (map 'vector
			 (lambda (v)
			   (make-array nok :element-type
				       (array-element-type v)))
			 data)
    with k = 0
    for i below ntot
    for ok across ok-vec
    when ok
      do
	 (loop for id below nd
	       do (setf (aref (aref data-new id) k)
			(aref (aref data id) i)))
	 (incf k)
    finally
       ;; make a new copy
       (return (make-instance (class-name (class-of acat))
			      :n nok
			      :ra-center (ra-center acat)
			      :dec-center (dec-center acat)
			      :radius-deg (radius-deg acat)
			      :data data-new
			      :fields (astro-catalog-fields acat)
			      :available-mags (available-mags acat)
			      :default-position-error (default-position-error acat)
			      :%map  (%map acat)))))

