
;; parent of SDSSn

(in-package astro-catalog)

(defclass sdss-catalog (astro-catalog)
  ( ;; the default position error for objects in sdss
   (defult-position-error :accessor default-position-error
     :initarg :default-position-error :initform #.(/ 0.10 3600d0))))



;; for sdss, objects that are not OK are marked by having identical
;; IDs, which indicates multple ID of a single object
(defmethod mark-astro-catalog-ok ((acat sdss-catalog))
  (when (astro-catalog-data acat)
    (add-astro-catalog-ok-column acat t)
    (let ((id-hash (make-hash-table :test 'equalp)))
      ;; fill hash with counts
      (loop for id across (get-astro-catalog-vector acat :id)
	    do (setf (gethash id id-hash)
		     (1+ (or (gethash id id-hash) 0))))
      ;; and mark those with more than 1 entry as bad
      (loop with ok-vec = (get-astro-catalog-vector acat :is-ok)
	    for i from 0
	    for id across (get-astro-catalog-vector acat :id)
	    do (when (>  (gethash id id-hash) 1)
		 (setf (aref ok-vec i) nil)))))
  acat)
	     
    
    
