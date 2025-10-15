
(in-package astro-catalog)

(defun list-objects-near-location (ra dec &key
			      (rmax 2.0)
			      (list-output nil)
			      (catalog-type  'astro-catalog:psps-3pi-mean-psf-mag-catalog))
  "Find the list of stars within RMAX arcsec of RA,DEC (either string
or decimal degree float) by retrieving a catalog.  If LIST-OUTPUT is
set, then return an assoc list.  Otherwise, print results.  The DIST
returned is the distance from RA,DEC given and object position."
  (let* ((ra/deg (if (stringp ra) (ra-dec:hms-string->deg ra) ra))
	 (dec/deg (if (stringp dec) (ra-dec:dms-string->deg dec) dec))
	 (cat 
	   (astro-catalog:get-cached-catalog-object 
	    ra/deg
	    dec/deg
	    (* 1/60 10.0) ;; catalog radius
	    catalog-type))
	 (outlist nil))
    (when (not list-output)
      (format t "Found catalog ~A ~%" cat)
      (terpri)
      (format t
	      "     ID              RA            DEC         dist(\")     g      r      i~%"))
	    
    (loop for i below (astro-catalog:astro-catalog-n cat)
	  for id = (astro-catalog:get-value cat :id i)
	  for rai  =(astro-catalog:get-value cat :ra i)
	  for deci = (astro-catalog:get-value cat :dec i)
	  for rmag = (astro-catalog:get-value cat :r i)
	  for gmag = (astro-catalog:get-value cat :g i)
	  for imag = (astro-catalog:get-value cat :i i)		   
	  for dist = (astro-coords:sky-angle ra/deg dec/deg rai deci
					     :units :arcsec)
	  when (< dist rmax)
	    do
	       (if (not list-output)
		   (format t 
			   "~15A ~A   ~A   ~4,1F      ~,2f  ~,2F  ~,2F~%"
			   id
			   (ra-dec:deg->hms-string rai)
			   (ra-dec:deg->dms-string deci)
			   dist gmag rmag imag)
		   (push `((:id . ,id) (:ra . ,rai) (:dec . ,deci) (:dist . ,dist)
			   (:g . ,gmag) (:r . ,rmag) (:i . ,imag))
			 outlist)))
    (when outlist
      (setf outlist (sort outlist 
			  '< 
			  :key 
			  (lambda (al) (cdr (assoc :dist al))))))
    outlist))
