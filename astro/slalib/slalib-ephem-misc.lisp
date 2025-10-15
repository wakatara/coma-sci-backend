#|

misc routines that use slalib-ephem

|#

(in-package slalib-ephem)


(defun could-it-be-this-observatory?
    (ra dec mjd observatory
     &key airmass hour-angle alt az sidereal-time
       (airmass-tol 0.03)
       (alt-tol 2.0)
       (az-tol  2.0)
       (hour-angle-tol 0.05)
       (sidereal-time-tol 0.05))
  "Could the observation have been taken at this OBS given these
 RA,DEC,MJD (mandatory) and constraints of airmass, hour-angle, alt,
 az, and sidereal-time, with associated tolerances."
  (let* ((obs (observatories:get-observatory observatory))
	 (airmass%  (if airmass
		       (compute-airmass-for-ra-dec-for-observatory
			ra dec mjd obs)))
	 ;;
	 (altaz%  (if (or alt az)
		      (multiple-value-list
		       (altaz-for-apparent-radec-for-observatory
			ra dec mjd obs))))
	 (alt%  (if alt (first altaz%)))
	 (az%   (if az  (second altaz%)))
	 ;;
	 (hour-angle%  (if hour-angle
			   (slalib:compute-hour-angle-for-ra
			    ra mjd
			    (- (observatory-wlongitude obs)))))
	 (sidereal-time%  (if sidereal-time
			       (slalib:compute-local-sidereal-time  
				mjd (- (observatory-wlongitude obs)))))
	
	 (airmass-ok  (if airmass
			   (< (abs (- airmass airmass%)) airmass-tol)
			   t))
	 (alt-ok  (if alt
		      (< (abs (- alt alt%)) alt-tol)
		      t))
	 (az-ok  (if az ;; AZ wraps around
		     (or (< (abs (- az az%     )) az-tol)
			 (< (abs (- az az% +360)) az-tol)
			 (< (abs (- az az% -360)) az-tol))
		     t))
	 (ha-ok  (if hour-angle
		     (< (abs (- hour-angle hour-angle%)) hour-angle-tol)
		     t))
	 (sidereal-time-ok
	   (if sidereal-time
	       (< (abs (- sidereal-time sidereal-time%)) sidereal-time-tol)
	       t)))
    (when (and airmass-ok alt-ok az-ok ha-ok sidereal-time-ok)
      (remove
       nil
       (list
	obs
	(if airmass
	    (list :airmass airmass% :err (- airmass airmass%)))
	(if alt
	    (list :alt alt% :err (- alt alt%)))
	(if az
	    (list :az az% :err (- az az%)))
	(if hour-angle
	    (list :hour-angle hour-angle% :err (- hour-angle hour-angle%)))
	(if sidereal-time
	    (list :sidereal-time sidereal-time%
		  :err (- sidereal-time sidereal-time%))))))))
  


;; figure out which obsvervatory an observation could have been taken from
(defun which-observatory-could-it-be?
    (ra dec mjd
     &key airmass hour-angle alt az sidereal-time
       (obs-list observatories:*obs-list*)
       (airmass-tol 0.03)
       (hour-angle-tol 0.05)
       (alt-tol 2.0)
       (az-tol  2.0)
       (sidereal-time-tol 0.05))
  "Figure out which observatory at RA,DEC,MJD an image could
be taken from, using AIRMASS HOUR-ANGLE ALT AZ as constraints"
  (when (not (or airmass hour-angle alt az sidereal-time))
    (error "No constraint in set {AIRMASS HOUR-ANGLE ALT AZ SIDEREAL-TIME} was given."))
  (loop for observatory in obs-list
	for result = (could-it-be-this-observatory?
		      ra dec mjd observatory
		      :airmass airmass :airmass-tol airmass-tol
		      :hour-angle hour-angle :hour-angle-tol hour-angle-tol
		      :alt alt :alt-tol alt-tol
		      :az  az  :az-tol az-tol
		      :sidereal-time-tol sidereal-time-tol)
	when result collect result))

							
			
  
