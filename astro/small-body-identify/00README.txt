


================================================================

Code to identify which object in ASTORB and a supplementary comet
database is represented by an observation

Usage

(defparmater *mem* (sbid:make-sbid-mem)) ;; make a memoization object

;; create a list of  (COMET-ELEM DISTANCE INDEX RA DEC DELTA) sorted by
;; DISTANCE, the distance in arcsec from the RA-OBS,DEC-OBS given
(generate-candidates ra-obs dec-obs mjd-obs *mem*
		     :max-distance 10.0 ;; arcsec from observed
		     :observatory "MKO"
		     :perturb t
		     ... ) ;; other less used args

================================================================
METHODS

It takes about 460s on one CPU to brute-compute the positions of all known
asteroids with perturbations, and 134 seconds without.

However, 

1. it is FAST to compute PV from pertubed comet elements

2. it is SLOW to compute ra,dec from PV

so we generate a fast scheme to create a pre-computation
of a PV vector to RA,DEC for a given MJD.  Then for this MJD
we use this pre-computed structure to sweep through
a set of orbits, as follows:

1. compute PV of earth := PVe
2. on this MJD, compute the RA,Dec that two vectors centered on earth
   map to.   ie, in the earth based system, compute
   what RAi,Deci v1:=(1,0,0)+PVe and v2:=(0,1,0)+PVe map to.

   These RAi,Deci define vectors u1,u2 in the RA,Dec coordinate system.

   Compute a quaternion rotation from (v1,v2) to (u1,u2)

3. For any object, fast-compute PVo, then v:=PVe-PVo, then adjust v
   for light travel time using velocity component, then use quaternion
   to convert v to u, then convert u to ra,dec.
   


This code is accurate to a few arcseconds typically, but can be 2" off
for objects about 0.1AU from the earth.  It is *likely* that some of
the error is because the observatory location is not precessed/nutated
to J2000 before being added to PV of geocenter, in
%adjust-rearth-for-observatory.  For our purposes, this is probably
not worth fixing.

If we see the intial search as "good to 10 arcminutes" then it should
be OK.


The we have a SBID-MEM struct that contains one more more SBID-SNAP snapshots
of all the positions at some MJD, then a search uses (or makes)
a snapshot, and uses it for a rough search using heuristics for radius of how fast
an object might move in a day as a function of DELTA,ECC.

Returns a list of orbits and distances (see above)
