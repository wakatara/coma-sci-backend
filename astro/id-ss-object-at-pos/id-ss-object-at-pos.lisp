#|

Given an object name, RA, DEC, MJD, find an object that might plausibly be
close to that location using

1. parse object name  (unless degenerate)

2. matching common names

3. brute-force match all comets

3. fuzzy matching

eg, for an exmaple that made it to (3)

(id-ss-object-at-pos::find-matching-object "Kapf" *ra* *dec* *mjd*)

==>

#S(id-ss-object-at-pos::id-ret
   :id "22P"
   :match ("22P" "Kopff" :comet)
   :dist/arcmin 0.006286289254216347d0 
   :ra-obj 63.0488843353506d0
   :dec-obj 17.591720311360014d0
   :match-method "FuzzyMatch"
   :fuzzy-strength 0.60096776
  )


|#


(defpackage id-ss-object-at-pos
  (:use #:cl)
  (:export
   #:find-matching-object

   #:id-ret ;; the result structure
   #:id-ret-p
   #:id-ret-id
   #:id-ret-match
   #:id-ret-dist/arcmin
   #:id-ret-ra-obj
   #:id-ret-dec-obj
   #:id-ret-match-method
   #:id-ret-orbit
   #:id-ret-fuzzy-strength
   #:id-ret-fuzzy-matches
   ))

(in-package id-ss-object-at-pos)

;; return for ID
(defstruct id-ret
  id  ;; the final name, like "1" (for Ceres) or "2P"
  match ;; the full small-body-id like ("1P" "Halley" :comet)
  orbit ;; the orbit
  dist/arcmin ;; distance in arcsec from field center
  ra-obj  ;; ra,dec of best candidate
  dec-obj
  match-method   ;; the match method that succeeded
  fuzzy-strength) ;; NIL if not a fuzzy match



#|

for a matching orbit, create a new designation of our
usual form ("(1)" "Ceres" :asteroid) to allow for
provisional designations to turn into real objects

eg,
1. parse an object
2. retrieve it from JPL/MPC
3. [here] convert it back to our final deignation

JPL:
PANSTARRS (C/2017 K2)  -- we need to reverse it
238P/Read              -- this is OK
238 Hypatia (A884 NA)  -- need to strip out (A884 NA)

MPC:
C/2017 K2 (PANSTARRS)  -- this is OK
(1) Ceres              -- this is OK
238P/Read              -- this is OK

|#

(defun %convert-orbit-to-new-desig (orbit)
  (declare (type orbital-elements:comet-elem orbit))
  (let ((data (orbital-elements:comet-elem-data orbit)))
    (cond
      ;; asteroids
      ((typep data 'orbital-elements:asteroid-desc)
       (let* ((name (orbital-elements:asteroid-desc-name data))
	      (is-jpl (equalp (orbital-elements:asteroid-desc-source data) "JPL Horizons"))
	      (n1 (position #\( name))
	      (n2 (position #\) name)))
	 (when name
	   (or
	    ;; fix the JPL weird one
	    (when (and is-jpl n1 n2 (< n1 n2) (> n1 5)) ;; it's like "2 Pallas (A802 FA)"
	      (let* ((fixed-name (subseq name 0 n1))) ;; lea
		(small-body-name:parse-small-body-name fixed-name)))
	    ;; just parse it straight as is OK for MPC
	    (small-body-name:parse-small-body-name
	     (orbital-elements:asteroid-desc-name data))))))
      ;; comets
      ((typep data 'orbital-elements:comet-desc)
       (let* ((name (orbital-elements:comet-desc-name data))
	      (is-jpl (equalp (orbital-elements:comet-desc-source data) "JPL Horizons"))
	      (n1 (position #\( name))
	      (n2 (position #\) name)))
	 (when name
	   (or
	    ;; remove the extra (...) from JPL
	    (when (and is-jpl n1 n2 (< n1 n2))
	      (small-body-name:parse-small-body-name (subseq name (1+ n1) n2)))
	    ;; then the oridinary MPC
	    (small-body-name:parse-small-body-name name))))))))
	   
	
#|
Test function to look up an object on JPL Horizins (first) then MPC (second),
then check if the orbit is at RA,DEC,MJD with a tolerance of DIST-TOL-ARCMIN

Returns (VALUES YES? RAOBJ DECOBJ NEW-DESIG ORBIT) where NEW-DESIG, if not nil,
is a new designation of our usual form ("1" "Ceres" :asteroid)
|#

;; jpl orbit cache with larger span
(defvar *jpl-orbit-cache* (jpl-horizons:build-jpl-orbit-cache :day-span 365))
(defvar *mpc-orbit-cache* (mpc:build-mpc-orbit-cache :day-span 365))
(defun could-it-be-desig (desig ra dec mjd &key (observatory "geocenter")
					     (dist-tol-arcmin 10)
					     ;; optionally provide an orbit
					     ;; instead of using JPL
					     (orbit nil))
  (declare (type string desig)
	   (type double-float ra dec mjd))
  (let* ((jpl-orbit (when (not orbit)
		      (jpl-horizons:get-jpl-horizons-elements-with-caching
		       desig :mjd mjd :sleep-time 1.0 :jpl-orbit-cache *jpl-orbit-cache*)))
	 (mpc-orbit (when (and (not orbit) (not jpl-orbit))
		      (mpc:get-mpc-elements-with-caching
		       desig
		       :sleep-time 1.0
		       :mpc-orbit-cache *mpc-orbit-cache*)))
	 (elem (or orbit jpl-orbit mpc-orbit)))
    (when elem
      (multiple-value-bind (raobj decobj)
	  (slalib-ephem:compute-radecr-from-comet-elem-for-observatory
	   elem
	   mjd observatory)
	(declare (type double-float raobj decobj))
	(let* ((dist (astro-coords:sky-angle
		      ra  dec
		      raobj decobj :units :arcmin))
	       (yes? (<= dist dist-tol-arcmin)))
	  (values yes? dist raobj decobj
		  (%convert-orbit-to-new-desig elem)
		  elem))))))
	   

;; returns (values yes? dist ra-landolt dec-landolt ("92 235" "SA 92-235" :LANDOLT-STAR))
(defun could-it-be-landolt (desig ra dec mjd &key (dist-tol-arcmin 10))
  (let ((%name (remove-if-not #'alphanumericp desig))
	(landolt+dist-list
	  (landolt:find-nearest-landolt-stars
	   ra dec (* 60d0 dist-tol-arcmin) :mjd mjd)))
    (block done
      ;;
      (when (not landolt+dist-list)
	(return-from done nil))
      ;;
      (flet ((return-landolt (landolt dist/arcsec)
	       (return-from done
		 (values t
			 (/ dist/arcsec 60)
			 (landolt:landolt-star-ra landolt)
			 (landolt:landolt-star-dec landolt)
			 (list
			  (landolt:landolt-star-simbadname landolt)
			  (landolt:landolt-star-name landolt)
			  :landolt-standard)))))
	;; search for one in the region with a soft-matching name
	(loop for (landolt dist/arcsec) in landolt+dist-list
	      for lname = (landolt:landolt-star-name landolt)
	      for %lname = (remove-if-not #'alphanumericp lname)
	      for slname = (landolt:landolt-star-simbadname landolt)
	      for %slname = (remove-if-not #'alphanumericp slname)
	      ;; do we match using substring, excluding non-alphanum chars,
	      ;; using both simbadname and short name
	      for is-match = (or (search %name %lname)
				 (search %name %slname))
	      when is-match
		do (return-landolt landolt dist/arcsec)
	      finally
		 ;; if none matches the name, return closest one to ra,dec
		 (return-landolt
		  (first (first landolt+dist-list)) 
		  (second (first landolt+dist-list))))))))


;; apply some plausible heuristics to the small body names available
;; in small-body-name package
(defun list-plausible-matches (name)
  (declare (type string name))
  (let ((sname ;; stripped name
	  (string-upcase (remove-if-not 'alphanumericp name)))
	(asname ;; alphabet only, stripped name
	  (string-upcase (remove-if-not 'alpha-char-p name))))
	  
    (labels (;;  is any full comet desig lurking within it, with a possible p or c added?
	     ;;  eg 1998VS4Vband90s is matched by "P/1998 VS24"
	     ;;  eg "455P10s" is matched by "455P" and "55P" (but there is no "5P")
	     (contains-comet-desig? ()
	       (let ((outlist nil)
		     (cname (concatenate 'string "C" sname))
		     (pname (concatenate 'string "P" sname)))
		 (small-body-name:iterate-over-small-bodies
		  (lambda (body)
		    (let* ((id (first body))
			   (bname (remove-if-not #'alphanumericp id)))
		      (when (or (search bname sname)
				(search bname cname)
				(search bname pname))
			(push body outlist))))
		  :do-comets t :do-asteroids nil)
		 outlist))	     
	     ;;
	     ;; eg 1566blurgIckarus ==> 
	     (asteroid? ()
	       (let ((nnn (parse-integer sname :junk-allowed t)))
		 (when nnn
		   (list (small-body-name:get-body-by-id (format nil "~D" nnn))))))
	     ;;
	     ;; 1998AB12 gets turned into "1998 ab" "1998 ab1" "1998 ab12"
	     (provisional? ()
	       (let ((yyyy (parse-integer sname :junk-allowed t)))
		 (when (and yyyy (<= 1900 yyyy 2050))
		   (loop for i from 4 below (length sname) ;; after the year
			 for j below 5 ;; don't do more than "1996 ab123"
			 for c = (aref sname i)
			 ;; bail if any letter is in violation of rule AB123
			 until (or (and (< j 2) (not (alpha-char-p c))) ;; first two not letters
				   (and (>= j 2) (not (digit-char-p c)))) ;; others not numbers
			 ;; collect after after first two cars AB,AB1,AB12,AB123
			 when (>= j 2) ;; need at least 2 letters after year
			   collect (list (format nil "~A ~A" yyyy (subseq sname 4 (1+ i)))
					 nil :provisional-desig)))))
	     ;; 
	     ;; does name contain any common name, or does common name
	     ;; contain name (if name is longer than 4 chars)
	     (contains-common? ()
	       (let ((outlist nil))
		 (small-body-name:iterate-over-small-bodies
		  (lambda (body)
		    (let ((common-name (second body)))
		      (when common-name
			(let ((scommon (remove-if-not 'alpha-char-p common-name)))
			  (when (or (search scommon asname :test 'equalp)
				    (and (>= (length asname) 4)
					 (search asname scommon :test 'equalp)))
			    (push body outlist)))))))
		 outlist))
	     ;; 
	     )

      (progn;remove-duplicates
       (remove nil
	       (append
		(contains-comet-desig?)
		(asteroid?)
		(provisional?)
		(contains-common?)
		)
	       :test 'equalp :key 'first)))))



;; list objects that contain a shortened version of
;; a solar system body (to length NSHORTEN)
(defun list-containing-shortened-common-name (name nshorten
					      &key (allow-asteroids t) (allow-comets t))
					      
  (declare (type string name))
  (let ((asname ;; alphabet only, stripped name
	  (string-upcase (remove-if-not 'alpha-char-p name)))
	(outlist-comets nil)
	(outlist-asteroids nil))
    (flet ((shorten (str)
	     (declare (type string str))
	     (if (<= nshorten (length str))
		 (subseq str 0 nshorten)
		 str)))
      (small-body-name:iterate-over-small-bodies 
       (lambda (body)
	 (let ((cname (second body))
	       (type  (third body)))
	   (when (and cname
		      (search (shorten cname) asname :test #'equalp))
	     (when (and allow-asteroids (eq type :asteroid))
	       (push body outlist-asteroids))
	     (when (and allow-comets (eq type :comet))
	       (push body outlist-comets))))))
      (append outlist-comets outlist-asteroids))))
	    
	  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; caching of successful hits
;; KEY=NAME  VAL=(list MATCH ORBIT)
(defvar *match-cache* (jk-cache:build-cache
		       :expire-time (* 24 3600 180) ;; foreever
		       :nmax 10000))
(defun cache-good-match (name match orbit)
  (jk-cache:cache-item name (list match orbit) *match-cache*))
(defun retrieve-cached-match (name) ;; return only the match, not the orbit
  (first (jk-cache:retrieve-item name *match-cache*)))

;; return ((MATCH ORBIT) (MATCH ORBIT) ....)
(defun get-list-of-past-good-matches ()
   (mapcar 'cdr
	   (jk-cache:list-entries *match-cache*)))
			 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
 
(defun find-matching-object (name ra dec mjd
			     &key
			       (fuzzy-match-threshold 0.30)
			       (dist-tol-arcmin 10)
			       (verbose nil)
			       (match-comets t)
			       (match-asteroids t)
			       (match-landolt-standards t)
			       (brute-force-comets t)
			       (try-all-previous-match-orbits t)
			       (observatory "geocenter"))
  "Find a matching solar system object for an observation using NAME at
RA,DEC,MJD in the following order:

1. An exact name parsing using SMALL-BODY-NAME:PARSE-SMALL-BODY-NAME
2. Using all matching common names (eg Kopff is a comet, and an asteroid)
3. Optionally, try matching a standard star if MATCH-LANDOLT-STANDARDS is true.
4. List a 'plausible matches' with mutual inclusion of some aspect
   of a known name.
5. Optionally (TRY-REMEMBERED-PAST-ORBITS) try all cached orbits that gave
   a match.
6. BRUTE-FORCE-COMETS if requested, trying all comets using BRUTE-FORCE-COMETS
   package, which contains saved comet orbits.
6. Generate a list of fuzzy-matches up to FUZZY-MATCH-THRESHOLD, and see
   whether any of these objects are at the rigth RA,DEC,MJD.

by default both :MATCH-COMETS an :MATCH-ASTEROIDS is T

TRY-PAST-REMEMBERED-ORBITS attempts to use all the previous orbits
that produced a match before fuzzy-matching."
  
  (let* ((split-name  (string-utils:split-string name "012345679 "))
	 (name0 (first split-name)) ;; eg in "Halley12siband" ==> "Halley"
	 (common-matches (append
			  (small-body-name:find-matching-common-names
			   name)
			  ;; remove any non-letters for cases like 1566Icarus
			  (small-body-name:find-matching-common-names
			   (remove-if-not #'alpha-char-p name))))
	 (best-match (small-body-name:parse-small-body-name
		      name))
	 (best-match-is-degenerate
	   (and best-match
		(eq (third best-match) :degenerate)))
	 (tried-it-already-hash (make-hash-table :test 'equalp)))

    (block done
      (labels
	  (;; function to finish off a good match, and return
	   (finish (match ;; match is ("1" "Halley" :comet) eg
		       dist ra-obj dec-obj fuzzy-strength
		     &key orbit method)
	      (when (not (eq (third match) :landolt-standard))
		(cache-good-match name match orbit))
	     (return-from done
	       (make-id-ret :id (first match)
			    :match  match
			    :match-method method 
			    :dist/arcmin dist
			    :fuzzy-strength  fuzzy-strength
			    :ra-obj  ra-obj
			    :dec-obj dec-obj
			    :orbit orbit)))
	   ;; test this match candidate using orbit, and
	   ;; call FINISH to return if it is good
	   (test-match (match &key fuzzy-strength method orbit)
	     (let  ((type (third match)))
	       (when (not (gethash match tried-it-already-hash))
		 (when verbose
		   (format verbose "Testing ~S~A~A~%" match
			   (if method (format nil " method=~A" method) "")
			   (if fuzzy-strength
			       (format nil " fuzzmatch=~A" fuzzy-strength)
			       "")))
		 (setf (gethash match tried-it-already-hash) t)
		 (when (or (and match-comets (eq type :comet))
			   (and match-asteroids (eq type :asteroid))
			   ;; allow third possibility
			   (not  (member type '(:comet :asteroid))))
		   (multiple-value-bind (yes? dist ra-obj dec-obj new-match orbit)
		       (ignore-errors
			(could-it-be-desig
			 (first match)
			 (float ra 1d0) (float dec 1d0) (float  mjd 1d0)
			 :observatory observatory
			 :dist-tol-arcmin dist-tol-arcmin
			 :orbit orbit))
		     (when yes?
		       ;; return the match
		       (finish (or new-match match) dist ra-obj dec-obj fuzzy-strength
			       :orbit orbit :method method))))))))
	

	;; see if we had this exact name before
	(let ((cached-match (retrieve-cached-match name)))
	  (when cached-match
	    (test-match cached-match :method "CachedMatch")))
	
	;; is the simple best match the one
	(when (and best-match (not best-match-is-degenerate))
	  (test-match best-match :method "SimpleParse"))

	;; cycle through exact common matches
	(dolist (match common-matches)
	  (test-match match :method "ExactCommonMatch"))

	;; try landolt standards
	(when match-landolt-standards 
	  (multiple-value-bind (yes? dist ra-obj dec-obj match)
	      (could-it-be-landolt name ra dec mjd :dist-tol-arcmin dist-tol-arcmin)
	    (when yes?
	      (when verbose (format verbose "Matched Landolt Standard ~S~%" match))
	      (finish match dist ra-obj dec-obj nil :method "LandoltStandard"))))

	;; cycle through plausible matches
	(let ((plausible-matches (list-plausible-matches name)))
	  (when (< (length plausible-matches) 200) ;; safety
	    (loop for match in plausible-matches
		  do (test-match match :method "NameOverlap"))))

	;; cycle through shortened matches
	(let ((shortened-matches3
		(list-containing-shortened-common-name
		 name 3 :allow-asteroids nil :allow-comets t))
	      (shortened-matches4
		(list-containing-shortened-common-name
		 name 4 :allow-asteroids t :allow-comets t)))
	  (when (< (length shortened-matches3) 200) ;; safety
	    (loop for match in shortened-matches3
		  do (test-match match :method "ShortenedCometCommon3")))
	  (when (< (length shortened-matches4) 200) ;; safety
	    (loop for match in shortened-matches4
		  do (test-match match :method "ShortenedCommon4"))))
	
	;; cycle through shortened plausible matches (taking first 4 of sname)
	(let ((sname (remove-if-not #'alpha-char-p name)))
	  (when (>= (length sname) 4)
	    (let ((plausible-matches (list-plausible-matches (subseq sname 0 4))))
	      (when (< (length plausible-matches) 200) ;; safety
		(loop for match in plausible-matches
		      do (test-match match :method "ShortenedNameOverlap"))))))

	;; try all objects that matched previously
	(when try-all-previous-match-orbits
	  (loop with past-match+orbits = (get-list-of-past-good-matches)
		for (match orbit) in past-match+orbits
		do (test-match match :method "PastMatchOrbit" :orbit orbit)))

	;; try all numbered comets using BRUTE-FORCE-COMETS package
	(when brute-force-comets
	  (let ((comet-candidates (brute-force-comets:brute-force-search-comets
				ra dec mjd :tol/arcmin 120))) ;; lots of potential error
	    (loop for triplet in comet-candidates ;; ("NAME" dist/arcmin YEAR-OF-ORBIT)
		  for comet-name = (first triplet)
		  for match = (list comet-name nil :comet)
		  do (test-match match :method "BruteForceComets"))))
	

	;; cycle through fuzzy matches of first string (NAME0)
	(when (>= (length name0) 3)
	  (let ((fuzzy-matches (small-body-name:fuzzy-match-common-name
				name0 :match-threshold fuzzy-match-threshold
				      :comets-first t)))
	    (loop for (match fuzzy-strength) in fuzzy-matches
		  for i below 200 ;; safety
		  do  (test-match match
				  :fuzzy-strength fuzzy-strength
				  :method "FuzzyMatch0"))))
	
	;; cycle through the fuzzy matches of the whole string
	(let ((fuzzy-matches (small-body-name:fuzzy-match-common-name
			      name :match-threshold fuzzy-match-threshold
				   :comets-first t)))
	  (loop for (match fuzzy-strength) in fuzzy-matches
		for i below 200 ;; safety
		do  (test-match match
				:fuzzy-strength fuzzy-strength
				:method "FuzzyMatch"))))
      ;; no match
      (return-from done nil)
      )))

	
(defun test-find-matching-object (object recovery-name  &key
							  (mjd 58849.0d0)
							  (fuzzy-match-threshold 0.30)
							  (verbose nil))
  "Test FIND-MATCHING-OBJECT by getting orbit for OBJECT, computing RA,DEC at MJD,
and trying to find a match using RECOVERY-NAME"
  (let ((elem (or (mpc:get-mpc-elements object)
		  (error "Cannot get object ~A from MPC" object))))
    (multiple-value-bind (ra dec)
	(slalib-ephem:compute-radecr-from-comet-elem-for-observatory
	 elem mjd "geocenter")
      (find-matching-object recovery-name ra dec mjd :fuzzy-match-threshold fuzzy-match-threshold
			    :verbose verbose))))
