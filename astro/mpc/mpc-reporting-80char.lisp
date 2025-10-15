
;; reporting of mpc objects
;; see https://minorplanetcenter.net/iau/info/TechInfo.html


(in-package mpc)


;; https://minorplanetcenter.net/iau/info/PackedDes.html#perm
(defun %pack-minor-planet-number (num)
  (let ((chars-a-z
	  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
	(chars-0-z
	  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
    (cond ((< num 100000)
	   (format nil "~5,'0D" num))
	  ((< num 619999)
	   (let* ((mod10000 (mod num 10000))
		  (div10000 (floor num 10000))
		  (char (aref chars-a-z  (- div10000 10))))
	     (format nil "~A~4,'0D" char mod10000)))
	  ((< num 15396336) ;; above 619999 but below max representation
	   (let* ((n0 (- num 620000))
		  (c4 (aref chars-0-z (mod n0 62)))
		  (n1 (floor n0 62))
		  (c3 (aref chars-0-z (mod n1 62)))
		  (n2 (floor n1 62))
		  (c2 (aref chars-0-z (mod n2 62)))
		  (n3 (floor n2 62))
		  (c1 (aref chars-0-z n3)))
	     (format nil "~A~A~A~A~A" "~" c1 c2 c3 c4)))))) ;; yes, "~" char
	   

(defun %build-namestr-for-asteroid (minor-planet-number
				    provisional-desig
				    is-discovery)
  (when (> (length provisional-desig) 7)
    (error "PROVISONAL-DESIG = ~A cannot be longer than 7 characters"
	   provisional-desig))
  
  (concatenate
   'string
   (if minor-planet-number
       (%pack-minor-planet-number minor-planet-number)
       "     ")
   (format nil "~7A"  (or provisional-desig ""))
   (if is-discovery "*" " ")))

(defun %build-namestr-for-comet (periodic-comet-number
				 comet-orbit-type
				 provisional-desig)
  (when (not (member comet-orbit-type '("P" "C" "D" "X" "A") :test 'string=))
    (error "COMET-ORBIT-TYPE = ~A is not one of PCDXA" comet-orbit-type))
  (when (> (length provisional-desig) 7)
    (error "PROVISONAL-DESIG = ~A cannot be longer than 7 characters"
	   provisional-desig))
  (concatenate
   'string
   (if periodic-comet-number
       (format nil "~4,'0D" periodic-comet-number)
	     "    ")
   comet-orbit-type
   (format nil "~7A"  (or provisional-desig ""))
   " "))

;; turn a symbol or band into an MPC one-letter symbol, failing
;; on bands MPC does not accept
(defun parse-mpc-photometric-band (band)
  (cond ((member band
		 '("V" "R" "I" "J" "W" "U" "C" "L" "H" "K" "Y"
		   "G" "g" "r" "i" "w" "y" "z" "o" "c" "v" "u") ;; as of May 2023
		 :test 'string=)
	 band)
	(t
	 (cadr (assoc
		    band
		    '((:uj "U") (:vj "V") (:rc "R") (:ic "I")
		      (:usdss "u") (:gsdss "g") (:rsdss "r") (:isdss "i")
		      (:zsdss "z") (:ysdss "y")
		      (:gri-cfht-megacam "g") ;; fake it
		      ))))))



(defun format-mpc-submission-line
  (&key
     mjd ra dec mag band observatory-code
     (high-precision nil) ;; 3 digits for ra, 2 for dec
     ;; RA-ERR and DEC-ERR (arcsec) and TIME-ERR (sec) can be specified in COM line;
     ;; useful for FINDORB
     ra-err dec-err time-err mag-err
     ;;
     object-type  ;; :asteroid or :comet
     is-discovery 
     periodic-comet-number 
     minor-planet-number 
     provisional-desig 
     comet-orbit-type 
     note1 
     note2
     (precise-time nil);; have permission to submit precise time
     (precise-mag nil)
     )

  (when (not mjd) (error "format-mpc-submission-line: MJD not given"))
  (when (not ra) (error "format-mpc-submission-line: RA not given"))
  (when (not dec) (error "format-mpc-submission-line: DEC not given"))
  (when (not band) (error "format-mpc-submission-line: BAND not given"))
  (when (not mag) (error "format-mpc-submission-line: MAG not given"))
  (when (not observatory-code)
    (error "format-mpc-submission-line: OBSERVATORY-CODE not given"))
  (when (not (member object-type '(:asteroid :comet)))
    (error "OBJECT-TYPE must be one of (:asteroid :comet)"))
  (when (not (parse-mpc-photometric-band band))
    (error "format-mpc-submission-line: Band ~A is not an MPC recognized 
photometric band" band))

  (when (and (or ra-err dec-err) (not (and ra-err dec-err)))
    (error "Only one of RA-ERR=~A and DEC-ERR=~A is given.  Both must be given."
	   ra-err dec-err))

  (let ((allowed-note1 "AaBbCcDdEFfGgHhIjJjKkMmNoOPpRrSsTtUuVWw")
	(allowed-note2 "PeCTMVvRrSscEOHNn"))
    (when (and (stringp note1) (= (length note1) 1)
	       (not (find (aref note1 0) allowed-note1)))
      (error "format-mpc-submission-line: NOTE1=~A not one of [~A]"
	     note1 allowed-note1))
    (when (and (stringp note2) (= (length note2) 1)
	       (not (find (aref note2 0) allowed-note2)))
      (error "format-mpc-submission-line: NOTE2=~A not one of [~A]"
	     note1 allowed-note2)))
  
	
  (concatenate
   'string
   ;; if ra-err,dec-err,time-err, or mag-err are given, create comment lines
   (if ra-err
	(format nil "COM Posn sigma ~,2F ~,2F~%" ra-err dec-err)
	"")
   (if time-err
	(format nil "COM Time sigma ~,2F~%" time-err)
	"")
   (if mag-err
	(format nil "COM Mag sigma ~,2F~%" mag-err)
	"")
     
   ;; first the name
   (cond
     ((equalp object-type :asteroid)
      (%build-namestr-for-asteroid minor-planet-number provisional-desig
				   is-discovery))
     (t
      (%build-namestr-for-comet periodic-comet-number comet-orbit-type
				provisional-desig)))
   ;;
   (or note1 " ")
   (or note2 " ")
   ;;;;; different 
   (astro-time:mjd-to-decimal-date-string
    mjd
    :ndecimal (if precise-time 6 5)
    :separator " ")
   (if precise-time "" " ")
   ;;
   ;;
   ;; output ra,dec to precision requested
   (ra-dec:deg->hms-string ra :rounding (if high-precision 3 2) :separator-strings #(" " " " ""))
   (if high-precision "" " ") ;; extra pad because we don't give 3 digits of precision
   (ra-dec:deg->dms-string dec :rounding (if high-precision 2 1) :separator-strings #(" " " " ""))
   (if high-precision "" " ") ;; extra pad because we don't give 2 digits of precision

   "         " ;; 9 spaces, for internal MPC use only

   ;; 
   (if precise-mag
       (format nil "~5,2F" mag)  ;; 2 decimal if permission
       (format nil "~4,1F " mag)) ;; else 1 decimal
   (parse-mpc-photometric-band band)
   "      " ;; 7 spaces
   (format nil "~3A" observatory-code)))
   
   
;; see https://minorplanetcenter.net/iau/info/OpticalObs.html
(defun make-mpc-optical-submission
    (&key
       (contact-info "-- contact name --") 
       (contact-email "contactemail@someplace.org")
       (observer-list '("Observer1" "Observer2" "..."))
       (measurer-list '("Measurer1" "Measurer2" "..."))
       (telescope-info "-- correct telescope description --")
       (catalogs-used-list '("catalog1" "catalog2" "..."))
       (acknowledge-info    "-- info to be mailed back --")
       (acknowledge-email-list '("contactemail@someplace.org" "..."))
       ;; information for the astrometry line
       mjd ra dec mag band  ;; can be vectors
       ra-err dec-err time-err ;; optional; go into COM line
       observatory-code
       object-type ;; :asteroid or :comet
       is-discovery
       periodic-comet-number
       minor-planet-number
       provisional-desig 
       comet-orbit-type
       ;; obs notes - https://minorplanetcenter.net/iau/info/ObsNote.html
       note1 
       (note2 "C") ;; for CCD
       (comment nil)
       )

"Create text for an MPC submission.  All arguments are keywords
but certain values and combinations are obligatory.

- MJD, RA, DEC, MAG, BAND may be single values or vectors.
   BAND must be one of V,R,I,J,W,U,C,L,H,K,Y,G,g,r,i,w,y,z,o,c,v,u or one of our normal 
   symbols like :GSDSS
- OBSERVATORY-CODE must be given; if a vector, it is PER OBSERVATION
- OBJECT-TYPE must be one of :ASTEROID or :COMET
- IS-DISCOVERY is T if this is a new discovery
- PERIODIC-COMET-NUMBER, MINOR-PLANET-NUMBER, or PROVISIONAL-DESIG
   should be given to construct the object's name
- COMET-ORBIT-TYPE should be one of strings P,C,D,X,A if it's a comet
"

  (when (and (or ra-err dec-err) (not (and ra-err dec-err)))
    (error "Only one of RA-ERR=~A and DEC-ERR=~A is given.  Both must be given."
	   ra-err dec-err))
  (with-output-to-string (s)
    (format s "COD ~A~%" (if (stringp observatory-code)
			     observatory-code
			     (aref observatory-code 0)))
    (when contact-info (format s "CON ~A~%" contact-info))
    (when contact-email (format s "CON ~A~%" contact-email))
    (format s "OBS ~{~a~^, ~}~%" observer-list)
    (format s "MEA ~{~a~^, ~}~%" measurer-list)
    (format s "TEL ~A~%" telescope-info)
    (format s "NET ~{~a~^, ~}~%" catalogs-used-list)
    (when acknowledge-info (format s "ACK ~A~%" acknowledge-info))
    (when acknowledge-email-list
      (format s "AC2 ~{~a~^, ~}~%" acknowledge-email-list))
    (terpri s)
    (when comment (format s "COM ~A~%" comment))

    ;; all of inputs must be vectors or a single object
    (let ((input-list
	    (append (list mjd ra dec mag band)
		    (if ra-err (list ra-err) nil)
		    (if dec-err (list dec-err) nil)
		    (if time-err (list time-err) nil))))
    (when (not (or
	        (every (lambda (x) (or (numberp x) (stringp x) (symbolp x)))
		       input-list)
		;;
		(and (every 'vectorp input-list)
		     (apply '=
			    (mapcar 'length input-list)))))
      (error "MJD,RA,DEC,MAG,BAND,RA-ERR,DEC-ERR,TIME-ERR must be atomic, or must be equal length vectors")))

      


    (let* ((atoms (not (vectorp mjd)))
	   (n (if atoms 1 (length mjd)))
	   (obs-codes (if (and (vectorp observatory-code)
			       (not (stringp observatory-code)))
			  observatory-code
			  (make-array n :initial-element observatory-code) )))
      (flet ((vectorify (input)
	       (cond ((not input) ;; special case like ra-err that can be NIL
		      (make-array n :initial-element nil))
		     (atoms ;; an atomic input that is turned into #(input)
		      (vector input))
		     (t     ;; a vector input kept as-is
		      input)))) 
      (map 'vector
	   (lambda (mjd ra dec band mag ra-err dec-err time-err obs-code)
	     (format s "~A~%"
		     (format-mpc-submission-line
		      :mjd mjd :ra ra :dec dec
		      :ra-err ra-err :dec-err dec-err :time-err time-err
		      :band band :mag mag
		      :observatory-code obs-code
		      :object-type object-type :is-discovery is-discovery
		      :periodic-comet-number periodic-comet-number
		      :minor-planet-number minor-planet-number
		      :provisional-desig provisional-desig
		      :comet-orbit-type comet-orbit-type
		      :note1 note1 :note2 note2)))
	   (vectorify mjd)
	   (vectorify ra)
	   (vectorify dec)
	   (vectorify band)
	   (vectorify mag)
	   (vectorify ra-err)
	   (vectorify dec-err)
	   (vectorify time-err)
	   obs-codes)))))

#|

Some tests

 (mpc:make-mpc-optical-submission
	  :minor-planet-number 99 :object-type :asteroid
	  :mjd 55555.550 :ra 11.11d0 :dec -33.3333d0 
	  :ra-err 0.1 :dec-err 0.2 :time-err 1.2
	  :band "V" :mag 20.0 :observatory-code "568")

 (mpc:make-mpc-optical-submission
	  :minor-planet-number 99 :object-type :asteroid
	  :mjd #(55555.650 55555.750 55555.850)
          :ra #(11.11d0 12.22 13.33)
          :dec #(-33.3333d0 -34.4444 -35.55)
          :ra-err #(0.1 0.2 0.3) :dec-err #(0.2 0.3 0.4) :time-err #(1 2 3)
	  :band #("V" "V" "V") :mag #(20.0 21.0 22.0)
          :observatory-code #("568" "569" "570"))


|#
	       
	 
	     
	    
    
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reverse parsing of a mpc line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-mpc-line-mjd-ra-dec-mag (mpcline)
  "Parse an MPC line, returning 
 (VALUES MJD RA DEC MAG FILTER OBSERVATORY-STRING [RA-ERR] [DEC-ERR]
where RA-ERR and DEC-ERR are optional.

The details of the object's ID in columns 1-16 are not parsed.
 (VALUES MJD RA DEC MAG FILTER OBSERVATORY-STRING [RA-ERR] [DEC-ERR]

This is clunky, but a legacy."

  (declare (type string mpcline))
  (when (< (length mpcline) 80)
    (error "mpc obs line <~S> too short" mpcline))
  (let* ((yr (ignore-errors (parse-integer mpcline :start 15 :end 19)))
	 (mon (ignore-errors (parse-integer mpcline :start 20 :end 22)))
	 (day (ignore-errors (jk-parse-float:parse-float mpcline :start 23 :end 32)))
	 (iday (when day (floor day)))
	 (fday (when day (- day iday)))
	 (mjd (when (and yr mon iday fday)
		(+ (astro-time:calendar-date-to-mjd yr mon iday 0 0 0d0)
		   fday)))
	 ;;
	 (ra (ignore-errors (ra-dec:hms-string->deg (subseq mpcline 32 44))))
	 (dec (ignore-errors (ra-dec:dms-string->deg (subseq mpcline 44 56))))
	 ;;
	 (mag (ignore-errors (jk-parse-float:parse-float (subseq mpcline 65 70))))
	 (filter (subseq mpcline 70 71))
	 (observatory (string-trim " " (subseq mpcline 77 80)))
	 (ra-error (if (> (length mpcline) 85)
		       (jk-parse-float:parse-float mpcline :start 81  :end 86
							:output-type 'single-float)))
	 (dec-error (if (> (length mpcline) 91)
			(jk-parse-float:parse-float mpcline :start 87  :end 92
							 :output-type 'single-float)))
	 )
    (values mjd ra dec mag filter observatory ra-error dec-error)))

