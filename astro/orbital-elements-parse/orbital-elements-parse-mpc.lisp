
(in-package orbital-elements-parse)


	      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MPC elements in one-line format
;;    these are different for asteroids and comets, but 
;;    (PARSE-MPC-ELEM-STRING ...) can distinguish between the two
;; 
;; see http://www.minorplanetcenter.net/iau/Ephemerides/Soft00.html
;;     https://www.minorplanetcenter.net/iau/MPC_Documentation.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun %decode-mpc-number (char)
  (declare (type character char))
  (let ((numchar-vec "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz") )
    (or (position char numchar-vec)
	(error "Invalid MPC number char ~A" char))))

(defun %decode-mpc-century (cyear)
  (declare (type character cyear))
   (cond ((char= cyear #\I) 1800)
	 ((char= cyear #\J) 1900)
	 ((char= cyear #\K) 2000)
	 ((char= cyear #\L) 2100)
	 (t (error "Invalid century code ~A" cyear))))

(defun %parse-mpc-packed-date-to-mjd (mpc-date)
  "parse an MPC date, with no fractional day component"
  (let* ((cyear (aref mpc-date 0))
	 (century (%decode-mpc-century cyear))
	 (year-in-century (parse-integer mpc-date :start 1 :end 3))
	 (year (+ century year-in-century))
	 (month (%decode-mpc-number (aref mpc-date 3)))
	 (day   (%decode-mpc-number (aref mpc-date 4)))
	 (mjd (astro-time:calendar-date-to-mjd year month day 0 0 0)))
    mjd))


;; see  http://www.minorplanetcenter.net/iau/info/PackedDes.html
;; section "Permanent Designations / Minor Planets"
#+nil  ;; moved to MPC-PACKED-DESIG package  -- delete this if no problems
(defun %parse-minor-planet-packed-desig (string) ;; 5 chars long
  (let ((chars-a-z   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
	(chars-0-z   "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
    (and (= (length string) 5)
	 (cond
	   ;; the simplest case of a number
	   ((every 'digit-char-p string)
	    (nth-value 0 (parse-integer string)))
	   ;; minor  planet < 620,000, as "A1234"
	   ((and (alpha-char-p (aref string 0)) 
		 (parse-integer string :start 1))
	    (let ((num-mod-10000 (parse-integer string  :start 1))
		  (num-div-10000 (+ 10
				    (position (aref string 0) chars-a-z))))
	      (format nil "~D"
		      (+ (* 10000 num-div-10000)
			 num-mod-10000))))
	   ;; minor planet above 620,000 as ~0000
	   ((and (eql #\~ (aref string 0)) ;; between 620,000 and 15396335
		 (every 'alphanumericp (subseq string 1)))
	    (loop for i from 4 downto 1
		  for pow62 = 1 then (* 62 pow62)
		  for c = (aref string i)
		  sum (* (position c chars-0-z) pow62) into ast-num
		  finally (return (format nil "~D" (+ 620000 ast-num)))))
	   ;;
	   (t
	    NIL)))))
					

;; see http://www.minorplanetcenter.net/iau/info/PackedDes.html
#+nil ;; moved to MPC-PACKED-DESIG package  -- delete this if no problems
(defun %parse-mpc-packed-asteroid-name (name)
  (let ((tname (string-trim " " name))
	tempvar)
    (cond ((every #'digit-char-p tname) ;; it's a numbered object
	   (string-trim " " name))
	  ((setf tempvar (%parse-minor-planet-packed-desig tname)) ;; KLUDGE
	   tempvar)
	  ((eql (aref name 2) #\S) ;; it's a survey object
	   (format nil "~A ~A-~A" (subseq name 3 7) (aref name 0) (aref name 1)))
	  ((not (find (aref name 0) "IJKL"))
	   (string-trim " " name)) ;; it's a Provisional (?) object
	  (T ;; not a survey object, so like "2007 TA418"
	   (let ((year (+ (%decode-mpc-century (aref name 0))
			  (parse-integer name :start 1 :end 3)))
		 (month-letter (aref name 3))
		 (2nd-letter (aref name 6))
		 (cycle-count (+ (* 10 (%decode-mpc-number (aref name 4)))
				 (parse-integer name :start 5 :end 6))))
	     (format nil "~4D ~A~A~A" year month-letter 2nd-letter
		     (if (zerop cycle-count) "" cycle-count)))))))

#+nil ;; moved to MPC-PACKED-DESIG package -- delete this if no problems
(defun %parse-mpc-packed-comet-name (name)  ;; this is the provisonal designation only
  (let* ((year (+ (%decode-mpc-century (aref name 0))
		  (parse-integer name :start 1 :end 3)))
	 (month-letter (aref name 3))
	 (cycle-count (+ (* 10 (%decode-mpc-number (aref name 4)))
			 (parse-integer name :start 5 :end 6)))
	 (frag-char (aref name 6))
	 (fragment (if (char= frag-char #\0) ""
		       (format nil "-~A" (char-upcase frag-char)))))
    (format nil "~4D ~A~A~A" year month-letter 
	    (if (zerop cycle-count) "" cycle-count)
	    fragment)))

 

#|

asteroids: http://www.minorplanetcenter.net/iau/info/MPOrbitFormat.html
comets: http://www.minorplanetcenter.net/iau/info/CometOrbitFormat.html

example MPC asteroid elements:
K14A52Z 18.2   0.15 K13B4 353.31110   17.62605  121.97038   18.72501  0.5615723  0.16096342   3.3469657  6 E2014-C04    40   1   24 days 0.50 M-v 38h MPC        0000         2014 AZ52           20140202

example MPC comet elements:
    CK13V010  2014 04 21.2451  1.660734  1.001314   48.0428   72.8097   65.3127  20131104  10.5  4.0      C/2013 V1 (Boattini)


|#


(defun %safe-parse-float (float-string where)
  (if (jk-parse-float:validate-float-string
       float-string :ignore-leading-whitespace t)
      (jk-parse-float:parse-float float-string)
      (error "Could not parse float string <~A> at <~A>" float-string where)))


(defun %parse-float-or-null (float-string)
  (if (jk-parse-float:validate-float-string float-string :ignore-leading-whitespace t)
      (jk-parse-float:parse-float float-string)
      nil))

;; given "(1) Ceres" or "1", return 1, else nil
(defun %get-num-from-asteroid-name (name)
  (declare (type string name))
  (let* ((components (string-utils:split-string name " "))
	 (c1 (first components))
	 (c2 (second components))
	 (number
	   (or
	    ;; just "123" or "123 Brunhild" (JPL doesn't do "(123) Brunhild")
	    (ignore-errors (parse-integer c1 :junk-allowed t)) 
	    (let ((nopen  (position #\( c1))
		  (nclose (position #\) c1)))
	      (when (and nopen nclose (> nclose nopen))
		(ignore-errors
		 (parse-integer name :start (+ 1 nopen) :end nclose)))))))
    ;; now we try to avoid stuff like 2016 AB12
    (when (and number
	       ;; if the number looks like a year, and the 2nd part
	       ;; has digits, it's not a comet number - YUCK
	       (not (and (<= 1850 number 2100)
			 (some 'digit-char-p c2))))
      number)))
	 

(defun parse-mpc-asteroid-elem-string (mpc-string &key (id nil)
				       (output-type :comet))
  "Parse MPC 'one line' elements as defined at
http://www.minorplanetcenter.net/iau/info/MPOrbitFormat.html. The ID,
if NIL, is taken from the string.  Converts output to COMET-ELEM if
OUTPUT-TYPE is :COMET.

Returns structure COMET-ELEM+HG or ASTEROID-ELEM+HG, which are 
subclasses of the parent classes, with H,G slots."
  (declare  (type string mpc-string))
  (when (< (length mpc-string) 103)
    (error "MPC-STRING asteroid orbit descriptor is shorter than 103 characters"))
  (let ((s-id (subseq mpc-string 0 7))
	(s-hmag (subseq mpc-string 8 13))
	(s-g (subseq mpc-string 14 19))
	(s-epoch (subseq mpc-string 20 25))
	(s-mean-anomaly (subseq mpc-string 26 35))
	(s-perih (subseq mpc-string 37 46))
	(s-anode (subseq mpc-string 48 57))
	(s-orbinc (subseq mpc-string 59 68))
	(s-ecc (subseq mpc-string 70 79))
	(s-semimaj (subseq mpc-string 92 103))
	(s-readable (when (> (length mpc-string) 194)
		      (string-trim " " (subseq mpc-string 166 194)))))
    ;;
    (let ((epoch (%parse-mpc-packed-date-to-mjd s-epoch))
	  ;; sometimes mean anomaly is unspecified, so we assume the orbit
	  ;; is poorly known but it doesn't matter at the level to which
	  ;; the orbit can be applied
	  (mean-anomaly (if (some 'digit-char-p s-mean-anomaly)
			    (%safe-parse-float s-mean-anomaly "mean-anomaly")
			    ;; problem - this is a bogus element
			    nil))
	  (h     (%parse-float-or-null s-hmag))
	  (g     (%parse-float-or-null s-g))
	  (perih (%safe-parse-float s-perih "perih"))
	  (anode (%safe-parse-float s-anode "anode"))
	  (orbinc (%safe-parse-float s-orbinc "orbinc"))
	  (ecc (%safe-parse-float s-ecc "ecc"))
	  (a   (%safe-parse-float s-semimaj "a (semimajor axis)"))
	  (parsed-id  (mpc-packed-desig:parse-packed-asteroid-name s-id)))
      (let* ((asteroid-desc
	       (make-asteroid-desc
		:name (or s-readable id parsed-id);; the ID we parsed
		:number (when s-readable (%get-num-from-asteroid-name s-readable))
		:source "MPC"
		:h h :g g))
	     ;; 
	     ;; if m is isn't defined, or e is >0 then this is really
	     ;; a comet pretending to be an asteroid, represeting a flaw
	     ;; in MPC (as discussed with Gareth)
	     (is-a-comet (or (not mean-anomaly)
			     (>= ecc 1)))
	     ;;
	     (asteroid-elem
	       (make-asteroid-elem
		:id (or id parsed-id) ;; maybe override parsed-id
		:epoch epoch
		:perih perih
		:anode anode
		:a a
		:e ecc
		:orbinc orbinc
		:m (or mean-anomaly most-positive-double-float)
		:data asteroid-desc)))
	;;

	(when  is-a-comet 
	  (error "This MPC asteroid string looks like a comet because MEAN-ANOMALY=~A and ECC=~A - it is probably a buggy MPC orbit." mean-anomaly ecc))
		   
	;;
	(cond ((eq output-type :comet)
	       (slalib-ephem:convert-asteroid-elem-to-comet-elem
		asteroid-elem))
	      ((eq output-type :asteroid)
	       asteroid-elem)
	      (t
	       (error "OUTPUT-TYPE=~A is not :asteroid or :comet"
		      output-type)))))))
 
	       
  
	   
;; convert year month day to MJD, allowing day to be optionally fractional
(defun %ymd-to-mjd (year month day)
  (multiple-value-bind (di df) (floor day)
    (+ (astro-time:calendar-date-to-mjd year month di 0 0 0)
       (* 1d0 df))))
	
(defun parse-mpc-comet-elem-string (mpc-string &key (id nil))
   "Parse MPC 'one line' cometary elements as defined at
http://www.minorplanetcenter.net/iau/info/CometOrbitFormat.html"
    (declare  (type string mpc-string))
  (when (< (length mpc-string) 89)
    (error "MPC-STRING asteroid orbit descriptor is shorter than 89 characters"))
  (let ((s-pcnum (string-left-trim "0" (string-trim " " (subseq mpc-string 0 4))))
	(s-orbtype (subseq mpc-string 4 5)) ;; orbital type - useful for final name - eg, C/2014 ..
	(s-provis-desig (string-trim " " (subseq mpc-string 5 12)))
	;; a POSSIBLE sub-desig like 'a' on numbered comets
	(s-sub-desig (string-trim " " (subseq mpc-string 11 12)))
	(s-year-peri (subseq mpc-string 14 18))
	(s-month-peri (subseq mpc-string 19 21))
	(s-day-peri (subseq mpc-string 22 29))
	(s-perih-dist (subseq mpc-string 30 39))
	(s-ecc (subseq mpc-string 41 49))
	(s-arg-peri (subseq mpc-string 51 59))
	(s-anode  (subseq mpc-string 61 69))
	(s-orbinc (subseq mpc-string 71 79))
	(s-year-perturb (subseq mpc-string 81 85))
	(s-month-perturb (subseq mpc-string 85 87))
	(s-day-perturb (subseq mpc-string 87 89))
	(s-full-desig  (if (> (length mpc-string) 104)
			   (string-trim '(#\space #\cr #\lf #\tab)
					(subseq mpc-string
						102
						(min 158 (length mpc-string)))))))
    

    ;;
    (let* ((epoch (cond  ((equalp s-year-perturb "    ") ;; no peturbation date, so use present epoch
			  (astro-time:ut-to-mjd (get-universal-time)))
			(t ;; else parse the epoch from which it was perturbed
			  (%ymd-to-mjd (parse-integer s-year-perturb) (parse-integer s-month-perturb)
			       (parse-integer s-day-perturb)))))
	   (time-peri (%ymd-to-mjd (parse-integer s-year-peri) (parse-integer s-month-peri)
				   (%safe-parse-float s-day-peri "time-peri"))) ;; time-peri has fractional day
	   (perih (%safe-parse-float s-arg-peri "perih"))
	   (anode (%safe-parse-float s-anode "anode"))
	   (orbinc (%safe-parse-float s-orbinc "orbinc"))
	   (ecc (%safe-parse-float s-ecc "ecc"))
	   (q   (%safe-parse-float s-perih-dist "q"))
	   (orbtype (if (not (equalp s-orbtype " "))
			(format nil "~A/" s-orbtype)
			""))
	   ;; there can be weird names like:
	   ;;    "332P      a  2016..." where the a is a sub-designator, so we parse provis
	   ;; desig only if no number. 
	   (provis-desig (if (and (not (plusp (length s-pcnum)) )
				  (plusp (length s-provis-desig)))
			     ;;(%parse-mpc-packed-comet-name s-provis-desig)
			     (mpc-packed-desig:parse-packed-comet-name s-provis-desig)
			     ""))
	   
	   (comet-desig  (if (> (length s-pcnum) 0)
			     (format nil "~A~A~A~A" s-pcnum s-orbtype
				     (if (plusp (length s-sub-desig)) "-" "")
				     s-sub-desig)
			     ""))
	   (parsed-id (cond ((plusp (length comet-desig))
			     (format nil "~A~A" comet-desig
				     (if (plusp (length provis-desig))
					 (format nil " (~A)" provis-desig) "")))
			    ((plusp (length provis-desig))
			     (format nil "~A~A" orbtype provis-desig))
			    (t "unknown"))))
      ;; (declare (ignore orbtype))
      ;;
      (make-comet-elem 
       :id (or id parsed-id) ;; maybe override parsed-id
       :epoch epoch :time-peri time-peri
       :orbinc orbinc :anode anode :perih perih :q q :e ecc
       :data (make-comet-desc
	      ;; favor the full desig given and fall back on parsed-id
	      :name (or s-full-desig parsed-id)
	      :source "MPC")))))
 

(defun parse-mpc-elem-string (mpc-string &key (id nil))
  "A general routine that determines if a string is MPC asteroid or comet, and parses
it to to an COMET-ELEM"
  (when (< (length mpc-string) 89)
    (error "MPC string is shorter than 89 characters"))
  (let ((y1 (ignore-errors (parse-integer mpc-string :start 14 :end 18)))
	(y2 (if (equalp (subseq mpc-string 81 85) "    ") ;; y2 can be blank (no perturb), or a year
		1999
		(ignore-errors (parse-integer mpc-string :start 81 :end 85)))))
    ;; if the years are in the right place for comet elements, do as comet
    (if (and y1 y2 (< 1700 y1 2300) (< 1700 y2 2300))
      (parse-mpc-comet-elem-string mpc-string :id id)
      (parse-mpc-asteroid-elem-string mpc-string :id id))))
				       
