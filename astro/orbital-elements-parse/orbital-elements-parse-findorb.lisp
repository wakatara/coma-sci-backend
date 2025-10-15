
(in-package orbital-elements-parse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse FIND_ORB elements like

;; P/2010 V1q
;;    Perihelion 2016 Mar 16.429579 +/- 0.586 TT = 10:18:35 (JD 2457463.929579)
;; Epoch 2016 Jan  8.0 TT = JDT 2457395.5   Earth MOID: 0.5950   Ju: 0.4519
;; M 347.60486 +/- 0.11                Ma: 0.0323                Micheli
;; n   0.18113712 +/- 0.000436         Peri.  151.50995 +/- 0.59
;; a   3.09360173 +/- 0.00496          Node     4.08388 +/- 0.18
;; e   0.4897870 +/- 0.000951          Incl.    9.37601 +/- 0.010
;; P   5.44           M(N) 21.4    K  10.0     U  7.6
;; q 1.57839553 +/- 0.00311    Q 4.60880793 +/- 0.00886

;; but an alternative format is

;;  NAME
;;   Perihelion 2018 Oct 24.528468 +/- 75.2 TT = 12:40:59 (JD 2458416.028468)
;; Epoch 2017 Nov  3.0 TT = JDT 2458060.5   Sa: 0.8875           Micheli
;; q   7.67511416 +/- 0.0677           (J2000 ecliptic)
;; H   12.0  G 0.15                    Peri.  161.17458 +/- 4.6
;;                                    Node   245.78651 +/- 0.39
;; e   1.0455596 +/- 0.081             Incl.  158.29801 +/- 0.17
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun parse-findorb-comet-elem-string (text)
  "Parse comet elements from the text returned by Find_Orb program,
returning structure COMET-ELEM+ERR, which contains errors in orbit 
parameters."  
  (declare (type string text))
  (let ((line-list (with-input-from-string (s text)
		     (loop for line = (read-line s nil nil) until (not line)
			   collect line))))
    (let* (name peri-jd epoch-jd
	   orbinc orbinc-err
	   anode anode-err
	   perih perih-err
	   q q-err
	   e e-err
	   nform) ;; nform is 1,2 depending on which form we're using

      ;; check if the name is missing; if so, lengthen line-list
      ;; so fields are on expected lines
      (if (search "Perihelion" (nth 0 line-list)) ;; no name present
	  (progn (setf name "Unknown")
		 (setf line-list (cons t line-list))) ;; lengthen line-list
	  (setf name (string-trim " " (nth 0 line-list))))

      ;;
      (setf peri-jd
	    (or
	     (ignore-errors
	      (jk-parse-float:parse-float
	       (nth 1 line-list)
	       :start (+ 2 (search "JD" (nth 1 line-list)))
	       :junk-allowed t))
	     (error "Could not parse JD")))
      ;;
      (setf epoch-jd
	    (or
	     (ignore-errors
	      (jk-parse-float:parse-float
	       (nth 2 line-list)
	       :start (+ 3 (search "JD" (nth 2 line-list)))
	       :junk-allowed t))
	     (error "Could not parse JD epoch")))
      
      ;; function to find a number and its error in a string,
      ;; when the field is denoted by label
      (flet ((parse-number-and-err (nline label &key (error-on-fail t))
	       (multiple-value-bind (val err)
		(ignore-errors
		 (let* ((line (nth nline line-list))
			(nval (+ (length label)
				 (search label line)))
			(nerr (+ 3 (search "+/-" line :start2 nval)))
			(val (jk-parse-float:parse-float
			      line :start nval :junk-allowed t))
			(err (jk-parse-float:parse-float
			      line :start nerr :junk-allowed t)))
		   (values val err)))
		 (if (not (and val err))
		     (when error-on-fail
		       (error "Could not parse field ~A" label))
		     (values val err)))))

	(cond ((parse-number-and-err 8 "q" :error-on-fail nil)
	       (setf nform 1))
	      ((parse-number-and-err 3 "q" :error-on-fail nil)
	       (setf nform 2))
	      (t
	       (error "Can't find q on line 8 or 3 - unknown format")))
	
	(multiple-value-setq (orbinc orbinc-err)
	  (parse-number-and-err 6 "Incl."))
	(multiple-value-setq (anode anode-err)
	  (parse-number-and-err  5  "Node"))
	(multiple-value-setq (perih perih-err)
	  (parse-number-and-err 4  "Peri."))
	(multiple-value-setq (q q-err)
	  (parse-number-and-err  (if (= nform 1) 8 3)  "q"))
	(multiple-value-setq (e e-err)
	  (parse-number-and-err  6   "e")))

      (values
       (make-comet-elem+err
	:id name
	:epoch (astro-time:jd-to-mjd epoch-jd)
	:time-peri (astro-time:jd-to-mjd peri-jd) :time-peri-err 0d0
	:orbinc orbinc :orbinc-err orbinc-err 
	:anode anode :anode-err anode-err
	:perih perih :perih-err perih-err
	:q q :q-err q-err
	:e e :e-err e-err)))))

		 
#|
 or the asteroidal form

0   1P/2020 O1                     MAIN, 2011 Jun 06 to 2020 Aug 01, 0\".451 
1   Epoch 2020 Aug 02.000000 TT = JDT 2459063.500000 (J2000)         Smith
2   M  20.62854565   +/-   0.00001545
3   n   0.22894956   +/-   0.00000000     Peri.  104.86479   +/-   0.00004
4   a   2.6463229    +/-   0.0000000      Node   175.98944   +/-   0.00003
5   e   0.1197981    +/-   0.0000000      Incl.    5.22325   +/-   0.00000
6   q   2.3292986    +/-   0.0000001                                   MAJ
7   N    128    U  -0.7    P   4.30   S   7.03   G  0.15    H  18.25")



|#

(defun parse-findorb-asteroid-elem-string (text &key (convert-to-cometary t))
  "Parse comet elements from the text returned by Find_Orb program,
returning structure COMET-ELEM+ERR, which contains errors in orbit 
parameters."  
  (declare (type string text))
  (let ((line-list (with-input-from-string (s text)
		     (loop for line = (read-line s nil nil) until (not line)
			   collect line))))
    (let* (name  epoch-jd
	   orbinc orbinc-err
	   anode anode-err
	   perih perih-err
	   m m-err
	   q q-err
	   a a-err
	   e e-err
	   g h
	   ) ;; nform is 1,2 depending on which form we're using

      ;; check if the epoch is missing; if so, lengthen line-list
      ;; so fields are on expected lines
      (if (search "Epoch" (nth 0 line-list)) ;; no name present
	  (progn (setf name "Unknown")
		 (setf line-list (cons t line-list))) ;; lengthen line-list
	  ;; name is only the first part of line
	  (setf name (string-trim " "
				  (subseq (nth 0 line-list)
					  0
					  (min 20 (length (nth 0 line-list)))))))

      ;;
      (setf epoch-jd
	    (or
	     (ignore-errors
	      (jk-parse-float:parse-float
	       (nth 1 line-list)
	       :start (+ 3 (search "JDT" (nth 1 line-list)))
	       :junk-allowed t))
	     (error "Could not parse JD epoch")))
      
      ;; function to find a number and its error in a string,
      ;; when the field is denoted by label
      (flet ((parse-number-and-err (nline label &key (error-on-fail t))
	       (multiple-value-bind (val err)
		(ignore-errors
		 (let* ((line (nth nline line-list))
			(nval (+ (length label)
				 (search label line)))
			(nerr (+ 3 (search "+/-" line :start2 nval)))
			(val (jk-parse-float:parse-float
			      line :start nval :junk-allowed t))
			(err (jk-parse-float:parse-float
			      line :start nerr :junk-allowed t)))
		   (values val err)))
		 (if (not (and val err))
		     (when error-on-fail
		       (error "Could not parse field ~A" label))
		     (values val err))))
	     (parse-number (nline label)
	       	(ignore-errors
		 (let* ((line (nth nline line-list))
			(nval (+ (length label)
				 (search label line)))
			(val (jk-parse-float:parse-float
			      line :start nval :junk-allowed t)))
		   val))))
			

	
	(multiple-value-setq (orbinc orbinc-err)
	  (parse-number-and-err 5 "Incl."))
	(multiple-value-setq (anode anode-err)
	  (parse-number-and-err  4  "Node"))
	(multiple-value-setq (perih perih-err)
	  (parse-number-and-err 3  "Peri."))
	(multiple-value-setq (q q-err)
	  (parse-number-and-err  6  "q"))	
	(multiple-value-setq (a a-err)
	  (parse-number-and-err  4  "a"))
	(multiple-value-setq (e e-err)
	  (parse-number-and-err  5  "e"))
      	(multiple-value-setq (m m-err)
	  (parse-number-and-err  2  "M"))
	(setf g (parse-number 7 "G"))
	(setf h (parse-number 7 "H")))

      (let ((asteroid-elem
	      (make-asteroid-elem+err
	       :id name
	       :epoch  (astro-time:jd-to-mjd epoch-jd)
	       ;; we don't have errors yet
	       :orbinc orbinc  :orbinc-err orbinc-err
	       :anode anode    :anode-err anode-err
	       :perih perih    :perih-err perih-err
	       :perih perih    :perih-err perih-err
	       :a a            :a-err a-err
	       :e e            :e-err e-err
	       :m m            :m-err m-err
	       :data (make-asteroid-desc
		      :name name :source "Findorb-Text"
		      :h h :g g))))
	 (if convert-to-cometary
		(slalib-ephem:convert-asteroid-elem-to-comet-elem asteroid-elem)
		asteroid-elem)))))


(defun parse-findorb-elem-string (text &key (convert-to-cometary t))
  "Parse findorb elements, identifying cometary style or asteroidal style
(Perihelion is present, or not).  The asteroidal is by default convert to 
cometary."
  (or (ignore-errors (parse-findorb-comet-elem-string text))
      (ignore-errors (parse-findorb-asteroid-elem-string
		      text :convert-to-cometary convert-to-cometary))
      (error "Could not parse the following as FINDORB asteroidal or cometary elements: ~A"
	     text)))


	

