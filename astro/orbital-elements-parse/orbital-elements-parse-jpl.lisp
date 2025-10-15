

(in-package orbital-elements-parse)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JPL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
(defun make-comet-elem-from-jpl-elements (&key epoch tp inc om w qr/km ec id)
  "Make comet-elem from JPL Horizon EPOCH TP INC OM W QR EC.
Note that EPOCH and TIME-PERI are in in JD, just like JPL.
This is just a convenience wrapper to transform JPL notation into
SLALIB notation."
  (when (not (every (lambda (x) (typep x 'double-float))
		    (list epoch tp inc om w qr/km ec)))
    (error 
     "All of EPOCH TP INC OM W QR EC must be set and must be a double float"))
  (make-comet-elem
   :id id
   :epoch (- epoch  2400000.5d0)
   :time-peri (- tp  2400000.5d0)
   :orbinc inc
   :anode om
   :perih w
   :q (/ qr/km 1.495978707d8)
   :e ec))
    



#|
parse-jpl-ephem-string is a helper function to turn HORIZONS style orbital element strings into
an EPHEM, like

"FK5/J2000.0 helio. ecliptic osc. elements (au, days, deg., period=Julian yrs): 
 
  EPOCH=  2456388.5 != 2013-Apr-06.0000000 (CT)   RMSW= n.a.                   
   EC= 1.000004876624184   QR= .01247623700403888  TP= 2456625.2675712714      
   OM= 295.6990330916964   W= 345.533055118787     IN= 62.0853836396048        
   A= -2558.375739699086   MA= 0.                  ADIST= 9.999999E99          
   PER= 9.999999E99        N= 7.617E-6             ANGMOM= .002717307          
   DAN= .01268             DDN= .78706             L= 288.8119264              
   B= -12.7532999          MOID= .0231545          TP= 2013-Nov-28.7675712714  "

This can be messed up, because EC digits can extend all the way to QR

However, the correct data in the JPL response is in the $$SOE ... $$EOE block, and the
spacing is different.

$$SOE
2459138.500000000 = A.D. 2020-Oct-16 00:00:00.0000 TDB 
 EC= 5.824782538431438E-03 QR= 8.932691788771608E+09 IN= 2.517053990762085E+01
 OM= 1.275929630944562E+02 W = 2.101175972664384E+02 Tp=  2504483.832642491907
 N = 2.450751768627654E-08 MA= 2.639835467948763E+02 TA= 2.633202734371143E+02
 A = 8.985027620764360E+09 AD= 9.037363452757113E+09 PR= 1.468937020094813E+10
$$EOE

|#
	     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; get the text in $$SOE<text>$$EOE
(defun %extract-jpl-soe-eoe-block (jpl-text)
  (let ((nstart (search "$$SOE" jpl-text))
	(nend   (search "$$EOE" jpl-text)))
    (when (and nstart nend (< nstart nend))
      (subseq jpl-text (+ 6 nstart) nend))))




;; this finds a tag like "EC= xx.xx" or "EC = xx.xx" but be careful
;; that the char before it may be a space or (overrunning) number,
;; not a letter (so we don't confuse "A=" with the end of "MA=")
;; returns NIL, or the index after "TAG=" or "TAG ="
(defun %find-jpl-tag-in-text (tag text &key (start 0) (end nil))
  (declare (type string tag text))
  (let ((end (if end
		 (min end (length text))
		 (length text)))
	(ntag (length tag)))
    ;;
    (flet ((confirm-tag (j)
	     (and (string-equal tag text :start2 j :end2 (+ j ntag))
		  (or (and
		       (< (+ j ntag 1) end)
		       (string-equal "=" text :start2 (+ j ntag) :end2 (+ j ntag 1))
		       (+ j ntag 2))
		      (and
		       (< (+ j ntag 2) end)
		       (string-equal " =" text :start2 (+ j ntag) :end2 (+ j ntag 2))
		       (+ j ntag 3))))))
      ;;
      (loop with c0 = (aref tag 0)
	    for last-c = #\null then c
	    for i from start below end
	    for c = (aref text i)
	    do (and (char= c0 c)
		    (or (zerop i) ;; start of text
			(not (alpha-char-p last-c))) ;; not inside another tag
		    (let ((tag-loc (confirm-tag i)))
		      (when tag-loc (return tag-loc))))
	    finally (return nil)))))
	  
		  
	  
			 
  

;; extract an assoc list like
;; (("EPOCH-JD" . 2459138.5d0) ("EC" . 0.005824782538431438d0)
;;  ("QR" . 8.932691788771608d9) ("IN" . 25.17053990762085d0)
;;  ("OM" . 127.59296309445621d0) ("W" . 210.1175972664384d0)
;;  ("Tp" . 2504483.8326424924d0) ("N" . 2.4507517686276538d-8)
;;  ("MA" . 263.9835467948763d0) ("TA" . 263.3202734371143d0)
;;  ("A" . 8.98502762076436d9) ("AD" . 9.037363452757113d9)
;;  ("PR" . 1.4689370200948132d10))
(defun %extract-jpl-orbit-params (jpl-orbit-text
				  &key
				    (start 0) (end nil)
				    ;; one form has EPOCH=
				    (try-named-epoch t)
				    ;; the other form just gives EPOCH as a number
				    (try-un-named-epoch t))
  (flet ((get-tag (tag)
	   (let ((ntag (%find-jpl-tag-in-text tag jpl-orbit-text :start start :end end)))
	     ;;(format t "Parsing ~A at n=~A~%" ttag (+ 3 ntag))
	     (ignore-errors
	      (numio:parse-float jpl-orbit-text :start ntag :junk-allowed t)))))
    (let ((epoch-jd
	    (or
	     (and try-named-epoch
		  (get-tag "EPOCH"))
	     (and try-un-named-epoch
		  (ignore-errors
		   (numio:parse-float jpl-orbit-text
				      :start start
				      :junk-allowed t)))))) ;; skips over whitespace
      (cons (cons "EPOCH-JD" epoch-jd)
	    (loop for tag in '("EC" "QR" "IN" "OM" "W" "Tp" "N" "MA" "TA" "A"
			       "AD" "PR")
		  collect (cons tag (get-tag tag)))))))

(defun %extract-jpl-asteroid-params (jpl-text)
  (let* ((n-start (search "Asteroid physical parameter" jpl-text))
	 (n-end   (when n-start (search "*****" jpl-text :start2 (+ n-start 1))))
	 (asteroid-text
	   (when (and n-start n-end)
	     (subseq jpl-text n-start n-end))))
    (when asteroid-text
      (flet ((parse-tag (tag)
	       (let ((nt (search (concatenate 'string tag "=") asteroid-text)))
		 (when nt (ignore-errors (numio:parse-float asteroid-text
							    :start (+ nt (length tag) 1)
							    :junk-allowed t))))))
	(loop for tag in '("G" "H" "ALBEDO" "RAD" "ROTPER")
	      collect (cons tag (parse-tag tag)))))))


(defun %extract-jpl-comet-params (jpl-text)
  (let* ((n-start (search "Comet physical" jpl-text))
	 (n-end   (when n-start (search "*****" jpl-text :start2 (+ n-start 1))))
	 (comet-text
	   (when (and n-start n-end)
	     (subseq jpl-text n-start n-end))))
    (when comet-text
      (flet ((parse-tag (tag)
	       (let ((nt (search (concatenate 'string tag "=") comet-text)))
		 (when nt (ignore-errors (numio:parse-float comet-text
							    :start (+ nt (length tag) 1)
							    :junk-allowed t))))))
	(loop for tag in '("DT" "A1" "A2" "A3"
			   "GM" "RAD" "M1" "M2" "k1" "k2" "PHCOF")
	      collect (cons tag (parse-tag tag)))))))

(defun %extract-jpl-target-body-name (jpl-text)
  (let* ((n (search "Target body name:" jpl-text))
	 (nstart (if n (+ n 17)))
	 (nend   (if n (+ n 46)))
	 (id (if (and n (< nend (length jpl-text)))
		 (string-trim " " (subseq jpl-text nstart nend)))))
    id))
	


(defun parse-jpl-elem-string (jpl-text &key (id nil)
					 (object-desc-name nil)
					 (use-soe-eoe-block :if-exists))
  "Parse a JPL element string into an COMET-ELEM structure.
OBJECT-DESC-NAME is the name placed into ASTEROID-DESC or COMET-DESC.

 USE-SOE-EOE-BLOCK means to find the $$SOE .. $$EOE block
   values are :IF-EXISTS, NIL, or :MANDATORY.  
 Generally, this block should be used when downlaoding data from JPL, but
 might not be present if we are given a text block with JPL orbit style data."

  (declare (type string jpl-text)
	   (type (member :if-exists :mandatory nil) use-soe-eoe-block))

  (flet ((assoc-tag (tag alist &optional (fail-on-error t))
	   (let ((value (cdr (assoc tag alist :test 'equalp))))
	     (when (and (not value) fail-on-error)
	       (error "JPL orbit value ~A missing." tag))
	     value)))
	   
    
    (let* ((soe-eoe-block
	     (if use-soe-eoe-block (%extract-jpl-soe-eoe-block jpl-text)))
	   (id (or id (%extract-jpl-target-body-name jpl-text)))
	   (object-desc-name (or object-desc-name id))
	   (asteroid-params (%extract-jpl-asteroid-params jpl-text))
	   (comet-params (%extract-jpl-comet-params jpl-text))
	   (start-of-main-block
	     (search "EPOCH=" jpl-text))
   (orbit-params
	     (cond ((and (member use-soe-eoe-block '(:if-exists :mandatory))
			 soe-eoe-block)
		    (%extract-jpl-orbit-params soe-eoe-block
					       :try-named-epoch nil))
		   ((and (eq use-soe-eoe-block :mandatory) (not soe-eoe-block))
		    (error "$$SOE .. $$EOE block not found"))
		   (t
		    (%extract-jpl-orbit-params jpl-text :start start-of-main-block))))
	   ;;tags must exist (tp inc om w qr ec))
	   (epoch-jd (assoc-tag "EPOCH-JD" orbit-params))
	   (inc (assoc-tag "IN" orbit-params))
	   (om (assoc-tag "OM" orbit-params))
	   (w (assoc-tag "W" orbit-params))
	   (qr (assoc-tag "QR" orbit-params))
	   (ec (assoc-tag "EC" orbit-params))
	   (tp (assoc-tag "Tp" orbit-params))
	   (asteroid-desc
	     (when asteroid-params
	       (make-asteroid-desc
		:name object-desc-name
		:number  (%get-num-from-asteroid-name object-desc-name)	
		:source "JPL Horizons"
		:h (assoc-tag "H" asteroid-params nil)
		:g (assoc-tag "G" asteroid-params nil)
		:radius (assoc-tag "RAD" asteroid-params  nil)
		:period (assoc-tag "ROTPER" asteroid-params  nil)
		:albedo (assoc-tag "ALBEDO" asteroid-params  nil))))
	   (nongravs
	     (when comet-params
	       (ignore-errors ;; if something is missing, don't fail
		(make-nongravs :dt (assoc-tag "DT" comet-params  nil)
			       :a1 (assoc-tag "A1" comet-params  nil)
			       :a2 (assoc-tag "A2" comet-params  nil)
			       :a3 (assoc-tag "A3" comet-params  nil)))))
	   (comet-desc
	     (when comet-params
	       (make-comet-desc 
		   :name object-desc-name
		   :source "JPL Horizons"
		   ;; more data may go here
		   )))
	   (comet-elements 
	     (make-comet-elem-from-jpl-elements  
	      :epoch epoch-jd :tp tp :inc inc :om om :w w :qr/km qr :ec ec :id id)))

      (setf (comet-elem-data comet-elements) (or asteroid-desc comet-desc))
      (setf (comet-elem-nongravs comet-elements) nongravs)
      comet-elements)))
      
     
	  
     
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the version before 2025-09-17 that failed
;; when the fields flowed into each other and eg
;; QR= had a digit before it instead of a space.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(defun parse-jpl-elem-string (jpl-string &key (id "UN-NAMED")
					 (object-desc-name nil))
  "Parse a JPL element string into an COMET-ELEM structure, to make it easier to ingest JPL data manually.
OBJECT-DESC-NAME is the name placed into ASTEROID-DESC or COMET-DESC."
  (declare (type string jpl-string))
  (let (epoch tp inc om w qr ec
	h g albedo rad rotper
	dt a1 a2 a3 ;; non-gravs
	(is-asteroid (search "Asteroid physical parameters" jpl-string))
	(is-comet (search "Comet physical" jpl-string)))
    (with-input-from-string (s jpl-string)
      (flet ((set-one-field (tag s)
	       (let ((value (ignore-errors  (numio:read-object-from-stream s :double-float))))
		 (cond ((equalp tag "EPOCH") (setf epoch value))
		       ((equalp tag "TP") (setf tp value))
		       ((equalp tag "IN") (setf inc value))
		       ((equalp tag "OM") (setf om value))
		       ((equalp tag "W") (setf w value))
		       ((equalp tag "QR") (setf qr value))
		       ((equalp tag "EC") (setf ec value))
		       ;; extrap asteroid params
		       ((equalp tag "H") (setf h value))
		       ((equalp tag "G") (setf g value))
		       ((equalp tag "ALBEDO") (setf albedo value))
		       ((equalp tag "RAD") (setf rad value))
		       ((equalp tag "ROTPER") (setf rotper value))
		       ;; non-gravs
		       ((equalp tag "DT") (setf dt value))
		       ((equalp tag "A1") (setf a1 value))
		       ((equalp tag "A2") (setf a2 value))
		       ((equalp tag "A3") (setf a3 value))
		       ))))
		       
	(loop with found-epoch = nil
	      with i-lines = 0
	      for line =   (read-line s nil nil)

	      ;; until line is blank
	      until (or (> i-lines 17) ;; only 17 lines after EPOCH used
			(not line))
	      do
		 (when found-epoch  (incf i-lines))
		 ;;
		 (setf line (string-trim '(#\space #\tab #\cr #\lf) line))
		 ;; replace all #\= chars with #\space to simplify
		 ;; parsing, because sometimes it is "QR ="  and sometimes "QR="
		 (nsubstitute #\space #\= line)
		 ;; a horrid kludge that fixes the situation like
		 ;; EC= 1.558348124682506E-5QR= 60.1
		 ;;		 (setf line (%unborkify-jpl-line line))

		 ;;
		 (with-input-from-string (sline line)
		   ;;(format t "Line is: <~A>~%" line)
		   (let ((tag1 (ignore-errors (numio:read-object-from-stream sline :string))))
		     (cond
		       ;; epoch is its own special line, and we parse only first field
		       ;; eg   EPOCH=  2458849.5 ! 2020-Jan-01.00 (TDB)         Residual RMS= .24563
		       ((equalp tag1 "EPOCH")
			(setf found-epoch t)
			(setf epoch (ignore-errors (numio:read-object-from-stream sline :double-float))))
		       ;;
		       ;; planets don't have "EPOCH" but have '2460196.328263889 = A.D. 2023-Sep-08 19:52:42.0000 TDB'
		       ((and tag1
			     (not found-epoch)
			     (jk-parse-float:validate-float-string tag1))
			(let ((%epoch (jk-parse-float:parse-float tag1)))
			  (when (and (< 2000000 %epoch 3000000) ;; looks like a JD
				     (search " A.D." line)
				     (search "TDB" line))
			    (setf found-epoch t)
			    (setf epoch %epoch))))
		       ;;
		       ((equalp tag1 "B") NIL) ;; don't do anything for funny last line, because it repeats TP=<string>
		       ;; we now have a line possibly containing 3 values we want
		       ((member tag1 '("EC" "OM" "A" "PER" "DAN" "GM" "H" "ALBEDO"
				       "AMRAT" "DT" "A1" "A2" "A3")
				:test 'equalp)
			(set-one-field tag1 sline) ;; set the next two fields
			(set-one-field (ignore-errors (numio:read-object-from-stream sline :string)) sline)
			(set-one-field (ignore-errors (numio:read-object-from-stream sline :string)) sline))))))
	(print (list :epoch epoch :tp tp :inc inc :om om :w w :qr qr :ec ec))
	(when (not (and epoch tp inc om w qr ec))
	  (error "JPL string did not have all required fields"))
	(let ((celem
		;; note that JPL elements don't reliably have the object name
		;; so we u
		(make-comet-elem-from-jpl-elements
		 :epoch epoch :tp tp :inc inc
		 :om om :w w
		 ;; qr is given by JPL in km
		 :qr/km qr
		 :ec ec :id id)))
	  (when is-asteroid
	    (setf (comet-elem-data celem)
		  (make-asteroid-desc
		   :name object-desc-name
		   :number  (%get-num-from-asteroid-name object-desc-name)	
		   :source "JPL Horizons"
		   :h h
		   :g g
		   :radius rad
		   :period rotper
		   :albedo albedo)))
	  ;;
	  (when is-comet
	    (when (and dt a1 a2 a3)
	      (setf (comet-elem-nongravs celem)
		    (make-nongravs :dt dt :a1 a1 :a2 a2 :a3 a3)))	    
	    (setf (comet-elem-data celem)
		  (make-comet-desc 
		   :name object-desc-name
		   :source "JPL Horizons"
		   ;; more data may go here
		   )))
	  
	  celem)))))
		   

	
