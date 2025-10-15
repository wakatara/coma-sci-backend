
#|

Download the most recent orbit file of just comets from
JPL to pre-warm the cache.

WARNING - it turns out the epoch of these orbits isn't updated, so they might
be very stale.    In fact, they might not even use the latest orbit fits.


File format is:

Num  Name                                     Epoch      q           e        i         w        Node          Tp       Ref
------------------------------------------- ------- ----------- ---------- --------- --------- --------- -------------- ------------
1         1         1         1
0         1         2         3         4         5         6         7         8         9         0         1         2         3
01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
 73P/Schwassmann-Wachmann 3-B                 53880  0.93911104 0.69328552  11.39695 198.79945  69.89248 20060607.92443 JPL K061/24

|#

(in-package jpl-horizons)


(defparameter *jpl-comet-page*
  "https://ssd.jpl.nasa.gov/dat/ELEMENTS.COMET")

(defun get-comet-orbit-file ()
  (let* ((bindata (drakma:http-request *jpl-comet-page*))
	 (text
	   (cond ((typep bindata 'string)
		  bindata)
		 ;; seems to be returned as butes
		 ((flexi-streams:octets-to-string
		   bindata :external-format :utf8)))))
    (when (search "Num  Name                                     Epoch"
			text)
      text)))


(defun %parse-orbit-file-name-string (str)
  (declare (type string str))
  (cond
    ;; numbered comet like  73P/Schwassmann-Wachmann 3-B  
    ((and (eql #\/ (aref str 4)) ;; a numbered comet
	  (digit-char-p (aref str 2)))
     (let* ((name (string-trim " " (subseq str 0 4)))
	    (ns (length str))
	    (nfrag (position #\- str :from-end t)) ;; search for fragment designator from end
	    (frag-name (when (and nfrag (not (= nfrag (1- ns))))
			 (string-trim " " (subseq str (1+ nfrag)))))
	    (good-frag (and frag-name
			    (<= (length frag-name) 2)
			    (every 'alpha-char-p frag-name))))
       (format nil "~A~A~A"
	       name
	       (if good-frag "-" "")
	       (if good-frag frag-name ""))))
    ;; other comets like   P/2019 LM4 (Palomar)
    (t
     (string-trim " " ;; get rid of the part like (Palomar)
		  (subseq str 0 (position #\( str))))))

;; convert string like "YYYYMMDD.xxx" to MJD
(defun %parse-orbit-file-date-to-mjd (date-str) 
  (let ((y (parse-integer date-str :start 0 :end 4))
	(m (parse-integer date-str :start 4 :end 6))
	(d (parse-integer date-str :start 6 :end 8))
	(df (jk-parse-float:parse-float date-str :start 8)))
    (+ (astro-time:calendar-date-to-mjd y m d 0 0 0)
       df)))


(defun parse-orbit-file-line (line)
  (let* ((name/s  (subseq line 0 43))
	 (epoch/s (subseq line 44 51))
	 (q/s     (subseq line 52 63))
	 (e/s     (subseq line 64 74))
	 (i/s     (subseq line 75 84))
	 (w/s     (subseq line 85 94))
	 (node/s  (subseq line 95 104))
	 (tp/s    (subseq line 105 119)) ;; in form 
	 (ref/s   (subseq line 120))
	 ;;
	 (name    (%parse-orbit-file-name-string name/s))
	 (epoch   (jk-parse-float:parse-float epoch/s)) ;; already mjd
	 (q       (jk-parse-float:parse-float q/s))
	 (e       (jk-parse-float:parse-float e/s))
	 (i       (jk-parse-float:parse-float i/s))
	 (w       (jk-parse-float:parse-float w/s))
	 (node    (jk-parse-float:parse-float node/s))
	 (tp/mjd  (%parse-orbit-file-date-to-mjd tp/s)))

    (declare (ignorable ref/s))
    ;;(print (list name/s epoch/s q/s e/s i/s w/s node/s tp/s ref/s))
    (orbital-elements:make-comet-elem
     :id name
     :epoch epoch
     :q q
     :orbinc i
     :e e
     :perih w
     :anode node
     :time-peri tp/mjd
     :data nil ;; less info than full JPL call
     )))
     
  
(defun get-elements-from-jpl-orbit-file ()
  (let ((orbit-file-text (get-comet-orbit-file)))
    (with-input-from-string (s orbit-file-text)
      ;; get rid of header line and underscore line
      (read-line s nil nil)
      (read-line s nil nil)
      (loop for line = (read-line s nil nil)
	    until (not line)
	    for orbit = (parse-orbit-file-line line)
	    when orbit
	      collect orbit))))

#+nil ;; this works, but the EPOCH in the orbits is stale. So don't use it.
(defun fill-cache-with-jpl-comets (&key (jpl-orbit-cache *default-jpl-orbit-cache*)
					(epoch-offset-from-present 20))
  "Fill the JPL-ORBIT-CACHE with the comets from the JPL orbit file, tagged with current
MJD.  This is for a special situation when the cache needs to be 'warmed' but with only
comets from the current epoch.

EPOCH-OFFSET-FROM-PRESENT is how man days the epoch of the elements may differ from the
current epoch.

Returns (VALUES N-ORBITS-TOTAL-OR-NIL  N-ORBITS-SAVED ERROR-OR-NIL)"
  (multiple-value-bind (orb-list err)
      (ignore-errors (get-elements-from-jpl-orbit-file))
    (let ((nsaved 0)) ;; the number that satisifed epoch constraint
      (when orb-list
	(loop with mjd = (astro-time:ut-to-mjd (get-universal-time))
	      for elements in orb-list
	      for epoch = (orbital-elements:comet-elem-epoch elements)
	      for object-name = (orbital-elements:comet-elem-id elements)
	      for results ;; the thing to save
		= (list elements "FROM JPL COMET FILE" nil)
	      when (< (abs (- epoch mjd)) epoch-offset-from-present)
		do (save-orbit-to-cache  object-name mjd results :jpl-orbit-cache jpl-orbit-cache)
		   (incf nsaved))
      (values (length orb-list) nsaved err)))))
					
  
	
