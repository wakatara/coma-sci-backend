


(in-package astorb)

;; astorb files can be named astorb.dat.NNNN  or astorb.dat.NNNN.gz
;; where NNNN is the MJD


(defparameter *astorb-data-dir-pathname*
  (jk-datadir:get-datadir-for-system "astorb"))

(defparameter *astorb-data-dir* (namestring *astorb-data-dir-pathname*))


;; Initialized to NIL for Docker build - will be populated at runtime when data is available
(defparameter *astorb-file-list* nil)

(defparameter *astorb-file* nil)

;; Function to initialize astorb file list - called at runtime when data directory is available
(defun initialize-astorb-file-list ()
  "Initialize *astorb-file-list* and *astorb-file* from data directory"
  (setf *astorb-file-list*
        (ignore-errors
          (mapcar
           ;; add the basenames back because we remove them
           (lambda (basename)
             (format nil "~A/~A" *astorb-data-dir* basename))
           (remove-if ;; remove any .fasl files
            (lambda (file) (search ".fasl" file))
            (sort  (mapcar 'file-io:file-minus-dir ;; look at only the basename, to be safe
                           (mapcar 'namestring
                                   (append ;; look for both .dat.NNN  and .dat.NNN.gz where NNN=MJD
                                    (directory (format nil "~A/astorb.dat.*" *astorb-data-dir*))
                                    (directory (format nil "~A/astorb.dat.*.gz" *astorb-data-dir*)))))
                   'string>)))))
  (setf *astorb-file* (first *astorb-file-list*))
  *astorb-file*)

(defparameter  *astorb-info-output-stream*
  (if (find-symbol "ASTORB-QUIET" :CL-USER)
      NIL
      *standard-output*))

;; describe current mjd file and return MJD of it
(defun describe-astorb-file (filename &key quiet)
  (let* ((basename (file-io:file-minus-dir filename))
	 (i-mjd-start
	   (loop for c across basename
		 for i from 0
		 when (digit-char-p c)
		   do (return i)
		 finally (error "NO MJD found in astorb file ~A~%" filename)))
	 (mjd-elements (numio:parse-float basename
					  :start  i-mjd-start
					  :junk-allowed t))
	 (date (astro-time:mjd-to-ut-string mjd-elements)))
    (when (and *astorb-info-output-stream* (not quiet))
      (format *astorb-info-output-stream*
	      "ASTORB PACKAGE: Using ASTORB-FILE ~A ~% with MJD=~A and UT ~A~%"
	      basename
	      mjd-elements date))
    mjd-elements))

;; Commented out for Docker build - top-level call fails if data directory is empty
;; This gets called later at runtime when data is available
;; (describe-astorb-file *astorb-file*)






(defparameter *read-astorb-on-load* nil) ;; if NIL, read at first use - CHANGED for Docker build
(defvar *the-astorb* nil)



;; define a struct, with a write for it
(defmacro defstruct-with-writer (name &rest slots)
  `(progn
     (defstruct ,name ,@slots)
     (defun ,(intern (string-upcase (format nil "~A-writer" name))) (obj)
       (list
	',(intern (string-upcase (format nil "MAKE-~A" name)))
	 ,@(loop with outlist = nil
		 for slot in slots
		 for slot-name = (first slot)
		 for slot-keyword = (intern (string-upcase slot-name) :keyword)
		 for slot-accessor = (intern
				      (string-upcase (format nil "~A-~A" name slot-name)))
		 do (push slot-keyword outlist)
		    (push `(,slot-accessor obj) outlist)
		 finally (return (reverse outlist)))))))

(defstruct-with-writer astorb
  ;; the epoch of the elements which we put into the file as a rough book-keeping
  ;; notice - the actual mjd of osculation is in each orbit
  (epoch-of-elements 0d0 :type double-float)
  (n 0 :type (unsigned-byte 32)) ;; number of elements
  ;;
  ;;
  ;; asteroid number, or 0 for no number
  (astnum (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  ;; name (ascii), or NIL
  (name (make-array 0) :type (simple-array t (*)))
  ;; simplified name with whitespace removed, lowercase (ascii), or NIL
  (sname (make-array 0) :type (simple-array t (*)))
  ;; H mag
  (hmag (make-array 0 :element-type 'single-float) :type (simple-array single-float (*)))
  ;; slop param G
  (g (make-array 0 :element-type 'single-float) :type (simple-array single-float (*)))
  ;; IRAS size
  (iras-km
   (make-array 0 :element-type 'single-float) :type (simple-array single-float (*)))
  ;; IRAS classification (single letter as string, possibly with question mark)
  (iras-class
   (make-array 0 :element-type 't) :type (simple-array t (*)))
  ;; codes as described in doc
  (code1 (make-array 0 :element-type '(unsigned-byte 8))
	 :type (simple-array(unsigned-byte 8) (*)))
  (code2 (make-array 0 :element-type '(unsigned-byte 8)) 
	 :type (simple-array(unsigned-byte 8) (*)))
  (code3 (make-array 0 :element-type '(unsigned-byte 8)) 
	 :type (simple-array(unsigned-byte 8) (*)))
  (code4 (make-array 0 :element-type '(unsigned-byte 8)) 
	 :type (simple-array(unsigned-byte 8) (*)))
  (code5 (make-array 0 :element-type '(unsigned-byte 8)) 
	 :type (simple-array(unsigned-byte 8) (*)))
  (code6 (make-array 0 :element-type '(unsigned-byte 8)) 
	 :type (simple-array(unsigned-byte 8) (*)))
  ;; orbital arc in days
  (orbarc
   (make-array 0 :element-type 'single-float) :type (simple-array single-float (*)))
  ;; number of observations for arc
  (nobs (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  ;; epoch of osculation YYYYMMDD - add 0.5
  (epoch-osc (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  ;; mean anomaly, deg
  (mean-anomaly  (make-array 0 :element-type 'double-float) 
		 :type (simple-array double-float (*)))
  ;; argument of perihelion
  (arg-peri  (make-array 0 :element-type 'double-float) 
	     :type (simple-array double-float (*)))
  ;; long of ascending node, deg
  (anode  (make-array 0 :element-type 'double-float) 
	  :type (simple-array double-float (*)))
  ;; orbital inclination
  (orbinc  (make-array 0 :element-type 'double-float) 
	   :type (simple-array double-float (*)))
  ;; eccentricity
  (ecc  (make-array 0 :element-type 'double-float) 
	   :type (simple-array double-float (*)))
  ;; semimajor axis
  (a  (make-array 0 :element-type 'double-float) 
	 :type (simple-array double-float (*)))
  ;; date of orbit computation, YYYYMMDD
  (orbit-date  (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  ;;
  ;; there are higher fields, but we ignore them for now 
  )
  

;; make string lowercase and remove non-alpha chars
(defun %scrub-string (string)
  (declare (type  (simple-array character (*)) string)
	   (optimize speed))
  (let* ((nspc (loop
		  with n of-type (unsigned-byte 28) = 0
		  for c of-type base-char across string
		  when (not (and (typep c 'base-char) (alphanumericp c)))
		  do  (incf n) 
		  finally (return n)))
	 (outstring (make-array (- (length string) nspc) :element-type 'base-char)))
    (loop 
       with i of-type (unsigned-byte 28) = 0
       for c across string
       when  (and (typep c 'base-char) (alphanumericp c))
       do 
	 (setf (aref outstring i) (char-downcase (the base-char c)))
	 (incf i))
    outstring))


(defun build-astorb (n)
  (flet ((afixnum () (make-array n :element-type 'fixnum :initial-element 0))
	 (ageneric () (make-array n :element-type 't :initial-element nil))
	 (afloat () (make-array n :element-type 'single-float :initial-element 0.0))
	 (adbl () (make-array n :element-type 'double-float :initial-element 0d0))
	 (a8bit () (make-array n :element-type '(unsigned-byte 8) :initial-element 0)))
    (make-astorb 
     :astnum (afixnum)
     :n n
     :name (ageneric) :sname (ageneric)
     :hmag (afloat)
     :g (afloat)
     :iras-km (afloat)
     :iras-class (ageneric)
     :code1 (a8bit)  :code2 (a8bit)  :code3 (a8bit)
     :code4 (a8bit)  :code5 (a8bit)  :code6 (a8bit)
     :orbarc (afloat)
     :nobs (afixnum)
     :epoch-osc (afixnum)
     :mean-anomaly (adbl)
     :arg-peri (adbl)
     :anode (adbl)
     :orbinc (adbl)
     :ecc (adbl)
     :a (adbl)
     :orbit-date (afixnum))))
	    
  
;; see if Juno is where it is supposed to be
(defun %test-astorb-on-juno  (astorb &key 
			      (mjd (astro-time:calendar-date-to-mjd 2017 02 03 0 0 0))
			      (jpl-ra 269.67695d0)
			      (jpl-dec -12.53366d0))
  (let ((elem-juno (get-comet-elem-for-nth-asteroid 2 :astorb astorb)))
    (multiple-value-bind (ra dec)
	(slalib-ephem:compute-radecr-from-comet-elem-for-observatory  
	 elem-juno
	 mjd
	 "uh88"
	 :perturb t)
      (let ((dra  (* 3600 (abs (- jpl-ra ra))))
	    (ddec (* 3600 (abs (- jpl-dec dec)))))
	(when (or (> dra 0.5)
		  (> ddec 0.5))
	  (error 
	   "ASTORB predicted Juno position does not match JPL value. 
      ra-JPL=~,6F     ra-pred=~,6F    err=~,5F
      dec-JPL=~,6F   dec-pred=~,6F    err=~,5F
   Is MJD of coords valid?"
	   jpl-ra ra   dra
	   jpl-dec dec ddec))))))



;; count lines even if file is a gzip file
(defun astorb-count-lines (filename)
  (if (not (string-utils:string-ends-with filename ".gz"))
	(file-io:file-count-lines filename)
      (gzip-stream:with-open-gzip-file (s filename)
	(loop with nret = 0
	      for b = (read-byte s nil nil) ;; buffered reading is no faster
	      until (not b)
	      when (eql b #.(char-code #\lf))
		do (incf nret)
	      finally (return nret)))))
    



;; open a file either in gzip mode or other mode, in byte mode
(defmacro astorb-with-open-file ((stream-var filename) &body body)
  `(let ((%file ,filename))
     (flet ((%astorb-with-open-file-body (,stream-var)
	      ,@body))
       (if (string-utils:string-ends-with %file ".gz")
	   (gzip-stream:with-open-gzip-file (%stream %file)
	     (%astorb-with-open-file-body %stream))
	   (with-open-file (%stream %file :element-type '(unsigned-byte 8))
	     (%astorb-with-open-file-body %stream))))))

;; read a line from byte stream
(defun astorb-read-line (stream &optional buffer)
  (declare (type stream stream)
	   (type (or null string) buffer))
  (loop with buffer = (or buffer (make-string 2048))
	with nmax = (length buffer)
	for b = (read-byte stream nil nil)
	for i of-type fixnum from 0
	when (not b)
	  do (return nil)
	do (let ((c (code-char b)))
	     (cond ((char= c #\lf)
		    (return buffer)) ;; leave junk on end - doesn't matter
		   ((char= c #\cr)
		    nil)
		   (t
		    (when (= i nmax) (error "Line too long in astorb stream."))
		    (setf (aref buffer i) c))))))
	
	
	
	 

;; astorb is a real PITA to read because not all fields are present
(defun read-astorb (&key (infile *astorb-file*))
  ;; the astorb file name has to have the MJD of the elements in it
  (let* ((mjd-elements (describe-astorb-file infile :quiet t)) ;; returns MJD
	 (n (astorb-count-lines infile))
	 (astorb (build-astorb n))
	 (tmpline (make-array 20 :element-type 'character))
	 (buffer (make-string 512))
	 (where nil) ;; what field we're parsing for debug
	 (ncurline 0)) ;; number of current line
    (setf (astorb-epoch-of-elements astorb) mjd-elements) ;; just a bookkeeping measure - not used
    (astorb-with-open-file (s infile)
      (labels ((subline (line n1 n2) ;; subseq using scratch array
		 (declare (type (simple-array character (*)) line)
			  (type (unsigned-byte 16) n1 n2)
			  (optimize speed))
		 (fill tmpline #\space)
		 (loop 
		    for i of-type fixnum from n1 below n2 
		    for j of-type fixnum from 0
		    do (setf (aref tmpline j) (aref line i)))
		 tmpline)
	       ;; for grab-string use real subseq to avoid clobbering
	       (grab-string (line n1 n2)
		 (declare (type (simple-array character (*)) line)
			  (type (unsigned-byte 16) n1 n2)
			  (optimize speed))			
		 (string-trim #(#\tab #\space) (subseq line n1 n2)))
	       (grab-int (line n1 n2 &optional (default 0))
		 (declare (type (simple-array character (*)) line)
			  (type (unsigned-byte 16) n1 n2)
			  (optimize speed))
		 (or (ignore-errors (parse-integer (subline line n1 n2)))
		     default))
	       (grab-dbl (line n1 n2 &optional (default 0d0))
		 (declare (type (simple-array character (*)) line)
			  (type (unsigned-byte 16) n1 n2)
			  (optimize speed))
		 (or (ignore-errors (numio:parse-float (subline line n1 n2)))
		     default))
	       (grab-float (line n1 n2 &optional (default 0e0))
		 (declare (type (simple-array character (*)) line)
			  (type (unsigned-byte 16) n1 n2)
			  (optimize speed))
		 (or (ignore-errors 
		       (float (numio:parse-float (subline line n1 n2)) 1.0))
		     default)))

	(multiple-value-bind (val err)
	    (ignore-errors
	     (loop
	       for i below n
	       for line of-type simple-string = (or (astorb-read-line s buffer)
						    (error "premature end of infile"))
	       do
		  (setf ncurline (1+ i))
		  (setf where "I")
		  (setf (aref (astorb-astnum astorb) i) (grab-int line 0 6))
		  (setf where "NAME")
		  (setf (aref (astorb-name astorb) i) (grab-string line 7 25))
		  (setf where "SNAME")
		  (setf (aref (astorb-sname astorb) i) 
			(if (aref (astorb-name astorb) i)
			    (%scrub-string  (aref (astorb-name astorb) i))))
		  (setf where "HMAG")
		  (setf (aref (astorb-hmag astorb) i) (grab-float line 42 47))
		  (setf where "G")
		  (setf (aref (astorb-g astorb) i) (grab-float line 49 53))
		  (setf where "IRAS-KM")
		  (setf (aref (astorb-iras-km astorb) i) (grab-float line 55 64))
		  (setf where "IRAS-CLASS")
		  (setf (aref (astorb-iras-class astorb) i) (grab-float line 65 58))
		  (setf where "CODE1")
		  (setf (aref (astorb-code1 astorb) i) (grab-int line 73 75))
		  (setf where "CODE2")
		  (setf (aref (astorb-code2 astorb) i) (grab-int line 77 79))
		  (setf where "CODE3")
		  (setf (aref (astorb-code3 astorb) i) (grab-int line 81 83))
		  (setf where "CODE4")
		  (setf (aref (astorb-code4 astorb) i) (grab-int line 87 87))
		  (setf where "CODE5")
		  (setf (aref (astorb-code5 astorb) i) (grab-int line 89 91))
		  (setf where "CODE6")
		  (setf (aref (astorb-code6 astorb) i) (grab-int line 93 95))
		  (setf where "ORBARC")
		  (setf (aref (astorb-orbarc astorb) i) (grab-float line 95 100))
		  (setf where "NOBS")
		  (setf (aref (astorb-nobs astorb) i) (grab-int line 101 105))
		  (setf where "EPOCH-OSC")
		  (setf (aref (astorb-epoch-osc astorb) i) (grab-int line 106 114))
		  (setf where "MEAN-ANOM")
		  (setf (aref (astorb-mean-anomaly astorb) i) (grab-dbl line 115 125))
		  (setf where "ARG-PERI")
		  (setf (aref (astorb-arg-peri astorb) i) (grab-dbl line 126 136))
		  (setf where "ANOIDE")
		  (setf (aref (astorb-anode astorb) i) (grab-dbl line 137 147))
		  (setf where "ORBINC")
		  (setf (aref (astorb-orbinc astorb) i) (grab-dbl line 148 157))
		  (setf where "ECC")
		  (setf (aref (astorb-ecc astorb) i) (grab-dbl line 158 168))
		  (setf where "A")
		  (setf (aref (astorb-a astorb) i) (grab-dbl line 170 181))
		  (setf where "ORBIT-DATE")
		  (setf (aref (astorb-orbit-date astorb) i) (grab-int line 182 190))
	       finally (return t))) ;; for ignore-errors
	  (when  (not val)
	    (error "ERROR ~A at line ~A: line is ~A" err ncurline buffer)))
	#+sbcl (sb-ext:gc :full t)
	astorb))))



;; convert YYYYMMMDD to MJD
(defun %mjd-from-astorb-date (yyyymmdd)
  (multiple-value-bind (year mmdd)
      (floor yyyymmdd 10000)
    (multiple-value-bind (month day)
	(floor mmdd 100)
      (+ (astro-time:calendar-date-to-mjd year month day 0 0 0)))))


(defun get-comet-elem-for-nth-asteroid (n &key (astorb (get-the-astorb)))
  "For Nth asteroid (0 indexed) in astorb database, return a SLALIB-EPHEM:COMET-ELEM,
after converting asteroidal orbit to cometary."
  (let* ((epoch-osc (aref (astorb-epoch-osc astorb) n))
	 (epoch-osc-mjd (%mjd-from-astorb-date epoch-osc))
	 (mean-anomaly (aref (astorb-mean-anomaly astorb) n))
	 (arg-peri (aref (astorb-arg-peri astorb) n))
	 (anode (aref (astorb-anode astorb) n))
	 (orbinc (aref (astorb-orbinc astorb) n))
	 (ecc (aref (astorb-ecc astorb) n))
	 (a (aref (astorb-a astorb) n))
	 (name (aref (astorb-name astorb) n))
	 (ast-num (aref (astorb-astnum astorb) n))
	 (fullname (if (zerop ast-num)
		       name (format nil "(~A) ~A"
				    ast-num name)))
	 (dm 0d0) ;; ignored
	 (velem (make-array 13 :element-type 'double-float)))
    ;; first convert asteroid to universal elements
    (slalib:sla-el2ue epoch-osc-mjd
		      2 ;; jform=2 => asteroid orbit
		      epoch-osc-mjd
		      (* (/ pi 180) orbinc)
		      (* (/ pi 180) anode)
		      (* (/ pi 180) arg-peri)   
		      a ecc  
		      (* (/ pi 180) mean-anomaly) 
		      dm velem)
    ;; then convert universal elem to comet elem
    (multiple-value-bind (epoch orbinc anode perih aorq e aorl dm)
	(slalib:sla-ue2el velem 3)
      (declare (ignore aorl dm))
      (slalib-ephem:make-comet-elem 
	 :id fullname 
	 :epoch epoch-osc-mjd 
	 :time-peri epoch
	 :orbinc (* orbinc (/ 180 pi))
	 :anode  (* anode (/ 180 pi))
	 :perih  (* perih (/ 180 pi))
	 :q      aorq 
	 :e      e
	 :data (slalib-ephem::make-asteroid-desc
		:name fullname
		:number (if (plusp ast-num) ast-num)
		:source "Astorb"
		:h  (aref (astorb-hmag astorb) n)
		:g  (aref (astorb-g astorb) n)
		:radius  (if (aref (astorb-iras-km astorb) n)
			     (* 0.5 (aref (astorb-iras-km astorb) n))
			     nil)
		:period nil
		:albedo nil   )))))

;; is s1 in s2? 
(defun %stringsearch (s1 s2)
  (declare (type simple-string s1 s2)
	   (optimize speed))
  (block done
    (loop
       with c0 = (aref s1 0)
       for i of-type (signed-byte 28) below (1+ (- (length s2) (length s1)))
       when (char= c0 (aref s2 i))
       ;; test remainder of string after zeroth char
       do
	 (loop 
	    for j from 1 below (length s1)
	    when (not (char= (aref s1 j) (aref s2 (+ i j))))
	    do (return) ;; quit this loop
	    finally (return-from done t)) ;; match
       finally (return-from done nil))))
	    

(defun search-for-asteroids-by-name (name &key (astorb (get-the-astorb)) 
				     (match-type :substring))
  "Return a list of asteroid indices that match name, and a list of the names

MATCH-TYPE can be 
    :SUBSTRING - the name is contained inside the true name, but case insensitive.
    :EXACT     - the name is an exact but case insensitive match

In both instances, both names are lowercased and have non-alphanumeric chars removed"
  (declare (type string name)
	   (type (member :substring :exact) match-type)
	   (optimize speed))

  (when (= (length name) 0) (error "Zero length name"))
  
  (loop 
     ;; the-name is lowercase, with whitespace removed
     with the-name of-type string = (%scrub-string name)
     for  i of-type (unsigned-byte 28) from 0       
     for ast-name of-type string across (astorb-sname astorb)
     when (and
	   ast-name
	   (cond ((eq match-type :exact)
		  (string= ast-name the-name)) ;; both are lowercase
		 ((eq match-type :substring)
		  (%stringsearch  the-name ast-name))))
     collect i into indices and collect (aref (astorb-name astorb) i) into names
     finally (return (values indices names))))

(defun find-numbered-asteroid (n  &key (astorb (get-the-astorb)))
  "Return the astorb index for numbered asteroid N"
  (let ((k (1- n)))
    (when
	(and (<= 0 k (1- (length (astorb-astnum astorb))))
	     (= (aref (astorb-astnum astorb) k) n))
      k)))

 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fasl saving facility for much faster loading of astorb database
(defparameter *fasl-base*  (format nil "~A-~A"
				   *astorb-file*
				   (uiop/os:implementation-identifier))) ;; eg "sbcl-1.3.21-macosx-x64"

(defun make-astorb-fasl (&key (astorb *the-astorb*)
			   (fasl-base *fasl-base*)
			   (delete-lisp-file t))
  (readable-arrays:with-readable-arrays
     (let ((lisp-file (format nil "~A.lisp" fasl-base))
	   (fasl-file (format nil "~A.fasl" fasl-base)))
       (unwind-protect
	    (progn
	      (with-open-file (s lisp-file :direction :output :if-exists :supersede)
		(write '(in-package astorb) :stream s)
		(terpri s)
		(write `(setf *the-astorb*
			      ,(astorb-writer astorb)) :stream s))
	      (compile-file lisp-file  :output-file fasl-file)))
       (when delete-lisp-file
	 (delete-file lisp-file)))))

(defun read-astorb-fasl  (&key (fasl-base *fasl-base*))
  (let ((fasl-file (format nil "~A.fasl" fasl-base)))
    (when (probe-file fasl-file)
      (load fasl-file)
      t)))
	   
    
				   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun maybe-read-the-astorb ()
    (when (not (or *the-astorb*
		   ;; if we can, set *the-astorb* using the fasl file
		   (read-astorb-fasl)))
      (format t "########### Reading astorb - it may take 5 minutes or more to read ########~%")
      (format t "###########   a GZIP file and build fast-loading fasl file         ########~%")
      (format t "########### It will then be fast until astorb file is updated      ########~%")
    (force-output)
    (setf *the-astorb* (read-astorb))
    ;; verify that Juno has the correct location
    (%test-astorb-on-juno *the-astorb*)
    (format t "########### Done reading astorb; creating fast-loading fasl    ##############~%")
    (force-output)
    (make-astorb-fasl)
    (format t "########### Done making fast-loading fasl ~A.fasl   #####~%" *fasl-base*)))
    


;; Commented out for Docker build - data will be loaded at runtime instead
;; (eval-when (:load-toplevel)
;;   (when *read-astorb-on-load*
;;     (maybe-read-the-astorb)))

(defun get-the-astorb ()
  "Get the astorb, reading it if necessary."
  (maybe-read-the-astorb)
  *the-astorb*)
