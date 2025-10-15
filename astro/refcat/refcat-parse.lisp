(in-package refcat)

(eval-when (:load-toplevel :compile-toplevel)
  (defparameter *refcat-fields*
    '(RA Dec plx dplx pmra dpmra pmdec
      dpmdec Gaia dGaia BP dBP RP dRP Teff AGaia dupvar Ag rp1 r1 r10 g
      dg gchi gcontrib r dr rchi rcontrib i di ichi icontrib z dz zchi
      zcontrib nstat J dJ H dH K dK
      ;; ID is a synthetic field
      id))
  
  (defparameter *refcat-units* '(10ndeg 10ndeg 10uas 10uas 10uas/yr
    10uas/yr 10uas/yr 10uas/yr mmag mmag mmag mmag mmag mmag K mmag integer
    mmag 0.1as 0.1as 0.1as mmag mmag hundredth bitmap mmag mmag hundredth bitmap
    mmag mmag hundredth bitmap mmag mmag hundredth bitmap integer mmag mmag mmag mmag
     mmag mmag
    ;; T is unit of our synthetic ID field - really a token because it won't be called
    t))) 


 
      
(eval `(defstruct (refcat-entry (:type vector) (:conc-name refcat-)) ,@*refcat-fields*))

(declaim (inline %parse-int parse-refcat-field))

;; faster version of parse-integer that stops on a comma
(defun %parse-int (s istart) 
  (declare (type (simple-array character)  s)
	   (optimize (speed 3) (safety 1)))
  ;;(format t "Parsing at ~A <~A> ~%" istart (subseq s istart (min (length s) (+ istart 20)))) (force-output)
  (loop with ntot = (length s)
	with sign = (if (char= (aref s istart) #\-) -1 +1)
	with istart-true = (if (= sign -1) (1+ istart) istart)
	with sum of-type (unsigned-byte 50) = 0
	for j of-type (signed-byte 20) from istart-true below ntot
	for c = (aref s j)
	until (char= c #\,)
	for n = (- (char-code c) #.(char-code #\0))
	do (setf sum (+ (* 10 sum) n)) ;; sbcl smart enough to code 10*n=(n<<3)+(n<<1)
	finally (return (* sign sum))))

(defun parse-refcat-field (line istart unit)
  (declare (type simple-string line)
	   (type  (unsigned-byte 10) istart)
	   (type symbol unit)
	   (optimize speed))
  (let ((n (%parse-int line istart))) 
    (declare (type (signed-byte 50) n))
    (cond ((eq unit '10ndeg)   (* n 1d-8))  ;; convert to deg
	  ((eq unit '10uas)    (* n 1e-2))  ;; 10micorarcsec  to mas
	  ((eq unit '10uas/yr) (* n 1e-2))  ;; 10micorarcsec/yr  to mas/yr
	  ((eq unit 'mmag)     (* n 1e-3))  ;; convert to mag
	  ((eq unit 'K)        (* n 1.0))   ;; convert to float
	  ((eq unit 'integer)   n)          ;; keep as integer
	  ((eq unit '0.1as)     (* n 0.1))  ;; convert to arcsec
	  ((eq unit 'hundredth) (* n 0.01)) ;; convert to ones
	  ((eq unit 'bitmap)    n)          ;; keep as integer
	  ((eq unit T)          nil)        ;; a special field to be parsed later (won't be called)
	  (t (error "unknown unit ~A" unit)))))
		 
;; the ID is rrrrrrrrrSdddddddd
;; where r are the digits of RA*1e6, d are the digits of abs(Dec)*1e6, and S
;; is a +/- depending on sign of dec
(defun %make-refcat-id-from-ra-dec (ra dec)
  (let ((nra  (round (* ra  1000000)))  ;; position to millionth of degree
	(ndec (round (abs (* dec 1000000)))))
    (format nil "~9,'0D~A~8,'0D" nra (if (minusp dec) "-" "+") ndec)))
	

(defun parse-refcat-line (line &optional (refcat-entry nil))
  (declare (type simple-string line)
	   (type (or null simple-array) refcat-entry)
	   (optimize speed))
  (loop with refcat-entry = (or refcat-entry (make-refcat-entry))
	with ra = nil and dec = nil
	for i below 44
	for unit in *refcat-units*
	for istart = 0 then (1+ (position #\, line :start istart))
	for value = (parse-refcat-field line istart unit)
	do (setf (aref refcat-entry i) value)
	   (when (= i 0) (setf ra value))
	   (when (= i 1) (setf dec value))
	finally
	   (setf (refcat-id refcat-entry)
		 (%make-refcat-id-from-ra-dec ra dec))
	   (return refcat-entry)))
  

;; read a potentially compressed refcat file as a string - gz is twice as fast (300 files/sec)
;; but compresses a bit worse
(defun read-refcat-file-as-string/filesys (file)
  (let ((gzfile (concatenate 'string (namestring file) ".gz"))
	(bz2file (concatenate 'string (namestring file) ".bz2")))
    (cond ((probe-file bz2file)
	   (let ((byte-vec
		   (with-open-file (s bz2file :element-type '(unsigned-byte 8))
		     (chipz:decompress nil 'chipz:bzip2 s))))
	     (map 'simple-string #'code-char byte-vec)))
	  ((probe-file gzfile) 
	   (let ((byte-vec
		   (with-open-file (s gzfile :element-type '(unsigned-byte 8))
		     (chipz:decompress nil 'chipz:gzip s))))
	     (map 'simple-string #'code-char byte-vec)))
	  ((probe-file file)
	   (with-open-file (s file :element-type 'character)
	     (let ((rstring (make-string (file-length s))))
	       (read-sequence  rstring s)
	       rstring)))
	  (nil
	   (error "None of ~A, ~A, or ~A found." file gzfile bz2file)))))


;; we save the last compression that was used, and try that one first
;; so we get future compression right on first try next time.
(let ((favored-compression :uncompressed) ;; or :gz or :bz2
      (c-lock (bordeaux-threads:make-lock "favored-compression-lock")))
  ;;
  (defun read-refcat-file-as-string/http (file)
	(let ((gzfile (concatenate 'string file ".gz"))
	      (bz2file (concatenate 'string file ".bz2")))
	  (labels ((url-encode (path) path
		     (with-output-to-string (s)
		       (loop for c across path
			     do (cond ((member c '(#\+));; (or (alphanumericp c) (member c '(#\_ #\- #\: #\/ #\.)))
				       (format s "%~2,'0x" (char-code c)))
				      (t
				       (write-char c s))))))

		   ;;
		   (retrieve-web-bytes (file)
		     (let* ((fcomp
			      (bordeaux-threads:with-lock-held (c-lock) favored-compression))
			    ;; list of possible compression methods with the last used one first
			    (compressions (cons fcomp
						(remove fcomp (list :uncompressed :gz :bz2)))))
		       ;;
		       (loop for compression in compressions
			     for uri-path = (url-encode
					     (cond ((eq compression :uncompressed) file)
						   ((eq compression :gz) gzfile)
						   ((eq compression :bz2) bz2file)))
			     do (multiple-value-bind (bytes code)
				    (ignore-errors
				     (drakma:http-request uri-path :force-binary t
								   :preserve-uri t)) ;; this prevents further escaping
				  (when (and bytes (eql code 200)) ;; HTTP-OK
				    (bordeaux-threads:with-lock-held (c-lock)
				      (setf favored-compression compression))
				    (return (values bytes compression))))))))
	    
	    (multiple-value-bind (web-bytes compression)
		(retrieve-web-bytes file)
	      ;;
	      (cond ((not web-bytes)
		     (error "None of ~A, ~A, or ~A found." file gzfile bz2file))
		    ((eq compression :bz2)
		     (let ((byte-vec
			     (chipz:decompress nil 'chipz:bzip2 web-bytes)))
		       (map 'simple-string #'code-char byte-vec)))
		    ((eq compression :gz)
		     (let ((byte-vec
			     (chipz:decompress nil 'chipz:gzip web-bytes)))
		       (map 'simple-string #'code-char byte-vec)))
		    (t  ;; uncompressed
		     (map 'simple-string #'code-char web-bytes)))))))) 
		     

(defun %refcat-is-http (rpath)
  (declare (type string rpath))
  (or (eql 0 (search "http://" rpath :test 'equalp))
      (eql 0 (search "https://" rpath :test 'equalp))))

;; read file from either HTTP://, HTTPS:// or local file sys
(defun read-refcat-file-as-string (file)
  (cond ((%refcat-is-http file)
	 (read-refcat-file-as-string/http file))
	(t
	 (read-refcat-file-as-string/filesys file))))




(defun read-refcat-file (file &key ra dec radius/deg)
  "Read a refcat file, and if RA,DEC,RADIUS/DEG are given, then return only those points 
that are within RADIUS/DEG of RA,DEC."
  (let ((refcat-file-string (read-refcat-file-as-string file)))
    (with-input-from-string (s refcat-file-string)
      (loop for line = (read-line s nil nil)
	    until (not line)
	    for re = (parse-refcat-line line)
	    when (or (not (and ra dec radius/deg))
		     (<= (astro-coords:sky-angle ra dec (refcat-ra re) (refcat-dec re)
						 :units :degrees)
			 radius/deg))	
	      collect re))))
