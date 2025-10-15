

(in-package terapix)

   
(defun %xml-find-value-for-name (tag name treenode)
  "Find the 'value' of a tag named 'tag' with a name 'name'"
  (let ((tags (xmls:xmlrep-find-child-tags tag treenode)))
    (loop 
       for tag in tags
       when
	 (equalp (xmls:xmlrep-attrib-value  "name" tag  nil) name)
       do
	 (return  (xmls:xmlrep-attrib-value "value" tag nil)))))
	 


(defun %xml-parse-value (val type array-size)
  (flet ((parse-one-value (type str)
	   (cond ((equalp type "int") (parse-integer str))
		 ((equalp type "float") (float 
					 (numio:parse-float (coerce str 'simple-string))
					 1.0))
		 ((equalp type "double") (* 1d0 (numio:parse-float str)))
		 ;((equalp type "char") str)
		 ((equalp type "boolean") (equalp str "Y")))))
    (cond ((equalp type "char") ;; ignire array size
	   val)
	  ((not array-size)
	   (parse-one-value type val))
	  (array-size
	   (let* ((n (if (not (equalp array-size "*")) (parse-integer array-size)))
		  (list (string-utils:split-string val '(#\space #\tab #\cr #\lf)))
		  (vlist
		  (loop for thing in list 
		     collect (parse-one-value type thing))))
	     (when (and n (not (= (length vlist) n)))
	       (error "Wrong length array parsing xml in  %xml-parse-value"))
	     (map 'vector 'identity vlist))))))
 	     
		 
;; parse the output xml file for scamp
(defun %parse-scamp-xml (xml)
  (declare (optimize debug))
  (let (table nfields fields field-names field-types
	      field-arraysizes
	      tabledata tr tds)
    
    (setf table (first
		  (xmls:xmlrep-find-child-tags 
		   "TABLE"
		   (first
		    (xmls:xmlrep-find-child-tags 
		     "RESOURCE"
		     (first (xmls:xmlrep-find-child-tags "RESOURCE" xml)))))))
    
    (when (not (equalp (xmls:xmlrep-attrib-value "name" table nil) "Fields"))
      (error "Fields table not found"))
    ;;
    (setf nfields
	  (ignore-errors
	    (parse-integer (%xml-find-value-for-name "PARAM" "NFields" table))))
    ;;
    (setf fields (xmls:xmlrep-find-child-tags "FIELD" table))

    (setf field-names 
	  (mapcar (lambda (thing) 
		    (xmls:xmlrep-attrib-value "name" thing nil))
		  fields))
    
    (setf field-types
	  (mapcar (lambda (thing) 
		    (xmls:xmlrep-attrib-value "datatype" thing nil))
		  fields))
    
    (setf field-arraysizes
	  (mapcar (lambda (thing) 
		    (xmls:xmlrep-attrib-value "arraysize" thing nil))
		  fields))
    
    (setf tabledata
	  (xmls:xmlrep-find-child-tag "TABLEDATA"
				      (xmls:xmlrep-find-child-tag "DATA" table nil)
				      nil))

    (setf tr
	  (xmls:xmlrep-find-child-tag "TR" tabledata nil))
    
    #+nil
    (setf tds 
	   (loop for thing in tr
		when (and (listp thing)
			  (or ;; FIXME to do p
			   ;; xml without http
			   (equalp (first thing) "TD")
			   ;; xml with http so ("TD . http)
			   (equalp (car (first thing)) "TD")))
		  collect (third thing)))

    ;; something changed after big quicklisp updated - now TD are nodes
    #+nil
    (setf tds 
	  (loop for thing in (xmls:node-children tr)
		when (and (listp thing)
			  (or 
			   ;; xml without http
			   (equalp (first thing) "TD")
			   ;; xml with http so ("TD . http)
			   (and (listp (first thing))
				(equalp (car (first thing)) "TD"))))
		  collect (third thing)))

    ;; new version after 2024-06-12 where TDs are nodes
    (setf tds 
	  (loop for thing in (xmls:node-children tr)
		when (xmls:node-p thing)
		  collect (first (xmls:node-children thing))))


     (loop 
	for name in field-names
	for type in field-types
	for array-size in field-arraysizes
	for val in tds
	for parsed-val = (%xml-parse-value val type array-size)
	collect (list name parsed-val))

    ))
    
(defun read-scamp-xml-file (file)
  (%parse-scamp-xml 
       (with-open-file (s file)
	 (xmls:parse s))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun read-scamp-head-file (file)
  "Read the output SCAMP .head FILE and return a list of sublists, one for 
each extension (terminated by END).  Each sublist consists of (KEY VALUE STRING-VALUE)  
pairs, and VALUE is parsed into the appropriate type.  STRING-VALUE is 
the original string representation of VALUE."
  (labels ((read-one-section (s) ;; read up to END
	     (reverse
	      (loop
		 with outlist = nil
		 for line = (read-line s nil nil)
		 do
		   (cond 
		     ;; parse fields, ignoring COMMENT and history
		     ((not line) (return outlist))
		     ((equalp (string-trim '(#\space #\tab) line) "END")
		      (return outlist))
		     ((< (length line) 8) ;; weird line
		      nil)
		     ((char= (aref line 8) #\=) 
		      (let* ((key (string-trim '(#\space #\tab)  (subseq line 0 8)))
			     (tail  (string-trim '(#\space #\tab)(subseq line 9)))
			     (value (parse-value tail)))
			(push (list key value tail) outlist)))))))
	   ;;
	   (parse-value (string)
	     (cond ((and (equalp (aref string 0) #\')
			 (position #\' string :start 1))
		    (string-trim  '(#\space #\tab)
				  (subseq string 1 (position #\' string :start 1))))
		   ((equalp (aref string 0) #\F)
		    NIL)
		   ((equalp (aref string 0) #\T)
		    T)
		   (t
		    (let* ((string ;; kill comment
			    (subseq string 0 (position #\/ string)))
			   (int-val (ignore-errors
				      (parse-integer string :junk-allowed nil)))
			   (float-val (if 
				       (not int-val)
				       (ignore-errors
					 (numio:parse-float string :junk-allowed nil)))))
		      (or int-val float-val :ERROR))))))

  (with-open-file (s file)
    (loop for list = (read-one-section s)
	 until (not list)
	 collect list))))
    

(defun write-scamp-head-file (list file &key (if-exists :supersede))
  "Given a list of lists of the form produced by READ-SCAMP-HEAD-FILE, write
it to FILE"
  (with-open-file (sout file :direction :output :if-exists if-exists)
    (loop 
       for extlist in list 
       do
       (loop 
	  for header-list in extlist
	  for key = (first header-list)
	  for val = (second header-list)
	  for val-string = (cond ((eq val t)   "T")
				 ((eq val nil) "F")
				 ((floatp val)
				  (if (or (< (abs val) 1d-5) (> (abs val) 1d5))
				      (substitute #\e #\d (format nil "~e" val))
				      (format nil "~F" val)))
				 ((integerp val)
				  (format nil "~D" val))
				 (t (format nil "\'~A\'" val)))
	  do 
	    (format sout "~8A= ~A~%" key val-string))
	 (write-line "END     " sout))))
	    
	    
       
       


;; get headers of form PVk_NNN out of a scamp head file sublist,
;; returning a PV array or NIL if none
(defun %get-pv-vec (k hdrs)
  (let ((pvhead (format nil "PV~D_" k)))
    ;; test if ANY PVk_XX headers present
    (when (some (lambda (hdr) (and (>= (length (first hdr)) 5)
				   (equalp pvhead (subseq (first hdr) 0 4))))
		hdrs)
      (loop
	with pvec = (make-array  100 :element-type 'double-float :initial-element 0d0)
	with biggest-i = nil
	for i below (length pvec)
	for header = (format nil "PV~D_~D" k i)
	for val = (second (assoc header hdrs :test 'equal))
	when (and val (realp val))
	  do 
	     (setf (aref pvec i) (float val 1d0))
	     (setf biggest-i i)
	  finally
	     (return
	       (when biggest-i
		 (subseq pvec 0 (1+ biggest-i))))))))
	

(defun parse-scamp-wcs-from-headfile (file)
  "Return a list of WCS structures for each section of head file.  Returns
a WCS:WCS-RADEC-TAN or WCS:WCS-RADEC-TAN-TPV depending on non-linear terms."
  (loop with hlist = (read-scamp-head-file file)
	for i from 1
	for h in hlist
	for pv1vec = (%get-pv-vec 1 h)
	for pv2vec = (%get-pv-vec 2 h)
	collect
	(flet ((gethead (key list)
		 (or (second (assoc key list :test 'equal))
		     (error "Key ~A not found in header number ~D" key i))))
	  (if (or pv1vec pv2vec)
	      (wcs:make-wcs-radec-tan-tpv
	       :crval1 (gethead "CRVAL1" h) 
	       :crval2 (gethead "CRVAL2" h) 
	       :crpix1 (gethead "CRPIX1" h) 
	       :crpix2 (gethead "CRPIX2" h) 
	       :cd1_1 (gethead "CD1_1" h) 
	       :cd1_2 (gethead "CD1_2" h) 
	       :cd2_1 (gethead "CD2_1" h) 
	       :cd2_2 (gethead "CD2_2" h) 
	       :equinox (gethead "EQUINOX" h)
	       :pv1vec (or pv1vec (make-array 0 :element-type 'double-float))
	       :pv2vec (or pv1vec (make-array 0 :element-type 'double-float)))
	      ;;
	      (wcs:make-wcs-radec-tan 
	       :crval1 (gethead "CRVAL1" h) 
	       :crval2 (gethead "CRVAL2" h) 
	       :crpix1 (gethead "CRPIX1" h) 
	       :crpix2 (gethead "CRPIX2" h) 
	       :cd1_1 (gethead "CD1_1" h) 
	       :cd1_2 (gethead "CD1_2" h) 
	       :cd2_1 (gethead "CD2_1" h) 
	       :cd2_2 (gethead "CD2_2" h) 
	       :equinox (gethead "EQUINOX" h))))))
  
