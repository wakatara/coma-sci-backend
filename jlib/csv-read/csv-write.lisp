
(in-package csv-read)

(defun %make-float-output-function (ndecimal)
  (if (not ndecimal)
      #'identity
      (let ((format-string
	      (format nil "~~,~D,,,,,'eG" ndecimal)))
	(lambda (x)
	  (string-trim
	   " " ;; not clear why space-padded
	   (format nil format-string x))))))

(defun write-csv-hash-to-file (hash outfile
			       &key
				 (column-name-vector nil)
				 (comment-char #\#)
				 (csv-comment nil)
				 (if-exists :supersede)
				 (ndecimal 6)
				 (line-selector nil))
  "Write a csv file in a hash with HASH-KEY=COLNAME, HASH-VAL=DATA.

If COLUMN-NAME-VECTOR is given, use these column names 
and their ordering.

CSV-COMMENT and COMMENT-CHAR are used to generate a comment at 
the top of the file.

NDECIMAL controls to how many decimal points floating point columns
are written. If NIL default to FARE-CSV full precision floats.

LINE-SELECTOR is NIL, or a function (LINE-SELECTOR NLINE) that returns
T for those line indices (beginning at 0) that are to be written."

  ;; build the column name vector if not given
  (when (not column-name-vector)
    (setf column-name-vector (make-array (hash-table-count hash)))
    (loop for colname being the hash-key of hash
	  for i from 0
	  do (setf (aref column-name-vector i) colname)))
  (when (zerop (length column-name-vector))
    (error "No columns to write"))
  ;; validate that column names are OK
  (loop for colname across column-name-vector
	when (not (gethash colname hash))
	  do (error "Column ~A not found in csv hash table" colname))
  ;; columns same length?
  (loop with nelem = (length (gethash (aref column-name-vector 0) hash))
	for colname across column-name-vector
	when (not (= (length (gethash colname hash))
		     nelem))
	  do (error "Column lengths are not all the same."))
	
  
  (with-open-file (sout outfile :direction :output :if-exists if-exists)
    ;; write the comment
    (when csv-comment
      (with-input-from-string (scom csv-comment)
	(loop for cline = (read-line scom nil nil)
	      until (not cline)
	      do (write-char comment-char sout)
		 (write-line cline sout))))
    (loop for colname across column-name-vector
	  for i from 0
	  do (format sout "~A~A" (if (= i 0) "" ",") colname))
    (terpri sout)

    (loop with nelem = (length (gethash (aref column-name-vector 0) hash))
	  with float-generator = (%make-float-output-function ndecimal)
	  with colvecs
	    = (loop with v = (make-array
			      (length column-name-vector))
		    for j below (length v)
		    for colname across column-name-vector
		    do (setf (aref v j)
			     (gethash colname hash))
		    finally (return v))
	  for i below nelem
	  for write-line = (if line-selector
			       (funcall line-selector i)
			       t)
	  when write-line
	  do (let ((csv-fields
		     (loop for colvec across colvecs
			   for val = (aref colvec i)
			   collect (cond ((floatp val)
					  (funcall float-generator val))
					 (t val)))))
	       (fare-csv:write-csv-line csv-fields sout)))))
		    
    
    
			       
