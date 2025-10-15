

(in-package csv-read)

	


(defun parse-item (string &key (parse-time nil) (date-convention :MM-DD-YYYY))
  (let ((retval nil))
    (cond ((ignore-errors (setf retval (parse-integer string)))
	   (values retval :integer))
	  ((ignore-errors (setf retval (jk-parse-float:parse-float string)))
	   (values retval :float))
	  ((equalp string  "")
	   (values nil :empty))
	  ;; parse date and time only if PARSE-TIME is set
	  ((and parse-time
		(setf retval
		      (jd-time-utils:parse-date-time-string
		       string
		       :date-convention date-convention)))
	   (values retval :time))
		      
	  (t
	   (values string :string)))))


;; read a csv line, respecting csv quoting convention that "" is a
;; quoted quote if it appears inside a quoted section, and allowing
;; CR,LF to be inside a quoted section.  The only reason this function
;; is here is because fare-csv:read-csv-line doesn't take a maximum
;; line length maximum; otherwise we'd just use FARE-CSV to do this.
(defun get-csv-line (s &key (scratch-string nil) 
		       (quote-char fare-csv:*quote*) line-number)
  (declare (type simple-string scratch-string)
	   (optimize speed))
  (let ((max-len (length scratch-string))
	(k 0))
    (declare (type fixnum k))
    (flet ((return-the-line ()
	     (return-from get-csv-line (subseq scratch-string 0 k)))
	   (add-char (c)
	     (when (= k max-len)
	       (error "CSV LINE number ~A too long (longer than ~A)" line-number max-len))
	     (setf (aref scratch-string k) c)
	     (incf k)))
	   
      (loop
	with in-quote = nil
	for i of-type fixnum from 0
	for cprev of-type (or null character) = nil then c
	for c of-type (or null character) = (read-char s nil nil)
	for cpeek of-type (or null character) = (if c (peek-char nil s nil nil))
	for is-a-eol = (or (eql c #\cr) (eql c #\lf)) 
	do 
	   (cond
	     ;; end of stream
	     ((not c)
	      (if (plusp k)
		  (progn
		    (when in-quote
		      (error "CSV stream ended in line ~A inside a quote" line-number))
		    (return-the-line))
		  (return nil)))
	     ;; end of line but not in a quote, so a real EOL
	     ((and is-a-eol (not in-quote))
	      (add-char c)
	      (when (or (eql cpeek #\cr) (eql cpeek #\lf))
		(add-char cpeek)
		(read-char s))
	      (return-the-line))
	     ;; quote char - it opens a quoted section, closes it, or marks a quoted-quote in
	     ;;   a quoted string
	     ((eql c quote-char)
	      (add-char c)
	      (cond
		((and (not in-quote) 
		      (eql cpeek quote-char)) ;; it's an empty string ""
		 (add-char quote-char) ;; add peeked quote
		 (read-char s)) ;; read the peeked quote
		;; entering quoted region
		((not in-quote)
		 (setf in-quote t))
		;; in a quote and the next char is a quote, so it's a quoted quote
		;;    (we already handled an imediately closed null-string "")
		((and in-quote
		      (eql cpeek quote-char))
		 (add-char quote-char) ;; add peeked quote
		 (read-char s)) ;; read the peeked quote
		;; in a quote, so this is ending quote
		(t
		 (setf in-quote nil))))
	     ;; any other char
	     (t
	      (add-char c)))))))
		    
	      
	      
	  

;; returns NIL for EOF, or next line not starting with comment
;; nvec is a one-element vector with the current line number
(defun get-next-non-comment-line (s comment-char nvec scratch-string)
  (loop for cpeek = (peek-char nil s nil nil)
	for line = (cond ((eql cpeek comment-char) ;; no fancy parsing for comment line
			  (file-io:read-line-of-max-length s (length scratch-string)))
			 ((not cpeek)
			  nil)
			 (t
			  (get-csv-line s :scratch-string scratch-string :line-number (aref nvec 0))))
	when line do (incf (aref nvec 0))
	when (or (not line) ;; return on EOF
		 (and (plusp (length line)) ;; not a comment
		      (not (eql (aref line 0) comment-char))))
	  do (return line)))

;; turn a comma separated line into a list of strings using FARE-CSV
(defun parse-csv-line (string)
  (declare (type string string))
  (with-input-from-string (s string)
    (fare-csv:read-csv-line s)))

;; convert output to simplest array of simplest type possible
(defun convert-to-simple-vec (vec)
  (let ((type (cond
		;; fixnums converted to fixnum vec; other ints will stay the same
		((every (lambda (x) (typep x 'fixnum)) vec)
		 'fixnum)
		;; if it's a blend of fixnums and float, then double-float output
		((every (lambda (x) (or (typep x 'fixnum)
					(typep x 'float)))
			vec)
		 'double-float)
		(t
		 t))))
    (map `(simple-array ,type (*))
	 (lambda (x) (if (eq type 'double-float)
			 (float x 1d0)
			 x))
	 vec)))
    
    
  


(defun read-csv-headers/columns-from-file
    (file &key 
	    (comment-char nil)
	    (hash-test 'equal)
	    (max-line-len #.(expt 2 13))
	    (max-num-lines  #.(expt 2 16))
	    (parse-time nil)
	    (date-convention :yyyy-mm-dd))
  "Read a CSV file, where the first (header) line contains field names.

If COMMENT-CHAR is T, then lines beginning with COMMENT-CHAR are
ignored. Each row must have the same number of elements as the 
header line, and each column is converted to a simple-array if possible.

If PARSE-TIME is T, then tries to parse times, using DATE-CONVENTION,
which can be :YYYY-MM-DD or :MM-DD-YYYY or :DD-MM-YYYY, or, taking
precedence, any ISO convention supported by
CL-DATE-TIME-PARSER:PARSE-DATE-TIME

Returns  
   (VALUES 
       COLUMN-HASH           ;; key=column name, val=column vector
       #(COLUMN-NAME-VECTOR  ;; vector of column names 
         COLUMN-VECTOR       ;; vector column vectors
         TYPE-VECTOR))       ;; vector of type of columns, one of
                                  :FLOAT :INTEGER :STRING :TIME :EMPTY T
"
  (when (not (probe-file file))
    (error "File ~A does not exist" file))
  (with-open-file (s file :direction :input)
    (let*  ((nvec (make-array 1 :initial-element 0)) ;; line counter
	    (scratch-string (make-string max-line-len))
	    (header-line (get-next-non-comment-line
			    s comment-char  nvec scratch-string))
	    (header-list (parse-csv-line header-line))
	    (ncols (length header-list))
	    ;; outvec is an array of adjustable arrays with fill pointer
	    (outvec (make-array
		     ncols
		     :initial-contents
		     (loop for i below ncols
			   collect (make-array 0 :adjustable t
						 :fill-pointer t))))
	    ;; out-types is a type :integer :floaot :string or T (for mixed)
	    (out-types (make-array ncols :initial-element nil)))
      ;;
      (when (not (every (lambda (thing) (or (not thing) (stringp thing)))
			(mapcar 'parse-item header-list)))
	(error
	 "Each value of the first row is not a string (column name) or NULL: ~S"
	 header-line))
      ;;
      (loop for line = (get-next-non-comment-line
			s comment-char nvec scratch-string)
	    for iline from 1
	    until (not line)
	    when (> iline max-num-lines)
	      do
	      (error "Number of lines in CSV file exceeds MAX-NUM-LINES=~A" max-num-lines)
	    do (loop with split-line = (parse-csv-line line)
		       initially (when (not (= (length split-line) ncols))
				   (error
	     "Number of fields of line #~A is ~A, and does not match the number of headers ~A"
				    (aref nvec 0) (length split-line) ncols))
		     for item in split-line
		     for icol from 0
		     do
			(multiple-value-bind (val type)
			    (parse-item item  :parse-time parse-time :date-convention date-convention)
			  (vector-push-extend val (aref outvec icol))
			  ;; note whether type of column is the same
			  (setf (aref out-types icol)
				(cond ((not (aref out-types icol)) ;; no type yet
				       type)
				      ((eq type (aref out-types icol)) ;; type is same
				       type)
				      (t ;; type has changed
				       #+nil ;; diagnostic
				       (format t "type of icol ~A in line ~A changed from ~A to ~A~%" icol iline (aref out-types icol) type)
				       t))))))
      ;;
      (let ((ov (map 'vector #'convert-to-simple-vec outvec))
	    (hash (make-hash-table :test hash-test)))
	(loop for head in header-list
	      for vec across ov
	      do (setf (gethash head hash) vec))
      (values
       hash
       (vector
	(map 'vector 'identity header-list)
	ov
	out-types))))))
			  
			  
			  

		 
		   
      
    
