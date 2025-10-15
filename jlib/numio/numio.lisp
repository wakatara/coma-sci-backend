;; numio package for reading in file columns into  vectors of numbers


;; note - modified 2013-07-31 to buffer into a string vector, because 
;; FILE-POSITION does not work for all lisp variants (eg, abcl)


(in-package numio)

(defparameter *max-line-len* (expt 2 12))
(defparameter *max-nlines* 1000000)
(defparameter *max-bytes*  (expt 2 24))

(defmacro whitespace-p (c)
  `(member ,c '(#\space #\tab #\cr #\lf)))

 
;; a structure that ingests a file or stream into a vector of lines
(defstruct fbuff
  (type nil)      ;; :file or :stream - the original type
  (orig-file nil)
  (stringvec nil :type (or null (simple-array T (*)))) ;; the vecor of lines
  (nlines nil))    ;; the current line


	   
	
 

(defun generic-file-to-input-stream (file)
  "Convert path,string, or stream to a stream"
  (cond ((pathnamep file)
	 (open file :direction :input))
	((stringp file)
	 ;; (open (make-pathname :name file)  :direction :input) this breaks bc directory is wrong
	 (open file :direction :input)) 
	((and (streamp file) (input-stream-p file))
	 file)
	(t
	 (error "generic-file-to-input-stream: FILE not a path, string, or stream"))))


(defmacro with-fstr ((file streamvar) &body stuff)
  `(let ((,streamvar nil))
     (unwind-protect
	 (progn
	   (setf ,streamvar (generic-file-to-input-stream ,file))
	   ,@stuff)
       (when (and (streamp ,streamvar)
		  (not (streamp ,file)))
	 (close ,streamvar)))))



(defun read-generic-file-into-fbuff (file &key
				       (line-selector nil)
				       (max-line-len *max-line-len*) 
				       (max-nlines *max-nlines*)
				       (max-bytes *max-bytes*))
  (declare (type fixnum max-line-len max-nlines max-bytes))
  (with-fstr (file s)
    (let ((linebuf (make-string max-line-len)) ;; could use base-char and be 2/3 faster but less general
	  (line-list nil))
      ;;
      (flet ((maybe-add-line (n)
	       (let ((line (subseq linebuf 0 n)))
		 (when (or (not line-selector)
			   (funcall line-selector line))
		   (push line line-list)))))
	;;
	(loop with n of-type fixnum = 0 ;; index in line
	    with nline of-type fixnum = 0
	    for nbytes of-type fixnum = 0
	    for c = (read-char s nil nil)
	    do  (when (> n max-line-len) 
		  (error "Line ~A longer than ~A" nline max-line-len))
		(when (> nline max-nlines)
		  (error "Too many lines in file: more than ~A" max-nlines))
		(when (> nbytes max-bytes)
		  (error "Too many bytes in file: more than ~A" max-bytes))
		(cond ((not c) ;; eof 
		       (when (plusp n) (maybe-add-line n))
		       (return))
		      ;; unix uses a #\lf, but dos uses #\cr #\lf
		      ((or (eql c #\lf) (eql c #\cr))
		       ;; DOS-style: clear next #\lf if we read a #\cr
		       (when (eql c #\cr)
			 (let ((cc (peek-char nil s nil nil)))
			   (when (eql cc #\lf) (read-char s nil nil))))
		       (maybe-add-line n)
		       (setf n 0))
		      (t ;; any other char
		       (setf (aref linebuf n) c)
		       (incf n))))
      (make-fbuff 
       :type (if (streamp file) :stream :file)
       :orig-file file
       :stringvec (nreverse (coerce line-list 'vector))
       :nlines (length line-list))))))
			  
		


	      



(defun count-fields-in-string (str)
  "Count the number of whitespace separated fields in a string"
  (declare (type simple-string str)
	   (optimize speed))
  (loop
   with nf of-type (unsigned-byte 28) = 0
   with in-field = nil ;; inside a field?
   for c of-type character across str
   do
   (cond ((whitespace-p c)
	  (setf in-field nil))
	 (t ;; not a whitespace
	  (when (not in-field)
	    (incf nf)
	    (setf in-field t))))
   finally (return nf)))
	  
   
	   
  
(defun read-double-cols-fixed (fbuff ncol nrow)
  "read numerical (double precision) data from FILE, assuming NROW rows
   and NCOL columns. Return data as a vector of double-precision
   column vectors"
  (declare (type fbuff fbuff)
	   (type (unsigned-byte 28) ncol nrow))
  (let* ((vecvec (make-array ncol))
	 (buf (make-string 2048))
	 (val 0.0d0))
    (declare (type double-float val)
	     (optimize speed))

    (dotimes (i ncol)
      (declare (type (unsigned-byte 28) i))
      (setf (aref vecvec i)
	    (make-array nrow :element-type 'double-float)))

    (dotimes (i nrow)
      (declare (type (unsigned-byte 28) i))
      (with-input-from-string (s (aref (fbuff-stringvec fbuff) i))
	(dotimes (j ncol)
	  (declare (type (unsigned-byte 28) j))
	  (progn
	    (setf val (get-object-from-stream s :double-float buf i))
	    (setf (aref (the (simple-array double-float (*))
			  (aref vecvec j))
			i) val)))))
    
      vecvec))
  
(defun read-double-cols (file
			 &key
			   (line-selector nil)
			   (max-line-len *max-line-len*) 
			   (max-nlines *max-nlines*)
			   (max-bytes *max-bytes*))
  "read numerical (double) columns from FILE, returning vector of double-precision cols"
  (let* ((fbuff (read-generic-file-into-fbuff file
					      :line-selector line-selector
					      :max-line-len max-line-len
					      :max-nlines max-nlines
					      :max-bytes max-bytes))
	 (nrow (fbuff-nlines fbuff))
	 (ncol (if (plusp (fbuff-nlines fbuff))
		   (count-fields-in-string (aref (fbuff-stringvec fbuff) 0))
		   0)))
					   
    (if (or (= nrow 0) (= ncol 0))
	(progn
	  (error (format t "File ~A has no elements in read-double-cols" 
			 file))))
    (read-double-cols-fixed fbuff ncol nrow)))


(defun read-cols (file type-list
		  &key
		    comment-char (nskip-initial-lines 0)
		    (max-line-len *max-line-len*)
		    (line-selector nil) 
		    (max-nlines *max-nlines*)
		    (max-bytes *max-bytes*))

  "read columns from FILE, returning vectors of appropriate types -
allowed types in type-list are 
  :integer :single-float :double-float :string :ignore
if the type is :ignore, then return a NIL in the appropriate slot
of the returned vector

If COMMENT-CHAR is a char, then lines that start with it are ignored

If LINE-SELECTOR is given, it is a function 
   (LAMBDA (LINE)) 
 that returns non-NIL if the line is to be accepted or ignored.

If NSKIP-INTITIAL-LINES is greater than zero, skip this many lines at start."
  (declare (type (or null base-char) comment-char)
	   (type (integer 0) nskip-initial-lines)
	   (type list type-list))

  (let* ((fbuff (read-generic-file-into-fbuff file
					      :line-selector line-selector
					      :max-line-len max-line-len
					      :max-nlines max-nlines
					      :max-bytes max-bytes))
	  (allowed-types '(:integer :single-float
				   :double-float :string :ignore))
	  (ncomments (loop for i from nskip-initial-lines below (fbuff-nlines fbuff)
			   for line = (aref  (fbuff-stringvec fbuff) i)
			   sum (if (and (plusp (length line))
					  (eql (aref line 0) comment-char))
				     1 0)))
	  (nrow 0)
	  (ncol (length type-list)))

     (loop for type in type-list
	   do (if (not (member type allowed-types))
		  (error "read-cols: read type ~A not in allowed types ~A"
			 type allowed-types)))
     (setf nrow (- (fbuff-nlines fbuff) ncomments))
     (decf nrow nskip-initial-lines)
     (if (or (= nrow 0) (= ncol 0))
	   (error (format t "File ~A has no elements in read-double-cols" 
			  file)))
     (read-cols-fixed fbuff type-list nrow :comment-char comment-char 
		      :nskip-initial-lines nskip-initial-lines)))
  
;; helper function for read-cols-fixed, transforming a vector of types
;; into a vector of vectors to hold read-in values --- note that most
;; ints can be stored un-boxed, so we don't give integers a special 
;; type of vector [eg (signed-byte 32)], because it would help only
;; for integers between 28 and 32 bits in length
(defun rcf-make-target-vector-from-type-list (type-list nrow)
  (make-array
   (length type-list)
   :initial-contents 
   (mapcar (lambda (type)
	     (if (eq type :ignore)
		 nil
	       (make-array nrow :element-type
			   (cond ((eq type :double-float)
				  'double-float)
				 ((eq type :single-float)
				  'single-float)
				 (t t))
			   :initial-element
			   (cond ((eq type :double-float)
				  '0.0d0)
				 ((eq type :single-float)
				  '0.0)
				 (t 0)))))
	   type-list)))


;; helper function for read-cols-fixed that reads an
;; object of the desired type
(defun get-object-from-stream (stream type tmpbuf irow
			       &optional
				 ignore-errors 
				 (eof-error-p t) (eof-value NIL))
  (declare (type stream stream)
	   (type simple-string tmpbuf)
	   (type (or null (unsigned-byte 28)) irow)
	   (optimize speed))
  ;; consume all leading spaces but ignoring EOF for now
  (loop for c = (read-char stream nil nil)
	until (not (member c '(#\space #\tab #\cr #\lf)))
	finally (if c (unread-char c stream)))
  (loop
   with n of-type (unsigned-byte 28) = (length tmpbuf)
   for i of-type (unsigned-byte 28) = 0 then (1+ i)
   for c = (read-char stream nil nil)
   until (or (not c) (whitespace-p c)) ;; either EOF or whitespace
   do
   ;;
   (when (= i n)
     (if irow
	 (error "object on line ~A too long in get-object-from-stream - buffer=<~A>" 
		(1+ irow) tmpbuf)
	 (error "object too long in get-object-from-stream - buffer=<~A>" tmpbuf)))
   ;;
   (setf (aref tmpbuf i) c)
   finally

   ;; when this is EOF and nothing read, either error or return EOF value
   (when (and (not c) (zerop i))
     ;; if  nothing read, error, or return EOF value
     (if eof-error-p
	 (error "End of file in GET-OBJECT-FROM-STREAM")
	 (return-from get-object-from-stream (values eof-value tmpbuf))))

   (when (and c (member c '(#\cr #\lf) :test #'char=))
     (unread-char c stream))
   
   (flet ((read-the-object ()
	    (cond ((eq type :string)
		   (subseq tmpbuf 0 i))
		  ((or (eq type :integer) (eq type :integer32))
		   (parse-integer tmpbuf :end i :junk-allowed nil))
		  ((eq type :double-float)
		   (numio:parse-float tmpbuf :end i :junk-allowed nil))
		  ((eq type :single-float)
		   (coerce
		    (the double-float 
			 (numio:parse-float tmpbuf :end i :junk-allowed nil))
		    'single-float))
		  ((eq type :ignore) ;; do nothing
		  t)
		  ((eq type t)
		   (let ((*read-eval* nil))
		     (read-from-string tmpbuf))))))
     (if ignore-errors
	 (return (values (ignore-errors (read-the-object)) tmpbuf i))
	 (return (values (read-the-object) tmpbuf i))))))

  
(defun read-object-from-stream (stream object-type &key (buffer nil) (ignore-errors t)
				(eof-error-p t) (eof-value NIL))
  "Read an object of type :STRING :INTEGER :INTEGER32 :DOUBLE-FLOAT
:SINGLE-FLOAT or :IGNORE or T (for Lisp object) from STREAM.  BUFFER
is a simple-string into which to read the data, or NIL to make a
buffer of length 256.  This function is a front end for
GET-OBJECT-FROM-STREAM.  If IGNORE-ERRORS is T, then errors on parsing
are ignored in favor of returning NIL.  The 2nd value returned is the
buffer, and the 3rd value is the final position in the buffer.

EOF-ERROR-P AND EOF-VALUE work just as in READ and READ-CHAR
"
  (let ((buffer (or buffer (make-string #.(expt 2 8) :initial-element
					#\space))))
    (get-object-from-stream stream object-type buffer nil ignore-errors
			    eof-error-p eof-value)))


(defun read-objects-from-stream (stream type-list &key (tmpbuf nil) (irow 0))
    "Read objects from current line, in same TYPE-LIST notation as read-cols - IROW
is the current row, for error reporting only. Objects of type :IGNORE are ignored, and
replaced with a placeholder in the return list."
    (declare (type stream stream)
	     (type list type-list)
	     (type (or null string) tmpbuf))
    (loop
       with tmpbuf = (or tmpbuf (make-string  #.(expt 2 12) :initial-element #\space))
       for type in type-list
       for thing =  (get-object-from-stream stream type tmpbuf irow)
       when (not (eq type :ignore))
       collect thing))


;; nrow is the number of rows not including comments 
(defun read-cols-fixed (fbuff type-list nrow
			&key comment-char (nskip-initial-lines 0))
  (declare (type (or null character) comment-char)
	   (type list type-list)
	   (type fixnum nrow nskip-initial-lines)
	   (type fbuff fbuff)
	   (optimize speed))
  (let ((erow nskip-initial-lines) ;; absolute current row in file
	(ecol 0)) ;; current column number
    (multiple-value-bind (retval error)
	(ignore-errors ;; hide error so we can print out current line and column
	  (loop
	    with tmpbuf = (make-string #.(expt 2 12) :initial-element #\space)
	    with tvec = (rcf-make-target-vector-from-type-list type-list nrow)
	    with irow of-type (unsigned-byte 28) = 0
	    for current-line of-type (or null string)
	      = (if (< erow (fbuff-nlines fbuff))
		    (aref (fbuff-stringvec fbuff) erow))
	    for is-comment-line
	      = (and comment-char current-line
		     (plusp (length current-line))
		     (char= comment-char (aref current-line 0)))
	    until (= irow nrow)
	    do (incf erow) ;; start at erow=1
	       (setf ecol 1) ;; start at ecol=1
	       (when (not is-comment-line)
		 (with-input-from-string (line-stream current-line)
		   (loop 
		     for type in type-list
		     for v across tvec
		     for thing = (get-object-from-stream line-stream
							 type tmpbuf irow)
		     do (when (not (eq type :ignore))
			  (setf (aref v irow) thing))
			(incf ecol))
		   (incf irow)))
	    finally
	 (return tvec)))
      ;; 
      (if retval
	  retval
	  (error "Error in ~A at Line ~A Field ~A (type ~A): ~A"
		 (fbuff-orig-file fbuff) erow ecol (nth (1- ecol) type-list) error)))))
	  
(defun make-array-for-read-2d-array (nrow ncol type)
  (make-array (list nrow ncol)
	      :element-type
	      (cond ((eq type :double-float)
		     'double-float)
		    ((eq type :single-float)
		     'single-float)
		    ((eq type :integer32)
		     '(signed-byte 32))
		    (t t))))

	
(defun read-2d-array (file &key (type t) comment-char
		      (max-line-len *max-line-len*) 
		      (max-nlines *max-nlines*)
		      (max-bytes *max-bytes*))
  (let ((allowed-types
	 '(t :integer :integer32 :single-float :double-float :string)))
    (when (not (member type allowed-types))
      (error "read-2d-array: type=~A must be one of ~S" type allowed-types)))

   (let* ((fbuff (read-generic-file-into-fbuff file 
					       :max-line-len max-line-len
					       :max-nlines max-nlines
					       :max-bytes max-bytes))
	  (ncomments (loop for i from 0 below (fbuff-nlines fbuff)
			   for line = (aref  (fbuff-stringvec fbuff) i)
			   sum (if (and (plusp (length line))
					(eql (aref line 0) comment-char))
				   1 0)))
	  (nrow (- (fbuff-nlines fbuff) ncomments))
	  ;; first line that's not a comment
	  (inotcom
	     (loop for i from 0 below (fbuff-nlines fbuff)
		   for line = (aref (fbuff-stringvec fbuff) i)
		   when (and (plusp (length line))
			     (not (eql (aref line 0) comment-char)))
		     do (return i)))
	  (ncol (if inotcom
		    (count-fields-in-string
		     (aref (fbuff-stringvec fbuff) inotcom))
		    0)))
     
     (when (zerop ncol)
       (error "read-2d-array: no columns to read"))
     (loop
      with tmpbuf = (make-string #.(expt 2 12) :initial-element #\space)
      with a = (make-array-for-read-2d-array nrow ncol type)
      for ir of-type (unsigned-byte 28) below nrow
      for line = (aref (fbuff-stringvec fbuff) ir)
      for first-char = (if (plusp (length line)) (aref line 0))
      do
      (when (and first-char (not (eql first-char comment-char)))
	(with-input-from-string (s line)
	  (loop
	    for ic of-type (unsigned-byte 28) below ncol
	    for thing = (get-object-from-stream s type tmpbuf ir)
	    do (setf (aref a ir ic) thing))))

      finally (return a))))
		 
    
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	


(defmacro with-cols ((file var-colnum-type-list &key comment-char) &body body)
  "Macro that binds columns read from a file to a set of variables

 (with-double-cols (some-data-file
                    ((xvec 0 :single-float) ;; bind xvec to column 0
                     (yvec 3 :double-float) ;; etc
                     (zvec 7 :string))
                    :comment-char #\#)
  ... do stuff with vectors xvec,yvec,zvec
  )"
  (let* ((vvar (gensym "file-vecs-"))
	 (maxcol  (apply 'max (mapcar 'second var-colnum-type-list)))
	 (read-col-list
	  (loop with lst = (make-list (1+ maxcol) :initial-element :ignore)
		for thing in var-colnum-type-list
		for pos = (second thing)
		for type = (third thing)
		do (setf (elt lst pos)  type)
		finally (return lst)))
	 (bindlist `(,vvar (numio:read-cols ,file ',read-col-list
			    :comment-char ,comment-char)))
	 (let-binding-list
	  (mapcar 
	   (lambda (vct) `(,(first vct) (aref ,vvar ,(second vct))))
	   var-colnum-type-list)))
    `(let* (,bindlist ,@let-binding-list) ,@body)))
