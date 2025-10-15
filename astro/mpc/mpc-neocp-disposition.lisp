#|

Figur out where a previous object went using MPC previous designation page

|#

(in-package mpc)

(defparameter *neo-disposition-page*
  "https://www.minorplanetcenter.net/iau/NEO/ToConfirm_PrevDes.html")


(defun %get-previous-disposition-parsed-html ()
  (let ((html (drakma:http-request *neo-disposition-page*)))
    (cl-html-parse-walker:convert-html-list-to-html-tags
     (cl-html-parse:parse-html html))))


(defstruct neodisp
  name
  final-name
  not-confirmed
  does-not-exist
  mpec)

(defun %parse-disposition-line (str)
  (declare (type string str))
  (let* ((n= (position #\= str))
	 (final-name
	   (if n= (string-trim " " (subseq str 0 n=))))
	 ;; namestr is after = if there was a final name,
	 ;; else it's just string
	 (namestr
	   (if final-name
	       (string-left-trim " " (subseq str (1+ n=)))
	       str))
	 ;; name is now everything up to first space
	 (name
	   (subseq namestr 0 (position #\space namestr)))
	 (not-confirmed (not (not (search "not confirmed" str))))
	 (does-not-exist (not (not (search "does-not-exist" str))))
	 (mpec-pos (search "MPEC" str))
	 (mpec (if mpec-pos
		   (with-output-to-string (s)
		       (loop for i from (+ 4 mpec-pos) below (length str)
			     for c = (aref str i)
			     ;; probably stops on ] but not sure
			     when (or (alphanumericp c)
				      (eql c #\-))
			       do (write-char c s))))))
    (make-neodisp
     :name name
     :final-name final-name
     :not-confirmed not-confirmed
     :does-not-exist does-not-exist
     :mpec mpec)))



(defun %stringify-html-thing (thing)
  (declare (type (or string cl-html-parse-walker:html-tag) thing))
  (cond ((stringp thing)
	 thing)
	(t
	 (with-output-to-string (s)
	   (loop for item in (cl-html-parse-walker:html-tag-contents thing)
		 do
		    (write-string (%stringify-html-thing item) s))))))


(defun %get-previous-dispositions-from-parsed-html (phtml &key (as-hash nil))
  (let* ((body-block (or (html-scrape:find-tag phtml  :tag-name :body)
			 (error "No <BODY> found")))
	 (div-block (or (html-scrape:find-tag body-block :tag-name :div :id "main")
			(error "No <DIV id='main'> in <BODY>")))
	 (ul-block (or (html-scrape:find-tag
			div-block
			:tag-name :ul)
		       (error "No <UL> in  <P> in <DIV id='main'> in <BODY>")))
	 (li-list
	   (loop for thing in (cl-html-parse-walker:html-tag-contents ul-block)
		 when (and (cl-html-parse-walker:html-tag-p thing)
			   (eq (cl-html-parse-walker:html-tag-name thing)
			       :li))
		   collect thing))
	 (li-strings 
	   (mapcar
	    (lambda (item)
	      (string-trim
	       '(#\tab #\space #\cr #\lf)
	       (%stringify-html-thing item)))
	    li-list))
	 (dispositions
	   (mapcar '%parse-disposition-line li-strings)))

    (if (not as-hash)
	dispositions
	(loop with h = (make-hash-table :test 'equal)
	      for disp in dispositions
	      do (setf (gethash (neodisp-name disp) h)
		       disp)
	      finally (return h)))))
    

(defun get-mpc-previous-dispositons (&key (as-hash t))
  "Return a hash table of NEODISP objects, eg
   #S(mpc::neodisp
        :name \"A119H5w\"
        :final-name \"2024 PS5\"
        :not-confirmed nil
        :does-not-exist nil
        :mpec \"2024-P169\")
If AS-HASH is true (default), then the result is a hash-table on NAME.
Otherwise, return a list."
  (%get-previous-dispositions-from-parsed-html
   (%get-previous-disposition-parsed-html)
   :as-hash as-hash))
