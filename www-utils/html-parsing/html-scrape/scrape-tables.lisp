
(in-package html-scrape)

(defstruct table-element
  (contents nil)
  (name    nil) ;; :TC  or :TH  
  (colspan nil)
  (rowspan nil)
  (irow nil)  ;; the row number of this element (first one, if rowspan>1)
  (icol nil)  ;; the column number of this element (first one, if colspan>1)
  (keywords nil))

;; a reference to another array element, when it is a SPAN
(defstruct table-element-ref
  irow
  icol)

;; given the outrmost 'dummmy' tag (name=T) produced by cl-html-parse-walker,
;; find the table that matches all criteria given
(defun %find-table-in-html-tags (outer-html-tag &key n-table id table-class)
  (declare (type chpwalk:html-tag outer-html-tag))
  (let ((i-table 0))
    (labels ((recurser (these-html-tags)
	       (loop for thing in these-html-tags
		     when (chpwalk:html-tag-p thing) ;; ignore strings
		       do
			  (when (eq (chpwalk:html-tag-name thing) :table) ;; is a table
			    (incf i-table)
			    ;; check that this is correct table
			    (when
				(and
				 (or (not n-table) (= i-table n-table))
				 ;; and this is the right table name
				 (or (not id)
				     (equalp (chpwalk:get-keyval-for-html-tag
					      :id thing)
					     id))
				 ;;  and this is the right class name
				 (or (not table-class)
				     (equalp (chpwalk:get-keyval-for-html-tag
					      :class thing)
					     table-class)))
			      (return thing))) ;; DONE
			  ;;
			  ;; otherwise, iterate on this tag, and if the iteration
			  ;; returns a value, retun this value.
			  (let ((result
				  (recurser
				   (chpwalk:html-tag-contents thing))))
			    (when result (return result)))
			  ;; else nothing
		     finally (return nil))))
      (recurser
       (list outer-html-tag)))))
			  
		     
      
		     
		     
    
       

(defun convert-html-table-into-table-element-list
    (outer-html-tag &key
		    (n-table nil)
		    (id nil)
		    (table-class nil)
		    (compute-indices t))
    "Walk OUTER-HTML-TAG, an outer CL-HTML-PARSE-WALKER:HTML-TAG
produced by CL-HTML-PARSE-WALKER:CONVERT-HTML-LIST-TO-HTML-TAGS,
find a table that satisifies

   N-TABLE      - the number of the table on the page, AND
   ID           - the HTML NAME of the table, AND
   TABLE-CLASS  - the html CLASS of the table 

And return a list of rows, each containing a list of TABLE-ELEMENT
representing one <TD> or <TH> tag.

If FILL-INDICES is T (default) then fill in TABLE-TAG-IROW, TABLE-TAG-ICOL
with what we think the 0-base indices of the table are for each tag."
  (declare (type chpwalk:html-tag outer-html-tag))
  
  (let ((table-tag (%find-table-in-html-tags outer-html-tag
					     :n-table n-table
					     :id id
					     :table-class table-class)))
    (let ((rows nil)
	  (this-row nil))
      (labels ((get-one-row (row-tag &key (recursing nil))
		 (when (not recursing) (setf this-row nil)) ;; starting this row
		 (loop for thing in (chpwalk:html-tag-contents row-tag)
		       when (chpwalk:html-tag-p thing)
			 do
			    (cond
			      ;; if it's a <TH> or <TD> tag, the save it and
			      ;; stop recursing
			      ((member (chpwalk:html-tag-name thing)
				       '(:th :td))
			       (push (make-table-element
				      :contents (chpwalk:html-tag-contents thing)
				      :name (chpwalk:html-tag-name thing)
				      :colspan
				      (or
				       (ignore-errors
					(parse-integer
					 (chpwalk:get-keyval-for-html-tag :colspan thing)))
				       1)
				      :rowspan
				      (or
				       (ignore-errors
					(parse-integer
					 (chpwalk:get-keyval-for-html-tag :rowspan thing)))
				       1)
				      :keywords
				      (chpwalk:html-tag-keywords thing))
				     this-row))
			      ;; otherwise, if it is <TR> or <TABLE> tag, something is broken
			      ((member (chpwalk:html-tag-name thing)
				       '(:tr :table))
			       nil)
			      ;; otherwise it's a tag like <i> weirdly
			      ;; placed in a table outside a cell, so we
			      ;; dig into its content to extract the columns
			      (t
			       (get-one-row thing :recursing t)))
		       finally
			  (when (not recursing) (return (reverse this-row)))))
	       ;;
	       (get-rows (parent-tag &key (recursing nil))
		 (when (not recursing) (setf rows nil)) ;; starting rows; not needed
		  (loop for thing in (chpwalk:html-tag-contents parent-tag)
		       when (chpwalk:html-tag-p thing)
			 do
			    (cond
			      ;; if it's a <TR> tag, just grab the row and stop recursing
			      ((member (chpwalk:html-tag-name thing)
				       '(:tr))
			       (push (get-one-row thing) rows))
			      ;; if it's a <TABLE> or wrong inner tag, something is broken
			      ((member (chpwalk:html-tag-name thing)
				       '(:tr :td :th :table))
			       nil)
			      ;; otherwise it is a regular tag that weirdly inside a table,
			      ;; so dig inside it
			      (t
			       (get-rows thing :recursing t)))
			finally
			   (when (not recursing)
			     (return (reverse rows))))))
	
	(when table-tag
	  (let ((rows (get-rows table-tag)))
	    (when compute-indices ;; the hash is discarded but the indices get filled in
	      (%make-index-hash-for-table-element-list rows))
	    rows))))))



;; verify that each row has at least one element with
;; rowspan=1; if not, we don't know what to do
(defun %verify-that-rows-are-sane (table-row-list)
  (loop for i from 1
	for row in table-row-list
	do
	   (when (not (some (lambda (elem)
			      (= 1 (table-element-rowspan elem)))
			    row))
	     (error "Row ~A of table does not a single element with ROWSPAN=1; can't parse table because the layout is not clear."
		    i))))




;; create a hash table with KEY = (irow icol)
;; and VALUE = TABLE-ELEMENT if original, or
;;     VALUE = #S(CL-HTML-PARSE-WALKER:TABLE-ELEMENT-REF :IROW IROW :ICOL ICOL)
;; if this cell contains a colspan, rowspan'ed element
(defun %make-index-hash-for-table-element-list (table-row-list)
  (%verify-that-rows-are-sane table-row-list)
  (flet
      ;; add entries with KEY=(irow-spanned icol-spanned) and VALUE=(icol irow)
      ;; that are really spanned versions of (icol irow)
      ((add-spanned-elements (elem hash irow icol)
	 (loop with old-indices = (list irow icol)
	       for irow-spanned from irow below (+ irow (table-element-rowspan elem))
	       do (loop for icol-spanned from icol below (+ icol (table-element-colspan elem))
			when (or (> irow-spanned irow) (> icol-spanned icol))
			  do
			     (when (gethash (list irow-spanned icol-spanned) hash)
			       (error "Element ~A is being spanned into index ~A,~A and collided with 
~A"  elem irow-spanned icol-spanned (gethash (list irow-spanned icol-spanned) hash)))
			     ;;
			     (setf (gethash (list irow-spanned icol-spanned) hash)
				   (make-table-element-ref :irow irow :icol icol))))))
    
    (loop with hash = (make-hash-table :test 'equalp)
	  with irow = 0
	  for row in table-row-list
	  do (loop with icol = 0
		   for elem in row
		   do
		      ;; step past any icol indices being used by spanning rows/cols
		      (loop while (gethash (list irow icol) hash)
			    do (incf icol))
		      ;(assert (not (gethash (list irow icol) hash)))
		      (setf (gethash (list irow icol) hash) elem)
		      (setf (table-element-irow elem) irow)
		      (setf (table-element-icol elem) icol)
		      ;; add those elements that span rows,cols
		      (add-spanned-elements elem hash irow icol))

	     (incf irow)
	  finally (return hash))))
		 
				     
;; turn the hash table created by %make-index-hash-for-table-element-list
;; into an array
(defun %make-array-from-index-hash (index-hash)
  (let ((rowmax 0) (colmax 0))
    (loop for row-col being the hash-key of index-hash
	  do (setf rowmax (max rowmax (first row-col)))
	     (setf colmax (max colmax (second row-col))))

    (loop with a = (make-array (list (1+ rowmax) (1+ colmax)) :initial-element nil)
	  for row-col being the hash-key of index-hash
	  for val being the hash-value of index-hash
	  for irow = (first row-col) and icol = (second row-col)
	  do (setf (aref a irow icol) val)
	  finally (return a))))
	  



(defun default-table-element-parser-function (content &key irow icol)
  (declare (ignore irow icol))
  content)
	   
				      
(defun scrape-html-table-into-array
    (outer-html-tag &key
		      (n-table nil)
		      (id nil)
		      (table-class nil)
		      (element-parser-function #'default-table-element-parser-function)
		      (un-span-elements t))
  "Walk OUTER-HTML-TAG, an outer CL-HTML-PARSE-WALKER:HTML-TAG
produced by CL-HTML-PARSE-WALKER:CONVERT-HTML-LIST-TO-HTML-TAGS,
find a table that satisifies

   N-TABLE     - the number of the table on the page, AND
   ID          - the HTML ID of the table, AND
   TABLE-CLASS - the html CLASS of the table 


Return an array, each array element is one of 
  - NIL, for no value, eg when an array is expanded because another row has
    more columns
  - a LIST of strings and HTML-ELEMENT structures, representing the table
    element here
  - #S(CL-HTML-PARSE-WALKER:TABLE-ELEMENT-REF :IROW IROW :ICOL ICOL),
     a pointer structure representing the index elsewhere in the array,
     pointing to the <TD> element that was ROWSPAN'ed or COLSPAN'ed into
     this space.

   ELEMENT-PARSER-FUNCTION is of the form
         (ELEMENT-PARSER-FUNCTION CELL-CONTENT :IROW IROW :ICOL ICOL)
   parses the elements.  By default it returns CELL-CONTENT as-is.

   Using #'HTML-SCRAPE:EXTRACT-VALUE-FROM-PARSED-HTML or (for more control)
   (HTML-SCRAPE::MAKE-EXTRACTOR-FOR-PARSED-HTML :DATE-CONVENTION :MM-DD-YY)
   will try to convert to numbers and dates and will fall back to
   extracted strings.   Or use a custom function with knowlege of the table.

   Any elements were spanned (colspan or rowspan) remain in the form
   #S(CL-HTML-PARSE-WALKER:TABLE-ELEMENT-REF :IROW IROW :ICOL ICOL) unless
   UN-SPAN-ELEMENTS is set (which is done by default).
"
  (declare (type chpwalk:html-tag outer-html-tag))

  (let ((element-list
	  (convert-html-table-into-table-element-list
	     outer-html-tag
	     :n-table n-table
	     :id id
	     :table-class table-class
	     :compute-indices nil)))
    (when element-list
      (let ((array 
	      (html-scrape::%make-array-from-index-hash
	       ;; make a hash with KEY=irow,icol and VAL=TABLE-ELEMENT or VAL=(IROW ICOL)
	       ;; and figure out what the indices are
	       (%make-index-hash-for-table-element-list
		element-list))))
	;;
	;; perform conversions 
	(loop for i below (array-total-size array)
	      for thing = (row-major-aref array i)
	      when (table-element-p thing)
		do (setf (row-major-aref array i)
			 (funcall element-parser-function (table-element-contents thing)
				  :irow (table-element-irow thing)
				  :icol (table-element-icol thing))))
	;;
	;; un-span if requested, so that '(iy ix) becomes the actual element there
	(when un-span-elements
	  (loop with ny = (array-dimension array 0)
		with nx = (array-dimension array 1)
		for i below (array-total-size array)
		for thing = (row-major-aref array i)
		when (table-element-ref-p thing)
		  do
		     (let ((iy (table-element-ref-irow thing))
			   (ix (table-element-ref-icol thing)))
		       (when (not (and (< -1 iy ny)
				       (< -1 ix nx)))
			 (error "Spanned element is out of bounds IY=~A, IX=~A for array NY=~A, NX=~A" iy ix ny nx))
		       (setf (row-major-aref array i) (aref array iy ix)))))
	
	array))))
    
   
  
