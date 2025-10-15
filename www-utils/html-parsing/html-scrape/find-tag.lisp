
(in-package html-scrape)



(defun find-tag  (html-thing &key tag-name n id class test-func (return :first))
  "Find an HTML-TAG (or string), returning first instance if RETURN is :FIRST, or
   all instances if RETURN is :ALL.

  HTML-THING can be an HTML-TAG, string, or list of HTML-TAG and strings and lists.

  - TAG-NAME is the HTML-TAG-NAME, like :TABLE or :A
  - CLASS    is a string for the CLASS attribute of tag
  - N        is the number of the tag (N=1 for first tag in document)
  - ID       is the HTML ID attribute of the tag
  - TEST-FUNC is a function that given an HTML-TAG or a string returns
    T or F.  Thus it can be used to return strings in the document as well;
    for example TEST-FUNC = 'stringp  and RETURN = :ALL returns all strings
    in the document.

    If TEST-FUNC returns a true value other than T, then that value
    is returned.    TEST func can have internal state, if it closes around
    a private set of variables, which makes it useful for returning
    a tag after some particular content has been seen.

The conditions TAG-NAME,CLASS,N,ID,TEST-FUNC are logically and'ed together,
but only condition TEST-FUNC is applied to strings."
  
  (declare (type (or chpwalk:html-tag string list) html-thing)
	   (type (member :first :all) return))
  (let ((i-thing 0) ;; counter for thing
	(result-list nil))
    (block done-block
      (labels ((recurser (thing)
		 (let ((tfr nil)) ;; test-func-result
		   (cond
		     ;; if it's a list, iterate it
		     ((listp thing)
		      (dolist (sub-thing thing) (recurser sub-thing)))
		     ;; for strings, test if test-func gives an answer
		     ((stringp thing)
		      (when (and test-func
				 (setf tfr (funcall test-func thing)))
			(push
			 ;; if test-func returned something other than True
			 ;; then that value is to be returned
			 (if (eq tfr t)
			     thing
			     tfr)
			 result-list)
			(when (eq return :first) (return-from done-block))))
		     ;; if it's a tag, check if correct one
		     ((chpwalk:html-tag-p thing)
		      ;;
		      ;; the test-func is applied to ALL tags so
		      ;; it can maintain its internal state
		      (when test-func
			(setf tfr (funcall test-func thing)))
		      ;; if all the conditions are metched then
		      ;; push the current THING (or the return
		      ;; of the test function, TFR) to the 
		      (when 
			  (and
			   ;; no tag-name given, or the correct tag-name
			   (or (not tag-name)
			       (and
				(eq (chpwalk:html-tag-name thing) tag-name)
				;; increment the number of times we've
				;; seen this tag-name
				(incf i-thing)))
			   ;; no N given, or the correct N
			   (or (not n) (= i-thing n))
			   ;; no ID given, or correct ID
			   (or (not id)
			       (equalp (chpwalk:get-keyval-for-html-tag
					:id thing)
				       id))
			   ;; no class given, or correct CLASS
			   (or (not class)
			       (equalp (chpwalk:get-keyval-for-html-tag
					:class thing)
				       class))
			   ;; no test-func, or test-func returns true
			   (or (not test-func)
			       tfr))
			;;
			(push
			 ;; if test-func returned a true non-T value
			 ;; then that is the return value
			 (if (eq tfr t)
			     thing
			     tfr)
			 result-list))
			;; terminate if returning only the first thing
		      (when (and result-list
				 (eq return :first))
			(return-from done-block))
		      ;; walk the contents
		      (recurser (chpwalk:html-tag-contents thing)))))))

	(recurser html-thing)))
    ;;
    (if (eq return :first)
	(car result-list)
	(nreverse result-list))))
