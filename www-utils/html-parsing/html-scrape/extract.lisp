;; tools for extracting content


(in-package html-scrape)

(defparameter *text-extraction-tags*
  '(:b :strong :i :em :mark :small :del :ins :a :style :span :div :q :blockquote :p
    :h1 :h2 :h3 :h4 :5 :h6 ))

(defun extract-text-from-parsed-html
    (html-thing &key (ignore-tags *text-extraction-tags*) (decode-html-entities t))
  "Extract the text within an HTML-TAG or string or list of these,
ignoring those tags in IGNORE-TAGS, which are taken to be tags that
represent only formatting, but returning NIL on other tags that might
represent content.

HTML-THING can be
   - an CL-HTML-PARSE-WALKER:HTML-TAG
   - a string
   - a list of strings and HTML-TAG

If DECODE-HTML-ENTITIES is T, then decode any HTML entities like &lt; in the text."
  (declare (type (or string list chpwalk:html-tag) html-thing))
  (let ((result-string
	  (block retblock
	    (with-output-to-string (sout)
	      (labels ((tag-to-text (thing)
			 (cond
			   ;; strings get spit out
			   ((stringp thing)
			    (format sout thing))
			   ;; it's a list, so we go through it
			   ((listp thing)
			    (dolist (sub-thing thing) (tag-to-text sub-thing)))
			   ;; tags we just ignore
			   ((or (eq (chpwalk:html-tag-name thing) t) ;; toplevel tag
				(member (chpwalk:html-tag-name thing) ignore-tags))
			    (dolist (sub-thing (chpwalk:html-tag-contents thing))
			      (tag-to-text sub-thing)))
			   ;; else not parsable so return
			   (t
			    (return-from retblock nil)))))
		
		(tag-to-text html-thing))))))
    (if (not result-string)
	nil
	(if decode-html-entities
	    (html-entities:decode-entities result-string)
	    result-string))))


(defun extract-value-from-text (text &key (date-convention nil))
  (declare (type (or null string) text)
	   (type (member NIL :mm-dd-yyyy :dd-mm-yyyy :yyyy-mm-dd
					 :mm-dd-yy :dd-mm-yy)
		 date-convention))
  (when text
    (let* ((stext (string-trim '(#\space #\tab) text))
	   (has-dbl-comma (search ",," stext))
	   ;; text without commas, for numbers with commas - but what about european?
	   (nctext (remove #\, stext))) 
      ;;
      (cond (has-dbl-comma  ;; can't be a valid integer or float
	     text)
	  (t
	   (or
	    (ignore-errors (parse-integer nctext))
	    (ignore-errors (parse-float:parse-float nctext))
	    (when date-convention
	      (ignore-errors
	       (jd-time-utils:parse-date-time-string
		nctext
		:date-convention date-convention)))
	    text))))))
	
    

(defun extract-value-from-parsed-html (html-thing
				       &key
					 (ignore-tags *text-extraction-tags*)
					 (decode-html-entities t)
					 (date-convention :yyyy-mm-dd))
    "Extract the value (number or string) within an HTML-TAG or string or list of these,
ignoring those tags in IGNORE-TAGS, which are taken to be tags that
represent only formatting, but returning NIL on other tags that might
represent content.

HTML-THING can be
   - an CL-HTML-PARSE-WALKER:HTML-TAG
   - a string
   - a list of strings and HTML-TAG

If DECODE-HTML-ENTITIES is T, then decode any HTML entities like &lt; in the text."
  (declare (type (or string list chpwalk:html-tag) html-thing))
  (declare (type (member NIL :mm-dd-yyyy :dd-mm-yyyy :yyyy-mm-dd
					 :mm-dd-yy)
		 date-convention))
  (extract-value-from-text
   (extract-text-from-parsed-html
    html-thing :ignore-tags ignore-tags
	       :decode-html-entities decode-html-entities)
   :date-convention date-convention))
   


(defun make-extractor-for-parsed-html (&key
					 (ignore-tags *text-extraction-tags*)
					 (decode-html-entities t)
					 (date-convention :yyyy-mm-dd))
  "Make a function to convert a block of parsed html in a table element to a
final form, like a float, integer, csv-read:date-time  (if date-convention is set)"
  (declare (type (member NIL :mm-dd-yyyy :dd-mm-yyyy :yyyy-mm-dd
					 :mm-dd-yy)
		 date-convention))
  (lambda (html-thing &key irow icol)
    (declare (ignore irow icol))
    (extract-value-from-parsed-html html-thing
				    :ignore-tags ignore-tags
				    :decode-html-entities decode-html-entities
				    :date-convention date-convention)))
