#|
This provides two main functions to help walk the output of html-parse

1. WALK-HTML, which calls a user-given function
    (lambda (&key string tag-name tag-keywords tag-contents after-contents) ..)   
   whenever it runs into an html element (tag, or list).
  
   This function must return non-NIL if the elements inside a tag are to
   be walked.

   Here
    STRING is if it contains a string element
    TAG-NAME if it has tag
      TAG-KEYWORDS if the tag has keywords like (:contents "foo" :style "mystyle")
      TAG-CONTENTS an html-parse representation of the elements inside
         this tag
      AFTER-CONTENTS is given as T to provide for finalization after TAG-CONTENTS
        are walked.

2. CONVERT-HTML-TO-HTML-TAGS, which converts an HTML list to an eaiser to 
   traverse nested list of structures like
     #S(HTML-TAG  
              :NAME :XX  
              :KEYWORDS (:contents "foo" :style "mystyle")
              :CONTENTS ( ... list of strings and #S(HTML-TAG .. )))
 
   where the outermost structure is a dummy with NAME=T


|#

(defpackage cl-html-parse-walker
  (:use #:cl)
  (:nicknames #:chpwalk) 
  (:export
   #:walk-html
   #:get-keyval
   #:get-keyval-for-html-tag
   ;;
   #:html-tag #:html-tag-p #:html-tag-name #:html-tag-keywords
   #:html-tag-contents
   #:convert-html-list-to-html-tags
   ))

(in-package cl-html-parse-walker)


;; the tags into which html is parsed. HTML turns into a list of these
;; tags and of strings, which have further sublists of tags inside
;; HTML-TAG-CONTENTS
(defstruct html-tag
  (name nil)
  (keywords nil)
  (contents nil))
  

(defun get-keyval (keyword tag-keywords)
  "Given a list like lst=(:a foo :b bar) 
   (get-keyval :a lst) returns foo"
  (loop for lst = tag-keywords then (cdr lst)
	until (not lst)
	when (eq (car lst) keyword)
	  do (return (cadr lst))))

(defun get-keyval-for-html-tag (keyword html-tag)
  "Runs GET-KEYVAL on the TAG-KEYWORDS of HTML-TAG"
  (declare (type html-tag html-tag))
  (get-keyval keyword (html-tag-keywords html-tag)))


(defun walk-html (html-list func &key (max-recursion-depth 1000))
  "Given an html list produced by cl-html-parse:parse-html and a function
   taking keyword arguments as

   (func (&key string tag-name tag-keywords tag-contents after-contents) ..)
   
   so that

      (FUNC :STRING STRING               ;; if it is a string
            :TAG-NAME    TAGSYMBOL       ;; eg :DIV
            :TAG-KEYWORDS  TAG-KEYWORDS  ;; eg  '(:CONTENTS \"something\")
            :TAG-CONTENTS  TAG-CONTENTS  ;; tags and strings inside this tag
            :AFTER-CONTENTS NIL/T)       ;; finalization after iterating contents 
   where all arguments default to NIL,
   walk the HTML-LIST and call the function on each tag and contents.
  
   If FUNC returns non-NIL, then the list inside TAG-CONTENTS is walked 
   recursively too."

  (let ((i-depth 0))
    (labels ((walk-html-list (hlist)
	       (dolist (item hlist)
		 (cond
		   ;; tag is just a string
		   ((stringp item)
		    (handle-string item))
		   ;; tag is just <br>=:br
		   ((keywordp item)
		    (handle-html-tag item nil nil))
		   ;; tag is <div>content</div>=(:div "content")
		   ((and (listp item)
			 (keywordp (first item)))
		    (handle-html-tag (first item)
				     nil ;; no contents
				     (cdr item)))
		   ;; tag is <A HREF="http://foo.com"/>blah</A> =
		   ;;   =  ((:a :href "http://foo.com") blah)
		   ((and (listp item)
			 (listp (car item))
			 (keywordp (caar item)))
		    (handle-html-tag (caar item)
				     (cdar item)
				     (cdr item))))))
	     (handle-string (string)
	       (funcall func :string string))
	     ;;
	     (handle-html-tag (tag-name tag-key-vals tag-contents)
	       (when (funcall func
			      :tag-name tag-name
			      :tag-keywords tag-key-vals
			      :tag-contents tag-contents)
		 (when (= (incf i-depth) max-recursion-depth)
		   (error "Excessively deep recursion (> ~A) in walk-html." max-recursion-depth))
		 (walk-html-list tag-contents)
		 (decf i-depth)
		 (funcall func :after-contents t) )))
      ;;
      (walk-html-list html-list))))

  


(defun convert-html-list-to-html-tags (html-list)
  "Convert HTML-LIST to a nested list of HTML-TAG structures, the outermost
  one being a dummy structure with NAME=T"
  (let* ((outer-tag (make-html-tag :name T)) ;; an outermost container
	 (tags (list outer-tag)))
    
    (walk-html
     html-list
     (lambda (&key string tag-name tag-keywords tag-contents after-contents)
       (declare (ignore tag-contents))
       (cond (string
	       (push string (html-tag-contents (first tags))))
	     ;#+nil
	      (tag-name
	       (let ((htag
		       (make-html-tag
			:name tag-name
			:keywords tag-keywords)))
		 ;; add this tag to parent tag
		 (push htag (html-tag-contents (first tags)))
		 ;; now this tag is the parent tag at head of tags
		 (push htag tags)
		 t)) ;; continue parsing
	      (after-contents
	       ;; reverse the inside
	       (setf  (html-tag-contents (first tags))
		      (nreverse (html-tag-contents (first tags))))
	       ;; and pop this level to restore the old parent
	       (pop tags)))))
    ;;
    ;; reversal of outermost
    (setf (html-tag-contents outer-tag)
	  (nreverse
	   (html-tag-contents outer-tag)))
    ;;
    outer-tag))
		   

