#|

Simple and robust argument passing

PROGRAM_NAME a1 a2 -x1 ax1 -x2 -x3 this that the-other

parses into

   '(("a1" "a2") ;; arguments BEFORE any keywords
     ;; keywords
     ((:x1 "ax1")
      (:x2)
      (:x3 "this" "that" "the-other")))

--foo is synonymous with -foo


|#

(defpackage simple-getopt
  (:use #:cl)
  (:export
   #:getopt
   #:get-argv))



(in-package simple-getopt)

;; a keyword is -xxxxxx  or --xxxxx where each x is an alphabetical character
(defun valid-keyword-p (arg)
  (declare (type string arg))
  ;; turn a double -- into a -
  (when (and (> (length arg) 1)
	     (char= (aref arg 0) #\-)
	     (char= (aref arg 1) #\-))
    (setf arg (subseq arg 1)))
  ;;
  (when
      (and (> (length arg) 1)
	   (char= (aref arg 0) #\-)
	   ;; keywords must begin with alpha char
	   (alpha-char-p (aref arg 1)))
    (let ((stripped (string-left-trim "-" arg)))
      (when (plusp (length stripped))
	(nth-value 0 (intern (string-upcase stripped) :keyword))))))


	   
       

(defun getopt (&optional (arg-list (get-argv)))
  (loop with un-named-args = nil
	with named-args = nil
	with saw-named = nil
	with current-named-arg = nil
	for arg in arg-list
	for vkp = (valid-keyword-p arg)

	do (cond (vkp
		  ;; save the current named arg
		  (when current-named-arg
		    (push (reverse current-named-arg) named-args)
		    (setf current-named-arg nil))
		  ;;
		  (setf saw-named t)
		  (push vkp current-named-arg))
		 ;;
		 (t ;; not a keyword
		  (cond ((not saw-named)
			 (push arg un-named-args))
			(t ;; already saw a named arg
			 (push arg current-named-arg)))))
	finally
	   (when current-named-arg
	     (push (reverse current-named-arg) named-args)
	     (setf current-named-arg nil))
	   (return (list (reverse un-named-args)
			 (reverse named-args)))))
			 
			 
	


(defun get-argv ()
  ;; Borrowed from command-line-arguments.  Temporary solution.
  ;; This is not PvE's code.
  #+sbcl (cdr sb-ext:*posix-argv*)
  #+clozure (cdr (ccl::command-line-arguments))
  #+gcl (cdr si:*command-args*)
  #+ecl (loop for i from 1 below (si:argc) collect (si:argv i))
  #+cmu (cdr extensions:*command-line-strings*)
  #+allegro (cdr (sys:command-line-arguments))
  #+lispworks (cdr sys:*line-arguments-list*)
  #+clisp ext:*args*
  #-(or sbcl clozure gcl ecl cmu allegro lispworks clisp)
  #+abcl extensions:*command-line-argument-list*
  (error "get-argv not supported for your implementation"))

