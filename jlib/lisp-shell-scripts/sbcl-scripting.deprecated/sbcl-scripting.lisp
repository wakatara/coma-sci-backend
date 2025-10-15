
(defpackage sbcl-scripting
  (:use #:cl)
  (:export
   *script-name*
   *script-arguments*
   *print-usage-and-quit* ;; function that prints usage and quits
   get-args
   arg-is-present
   get-arg-values
   ;; special cases where keyword takes zero or 1 values
   get-zero-arg-or-error
   get-single-arg-or-error
   ))

(in-package sbcl-scripting)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *script-name*  (sb-posix:getenv "SCRIPTFILE"))
(defparameter *script-arguments* (cdr sb-ext:*posix-argv*))

;; a better function should be substituted by the calling script
(defparameter *print-usage-and-quit*
  (lambda  (&optional err-string)
    (when err-string (format *error-output* "~%~A~%~%" err-string))
    (format *error-output* "~%~% Program called incorrectly.")))
	    



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; given a command line "--key1 val1 arg2 arg2"
;; return  (values '((:key1 "val1" "val2" ..) ("arg1" "arg2" ..)))
;; and call PRINT-USAGE-AND-QUIT-FUNC when --HELP is an argument, or an
;; unknown KEY is seen
(defun get-args (allowed-args
		 &key
		   (print-usage-and-quit-func *print-usage-and-quit*)
		   (arguments *script-arguments*))
  "Parse command line arguments using SIMPLE-GETOPT:GETOPT
Allowed-args are the allowed keywords

Un-named arguments come first, and a keyword maybe have several 
values following it.  

PROGRAM_NAME a1 a2 -x1 ax1 -x2 -x3 this that

parses into

   '((\"a1\" \"a2\") ;; arguments BEFORE any keywords
     ;; keywords
     ((:x1 \"ax1\")
      (:x2)
      (:x3 \"this\" \"that\")))


"
  (let* ((parsed-args (simple-getopt:getopt arguments))
	 (un-named-args (first parsed-args))
	 (named-args (second parsed-args))
	 (named-args
	   (loop for alist in named-args
		 for key = (car alist) 
		 until  (not alist)
		
		 if (eq key :help)
			do
			   (funcall print-usage-and-quit-func)
		 else if (not (find key allowed-args))
			do
			   (funcall print-usage-and-quit-func
			      (format nil "ERROR: Unknown argument ~A" key))
		 else
		   collect alist)))

    
    (loop for arg in un-named-args
	  when (and (>= (length arg) 2)
		    (eql (aref arg 0) #\-)
		    (eql (aref arg 1) #\-))
	    do (funcall print-usage-and-quit-func
			(format nil "ERROR: Un-named argument ~S cannot start with --~%Named arguments must come before un-named arguments"
				arg)))
    (values named-args un-named-args)))


(defun arg-is-present (keyword named-args)
  "Return the full list (:KEYWORD VAL1 VAL2) if an arg is present."
  (assoc keyword  named-args))

;; get a list of values of keyword
(defun get-arg-values (keyword named-args)
  "Given PARSED-ARGS from GET-ARGS, return a list of the values of KEYWORD."
  (cdr (assoc keyword named-args)))

;; get an single arg, or throw error
(defun get-single-arg-or-error
    (keyword named-args
     &key
     (print-usage-and-quit-func *print-usage-and-quit*))
      "Get the argument for a keyword that takes exactly 1 arg, or 
PRINT-USAGE-AND-QUIT"
  (let ((arglist (get-arg-values keyword named-args)))
    (if (= (length arglist) 1)
	(first arglist)
	(funcall print-usage-and-quit-func
		 (format
		  nil
		  "ERROR: Argument ~A should have 1 value but has ~D values ~{~A ~}"
		  keyword
		  (length arglist)
		  arglist
		  )))))

(defun get-zero-arg-or-error
    (keyword named-args
     &key
       (print-usage-and-quit-func *print-usage-and-quit*))
      "Get a argument for a keyword that takes exactly 0 arg, or 
PRINT-USAGE-AND-QUIT"
  (let ((arglist (assoc keyword named-args)))
    (cond
      ((not arglist) ;; argument not present
       nil)
      ((= (length arglist) 1)  ;; argument present
       t)
      (t  ;; more than zero values for argument
       (funcall print-usage-and-quit-func
		(format
		 nil
		  "ERROR: Argument ~A should have 0 values but has ~{~A ~}"
		  keyword
		  arglist))))))
