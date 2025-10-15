



(in-package run-program)
#|
   The RUN-PROGRAM routine runs a program, returing a program 
   structure with input, output, error streams


|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %get-path-variable ()
  (extensions:getenv "PATH"))


(defun %get-default-environment () (extensions:getenv-all))

(defun run-program (program args
		    &key
		      (environment (%get-default-environment))
		      (wait nil))

  "Run a program, the path of which is fully specified.

ARGS is a list of strings, representing arguments.
ENVIRONMENT is a list of conses like (FOO . BAR) or a string like FOO=BAR
WAIT is T if Lisp waits for program to finish.

Remember to run PROCESS-CLOSE at end, or streams will remain open."
  
  (flet ((fix-env-component (thing)
	   (cond ((consp thing)
		  thing)
		 ((stringp thing)
		  (let ((n (position #\= thing)))
		    (if (not n) 
			(cons thing "")
			(cons (subseq thing 0 n) (subseq thing (1+ n))))))
		 ((not thing)
		  nil)
		 (t
		  (error "Invalid item ~A in environment" thing)))))
  (system:run-program
   program args
   :environment (mapcar #'fix-env-component environment)
   :clear-environment t ;; because we pass whole env
   :wait wait)))

(defun process-alive-p (p) (system:process-alive-p p))
(defun process-close (p)
  (close (process-input p))
  (close (process-output p))
  (close (process-error p))
  ;; not clear if we should do kill
  (ignore-errors (process-kill p)))
(defun process-core-dumped (p) (declare (ignorable p)) nil) ;; DUMMY
(defun process-error (p)  (system:process-error p))
(defun process-exit-code (p)  (system:process-exit-code p))
(defun process-input (p)  (system:process-input p))
(defun process-output (p)  (system:process-output p))
(defun process-kill (p)  (system:process-kill p))
(defun process-p (p)  (system:process-p p))
  
