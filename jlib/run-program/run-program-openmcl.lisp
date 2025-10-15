


(in-package run-program)
#|
   The RUN-PROGRAM routine runs a program, returing a program 
   structure with input, output, error streams


|#

(defun %get-path-variable ()
  (ccl:getenv "PATH"))

(defun %get-default-environment () nil)

(defun run-program (program args
		    &key
		      (environment (%get-default-environment))
		      (input :stream)
		      (output :stream)
		      (error :stream)
		      (if-input-does-not-exist nil)
		      (if-output-exists :error)
		      (if-error-exists :error)
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
  (ccl:run-program
	  program args
	  :env  environment
	  ;;
	  :wait wait
	  :pty NIL
	  :input input
	  :output output 
	  :error error
	  :if-input-does-not-exist if-input-does-not-exist
	  :if-output-exists if-output-exists
	  :if-error-exists if-error-exists)))
  
  
(defun process-alive-p (p)
  (eq (ccl:external-process-status p) :running))
(defun process-close (p)
  (close (ccl:external-process-input-stream p))
  (close (ccl:external-process-output-stream p))
  (close (ccl:external-process-error-stream p))
  (ccl:signal-external-process p 3 :error-if-exited nil))
(defun process-core-dumped (p)  (declare (ignore p))  nil)
(defun process-exit-code (p)
  (multiple-value-bind (status code)
      (ccl:external-process-status p)
    (declare (ignorable status))
    code))
(defun process-input (p) (ccl:external-process-input-stream p))
(defun process-output (p) (ccl:external-process-output-stream p))
(defun process-error (p) (ccl:external-process-error-stream p))
(defun process-kill (p)
  (ccl:signal-external-process p 9 :error-if-exited nil))
(defun process-p (p)  (ccl::external-process-p p))

 
