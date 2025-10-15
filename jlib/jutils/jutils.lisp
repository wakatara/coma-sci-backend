; utility routines for my library

(defpackage #:jutils
  (:use #:common-lisp)
  (:export
   #:getenv
   #:*library-file-search-path-list*
   #:add-object-path
   #:find-object-file
   #:load-foreign-object-file
   #:load-foreign-shared-library
   ;;
   #:with-directory/chdir
   #:with-directory 
   #:getcwd
   #:chdir
   ;;
   #:run-program
   #:process-p
   #:process-alive-p      #:process-close
   #:process-core-dumped  #:process-error
   #:process-exit-code    #:process-input
   #:process-kill         #:process-output
   ;;
   )
  )

(in-package jutils)

(defun getenv (var-string)
  #+openmcl (ccl::getenv var-string)
  #+sbcl
  (sb-ext:posix-getenv var-string)
  #+cmu
  (loop
   with answer = nil
   with searchstring = (concatenate 'string var-string "=")
   with n = (length searchstring)
   for thing in common-lisp::lisp-environment-list
   when (and (> (length thing) n)
	     (string=  searchstring thing :end2 n))
   do (setf answer (subseq thing n))
   finally (return answer)))



(defparameter *original-library-file-search-path-list* '(""))
;  (append '("/usr/lib" "/usr/local/lib") (getenv "LD_LIBRARY_PATH")))

(defparameter *library-file-search-path-list*
  (copy-list *original-library-file-search-path-list*))

(defun add-object-path (path-string)
  "add a path to the search path for dynamic libraries *library-file-path*"
  (when (not (member path-string  *library-file-search-path-list*
		     :test #'equal))
    (setf *library-file-search-path-list*
	  (append *library-file-search-path-list* (list path-string)))))

(defun find-object-file (file-string &key
			  (search-path-list *library-file-search-path-list*))
  "find the first library in path list (default is
*library-file-search-path-list*) that matches file-string -
for file in current dir, use leading dot as in ./foo.o"  
  (loop
   initially
   ;; if using leading dot notation "./foo.o" test for a file in this dir
   (when (and (> (length file-string) 2)
	      (string= "./" file-string :end2 2)
	      (probe-file file-string))
     (return file-string))
   for path in search-path-list
   for n = (length path)
   ;; do we need a slash?
   for slash = (if (and (plusp n) (not (char= (aref path (1- n)) #\/)))
		   "/" "")
   for fullpath = (concatenate 'string path slash file-string)
   when (probe-file fullpath)
   do (return fullpath)
   finally (return nil)))
			

(defun load-foreign-object-file
    (object-file
     &key (libraries '("-lc"))
     (search-path-list *library-file-search-path-list*))
  "Load a foreign object file in search-path-list
 (*library-file-search-path-list* by default).  libraries is a list of
UNIX LD format libraries"
  (let ((lib-file (find-object-file object-file)))
    ;;
    (when (not lib-file)
      (error
       "error in load-foreign-object-code - could not find ~A in path list ~A"
       object-file search-path-list))
    ;;
    #+(or sbcl cmu)
    (#+sbcl sb-alien:load-foreign
     #+cmu  alien:load-foreign
            (list lib-file)
            :libraries libraries)
    lib-file))


(defun load-foreign-shared-library (lib &key (search-path-list *library-file-search-path-list*))
  (let ((lib-file (find-object-file lib)))
      ;;
    (when (not lib-file)
      (error
       "error in load-foreign-shared-library - could not find ~A in path list ~A"
       lib search-path-list))
    #+sbcl (sb-alien:load-1-foreign lib-file)
    #+cmu (error "I don't know how to load-foreign-shared-library")
    lib-file))



(defmacro with-directory (dir &rest forms)
  "run FORMS after temporarily changing into a new directory -
does not run chdir, so it is thread-safe, but doesn't help
with unix file access or unix process spawning"
  (let ((dirvar (gensym "with-directory-dir"))
	(thedir (gensym "dir")))
    `(let* ((,thedir ,dir)
	    (,dirvar
	     (cond ((stringp ,thedir)
		    (%make-pathname-for-stringdir ,thedir))
		   ((pathnamep ,thedir)
		    ,dir)
		   (t
		    (error "dir ~A not a string or pathname" ,dir)))))
       (let ((cl:*default-pathname-defaults* ,dirvar))
	 ,@forms))))

  

#+cmu  
(defmacro with-directory/chdir (dir &rest forms)   
  "run FORMS after temporarily changing into a new directory - note
that this could be VERY DANGEROUS in multithreaded applications, and
that if using Lisp-only file accesses one should temporarily bind
cl:*default-pathname-defaults* to the desired directory instead - this
is what with-directory does"
  (let ((dirvar (gensym)))
    `(let ((,dirvar (ext:default-directory)))
       (unwind-protect
	   (progn
	     (setf (ext:default-directory) ,dir)
	     ,@forms)
	 (setf (ext:default-directory) ,dirvar))))) 
      

#+sbcl ;; why is this not in sb-unix package?
(sb-alien:define-alien-routine ("chdir" unix-chdir) sb-alien:int
  (dir-name sb-alien:c-string))

#+sbcl
(defmacro with-directory/chdir (dir &rest forms)   
  "run FORMS after temporarily changing into a new directory - note
that this could be VERY DANGEROUS in multithreaded applications, and
that if using Lisp-only file accesses one should temporarily bind
cl:*default-pathname-defaults* to the desired directory instead - this
is what with-directory does"
  (let ((cwdvar (gensym))
	(dirvar (gensym)))
    `(let ((,cwdvar (sb-unix:posix-getcwd))
	   (,dirvar ,dir))
       (with-directory ,dirvar ;; put in with-directory to handle lisp part
	 (unwind-protect 
		(progn
		  (unix-chdir ,dirvar) 
		  ,@forms)
	   (chdir ,cwdvar))))))

 
;; helper function for with-directory
(defun %make-pathname-for-stringdir (str)
  (declare (type string str))
  ;; is it an absolute path (starts with / ?)
  (cond ((and (> (length str) 0)
	      (char= (aref str 0) #\/))
	 (make-pathname :directory `(:absolute ,str)))
	(t
	 (make-pathname 
	  :directory 
	  `(:absolute 
	    ,(concatenate 'string 
			  (string-trim "/" (getcwd))
			  "/" 
			  (string-right-trim "/" str)))))))
	 



(defun getcwd ()
  "get the current directory - note that this is the Wrong Thing
in Lisp, as there is no such thing as the current directory, and
one is supposed to use cl:*default-pathname-defaults* as the base for
filename accesses"
  #+sbcl
  (sb-unix:posix-getcwd)
  #+cmu
  (ext:default-directory))


(defun chdir (dir)
  "chage default directory to dir - note that this is the Wrong Thing
in Lisp, as there is no such thing as the current directory, and
one is supposed to use cl:*default-pathname-defaults* as the base for
filename accesses"
  (let ((unix-path (cond ((stringp dir)
			  dir)
			 ((pathnamep dir)
			  (namestring dir))
			 (t
			  (error
			   "dir in chdir should be a string or pathname")))))
    #+sbcl
    (progn
      (when (not (zerop (unix-chdir unix-path)))
	(error "Error changing to directory ~A~%" dir))
      (setf *default-pathname-defaults*
	    (make-pathname :directory dir)))
    #+cmu
    (setf (ext:default-directory) unix-path)
    dir))

#+(or sbcl cmu)
(defun run-program (program args &key
		    #+sbcl (environment
			    (sb-ext:posix-environ))
		    #+cmu (environment common-lisp::lisp-environment-list)
		    (wait t) 
		    #+sbcl search
		    pty input if-input-does-not-exist
		    output (error output) (if-output-exists error) 
		    (if-error-exists error) status-hook)
  "a simple alias for sbcl/cmucl run-program - note that the SBCL inherits
the unix envornment by default, and the CMUCL behavior has been modified
to reflect this SBCL behavior - the :ENV argument has been abandoned in
favor the :ENVIRONMENT argument.

also, the CMUCL version does not support the :SEARCH keyword.

WARNING: the current working directory for the unix process is
basically a mystery.   All paths used by Unix programs should be fully
specified relative to root."
  (#+sbcl sb-ext:run-program
	  #+cmu  ext:run-program
	  ;;
	  program args
	  ;; in cmucl, substitute environment for ENV
	  #+sbcl :environment #+sbcl environment 
	  #+cmu :env #+cmu environment
	  ;;
	  :wait wait
	  #+sbcl :search #+sbcl search
	  :pty pty :input input
	  :if-input-does-not-exist if-input-does-not-exist
	  :output output :if-output-exists if-output-exists
	  :error error
	  :if-error-exists if-error-exists
	  :status-hook status-hook))

;; import process accessors - setf expanders don't work, but
;; we never use them
#+(or sbcl cmu)
(progn
(defun process-alive-p (p)
  (#+sbcl sb-impl::process-alive-p #+cmu ext:process-alive-p p))
(defun process-close (p)
  (#+sbcl sb-impl::process-close #+cmu ext:process-close p))
(defun process-core-dumped (p)
  (#+sbcl sb-impl::process-core-dumped #+cmu ext:process-core-dumped p))
(defun process-error (p)
  (#+sbcl sb-impl::process-error #+cmu ext:process-error p))
(defun process-exit-code (p)
  (#+sbcl sb-impl::process-exit-code #+cmu ext:process-exit-code p))
(defun process-input (p)
  (#+sbcl sb-impl::process-input #+cmu ext:process-input p))
(defun process-output (p)
  (#+sbcl sb-impl::process-output #+cmu ext:process-output p))
(defun process-kill (p signal)
  (#+sbcl sb-impl::process-kill #+cmu ext:process-kill p signal))
(defun process-p (p)
  (#+sbcl sb-impl::process-p #+cmu ext:process-p p))
)

