
(in-package jk-datadir)



(defparameter *default-permission* :group
  "Default write permission for user directories, :ALL :GROUP or :USER")

(defparameter *lisp-library-directory*
  (or (uiop:getenv "LISP_LIB")
      (error "Cannot get environment variable LISP_LIB in JK-DATADIR package")))

;; with trailing /
(defparameter *default-datadir-pathname*
  (pathname
   (concatenate
    'string
    (string-right-trim "/"
		       (or (uiop:getenv "LISP_LIB_DATADIR")
			   (format nil "~A/~A"
				   (string-right-trim
				    "/" *lisp-library-directory*)
				   "SYSTEM-DATA")))
    "/")))


(when (not (eq :absolute
	       (first (pathname-directory *default-datadir-pathname*))))
  (error "JK-DATADIR: DATADIR ~A is not absolute path"
	 *default-datadir-pathname*))


(defun %set-directory-write-permission (dir permission)
  (let ((posix-code (cond ((eq permission :all) #o777)
			  ((eq permission :group) #o770)
			  ((eq permission :user) #o700))))
  #+sbcl (sb-posix:chmod dir posix-code)))


(defgeneric get-datadir-for-system (sys &key create permission)
  (:documentation
     "Get the data directory for a SYSTEM, for system specific data for this system.
By default it is in LISP_LIB/SYSTEM-DATA/<package-name> but
it can be overriden by $LISP_LIB_DATADIR.

Permission is the write permission, either :GROUP, :ALL, or :USER.
*DEFAULT-PERMISSION* :GROUP but this variable can be set otherwse."))


(defmethod get-datadir-for-system ((sys asdf:system)
				   &key
				     (create t)
				     (permission *default-permission*))

  (declare (type (member :all :group :user)  permission))
  (let* ((name (asdf:component-name sys))
	 (lib-dirs
	   (pathname-directory *default-datadir-pathname*))
	 (data-dir
	   (make-pathname
	    :directory (append lib-dirs (list name)))))
    ;;
    (when create
      (when (not (cl-fad:directory-exists-p data-dir))
	(ensure-directories-exist
	 (merge-pathnames
	  data-dir
	  (make-pathname :name "IGNORE")))
	(%set-directory-write-permission data-dir permission)))
    ;;
    data-dir))
  
(defmethod get-datadir-for-system ((sys string)
				   &key
				     (create t)
				     (permission *default-permission*))
  (get-datadir-for-system
   (or (asdf:find-system sys)
       (error "ASFD system ~A not found." sys))
    :create create :permission permission))

