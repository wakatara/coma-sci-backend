;; general logging facility
;;
;;  The generic method is (WRITELOG LOGGER STRING :LOG-TYPE ... :LOG-LEVEL ...)
;;  where LOGGER is NIL,T, a stream, or a FILE-LOGGER object
;;  and LOG-TYPE can be :LOG or :ERROR and :LOG-LEVEL is an integer from 0 (low)
;;  to 5 (high).
;;
;;  Most LOGGER objects ignore LOG-TYPE and LOG-LEVEL, but user-generated
;;  loggers can use them.
;;
;;
;;  A FILE-LOGGER is made using
;;    (MAKE-FILE-LOGGER OUTFILE :IF-EXISTS ... :TIMESTAMP NIL/T )
;;
;;
;;



(defpackage logger
  (:use  #:cl)
  (:export
   #:writelog
   #:writelog-and-error
   #:*default-logger*
   ;; class that logs to one file
   #:file-logger 
   #:make-file-logger
   ;; class that logs to two files (LOG and ERROR)
   #:dual-file-logger
   #:make-dual-file-logger
   #:broadcast-logger
   #:make-broadcast-logger
   ))

(in-package logger)

;; can rebind *default-logger* to enable global logging
(defparameter *default-logger* nil)

(defgeneric writelog (logger log-string &key log-type log-level)
  (:documentation "Log output to a LOGGER which may be a one of
 LOGGER = NIL    - no logging
 LOGGER = T      - standard output or error, depending on LOG-TYPE
 LOGGER = stream - log to stream
 LOGGER = FILE-LOGGER (object) - log to a FILE-LOG object, which contains an 
          output file
 LOGGER = DUAL-FILE-LOGGER (object) - log to separate :LOG and :ERROR files
 ... other types and methods defined by user ...

 LOG-TYPE should be :LOG or :ERROR, but is ignored for stream-based loggers
 LOG-LEVEL should be an integer [0..5] but is ignored for stream-based loggers
"))

;; no logging
(defmethod writelog ((logger (eql nil)) log-string
		     &key log-type log-level)
  (declare (ignore logger log-string log-type log-level))
  t)
  

(defmethod writelog ((logger (eql t)) log-string
		     &key log-type log-level)
  (cond ((eq log-type :error)
	 (writelog *error-output* log-string
		   :log-type log-type :log-level log-level))
	(t
	 (writelog *standard-output* log-string
		   :log-type log-type :log-level log-level))))


(defmethod writelog ((logger stream)
		     log-string
		     &key log-type log-level)
  (declare (ignore log-type log-level))
  (ignore-errors
   (write-string log-string logger)
   (terpri logger)
   (force-output logger)))


(defclass logger () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a logger that broadcasts to a list of subloggers
(defclass broadcast-logger (logger)
  ((logger-list :initarg :logger-list :initform nil
		:accessor broadcast-logger-logger-list)))

(defmethod writelog ((b-logger broadcast-logger)
		     log-string
		     &key log-type log-level)
  (loop for logger in (broadcast-logger-logger-list b-logger)
	do (writelog logger log-string
		     :log-type log-type
		     :log-level log-level)))

(defun make-broadcast-logger (&rest logger-list)
  "Make a BROADCAST-LOGGER from a list of loggers.
Anything sent to the BROADCAST-LOGGER is sent to each 
logger in the list."
  (make-instance 'broadcast-logger
		 :logger-list logger-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; one output file logger
(defclass file-logger (logger)
  ((outfile :initarg :outfile :initform nil :accessor file-logger-outfile)
   ;; NAME is not used for much
   (name    :initarg :name    :initform nil :accessor  file-logger-name) 
   (timestamp :initarg :timestamp :initform t :accessor file-logger-timestamp)
   (log-level :initarg :log-level :initform 0 :accessor log-level)
   (lock    :initarg :lock
	    :initform (bordeaux-threads:make-recursive-lock "file-logger-lock")
	    :accessor file-logger-lock)))

(defun make-timestamp (&optional (ut (get-universal-time)))
  "Convert Lisp integer UT to a date string, assuming Timezone=0"
   (multiple-value-bind (sec min hour day month year)
       (decode-universal-time ut 0)
     (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
             year month day hour min sec)))
   



(defun make-file-logger (outfile &key (name nil) (timestamp t) (if-exists :append)
				 (log-level 0))
  (declare (type (member :append :supersede :error) if-exists)
	   (type (integer 0 5) log-level))
  ;; do a test write
  (with-open-file (sout outfile :direction :output :if-exists if-exists
				:if-does-not-exist :create)
    (format sout "===== Starting log~Aat ~A ======~%"
	    (if name (format nil " ~A " name) " ")
 	    (make-timestamp)))
  ;;
  (make-instance 'file-logger :outfile outfile :name name
			      :log-level log-level
			      :timestamp timestamp))

(defmethod writelog ((logger file-logger)  log-string
		     &key log-type (log-level 5))
  (declare (ignore log-type))
  (ignore-errors
   (when (>= log-level (log-level logger))
     (bordeaux-threads:with-lock-held ((file-logger-lock logger))
       (with-open-file (sout (file-logger-outfile logger)
			     :direction :output :if-exists :append
			     :if-does-not-exist :create)
	 (when (file-logger-timestamp logger)
	   (write-string (make-timestamp) sout)
	   (write-string " - " sout))
	 (write-string log-string sout)
	 (terpri sout)
	 (force-output sout))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dual output file logger for LOG-TYPE = LOG and ERROR
(defclass dual-file-logger (logger)
  ((log-outfile :initarg :log-outfile :initform nil
		:accessor file-logger-log-outfile)
   (error-outfile :initarg :error-outfile :initform nil
		:accessor file-logger-error-outfile)
   ;; NAME is not used for much
   (name    :initarg :name    :initform nil :accessor  file-logger-name) 
   (timestamp :initarg :timestamp :initform t :accessor file-logger-timestamp)
   (log-level :initarg :log-level :initform 0 :accessor log-level)
   (error-log-level :initarg :error-log-level :initform 0 :accessor error-log-level)
   (lock    :initarg :lock
	    :initform (bordeaux-threads:make-recursive-lock "file-logger-lock")
	    :accessor file-logger-lock)))
     
    
(defun make-dual-file-logger (log-outfile error-outfile
			      &key (name nil) (timestamp t)
				(log-level 0)
				(error-log-level 0)
				(if-exists :append))
  (declare (type (member :append :supersede :error) if-exists)
	   (type (integer 0 5) log-level error-log-level))
  ;; do a test write
  (with-open-file (sout log-outfile :direction :output :if-exists if-exists
				:if-does-not-exist :create)
    (format sout "===== Starting LOG log~Aat ~A ======~%"
	    (if name (format nil " ~A " name) " ")
 	    (make-timestamp)))
  
   (with-open-file (sout error-outfile :direction :output :if-exists if-exists
				:if-does-not-exist :create)
    (format sout "===== Starting ERROR log~Aat ~A ======~%"
	    (if name (format nil " ~A " name) " ")
 	    (make-timestamp)))
  
  ;;
  (make-instance 'dual-file-logger
		 :log-outfile log-outfile
		 :error-outfile error-outfile
		 :log-level log-level
		 :error-log-level error-log-level
		 :name name :timestamp timestamp))


(defmethod writelog ((logger dual-file-logger)  log-string
		     &key (log-type :log) (log-level 5))
  (ignore-errors
   (bordeaux-threads:with-lock-held ((file-logger-lock logger))
     (flet ((write-to-log (file level)
	      (when (>= log-level level)
		(with-open-file (sout file
				      :direction :output :if-exists :append
				      :if-does-not-exist :create)
		  (when (file-logger-timestamp logger)
		    (write-string (make-timestamp) sout)
		    (write-string " - " sout))
		  (write-string log-string sout)
		  (terpri sout)
		  (force-output sout)))))
       ;;
       (cond ((eq log-type :error)
	      (write-to-log (file-logger-error-outfile logger)
			    (error-log-level logger)))
	     (t ;; anything that's not an error goes to :LOG
	      (write-to-log (file-logger-log-outfile logger)
			    (log-level logger))))))))
	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; writelog and throw an error
(defgeneric writelog-and-error (logger log-string &key log-type log-level)
  (:documentation "WRITELOG, then throw an error."))


(defmethod writelog-and-error ((logger t)  log-string
			       &key (log-type :error) (log-level 5))
  (writelog logger log-string :log-type log-type :log-level log-level)
  (error log-string))
