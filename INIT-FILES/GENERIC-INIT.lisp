
#|

Generic initialization file that is currently good for SBCL and CCL

Loads astro-lisp-init.lisp if environment variable ASTROLISP is set

|#


;; load this first - it gives is UIOP/OS:GETENV
#-clisp
(require 'asdf) ;; system definition package - ECL has its own

#+clisp ;; CLISP can't REQUIRE asdf on its own
(or (ignore-errors (require 'asdf)) ;; if compiled
    (load (concatenate
	   'string
	   (or (getenv "LISP_LIB")
	       (error "LISP_LIB env var not found - needed first thing by CLISP"))
	   "/asdf/build/asdf.lisp")))

#+clisp (setf *print-circle* t) ;; prevent blow-ups
		  

#+ecl(asdf:register-immutable-system "asdf")  ; Prevents ASDF from reloading

(defparameter *required-environment-variables*
  '("LISP_LIB")) 

#+sbcl
(progn
  (require 'sb-posix)
  (require 'sb-introspect))

(loop for var in *required-environment-variables*
      when (not (uiop/os:getenv var))
	collect var into bad-vars
      finally
	 (when bad-vars
	   (error "Environment variables ~A not defined" bad-vars)))

(setf *DEBUG-BEGINNER-HELP-P* nil)
(setf *print-case* :downcase)

;; for CCL/OPEMCL running on ARM CPU, force it to run on a single core
;; to try to mitigate the threading corruption that hasn't been fixed for years
#+(and ccl arm linux)
(run-program "sudo" `("taskset" "-pc" "0" ,(format nil "~D" (ccl::getpid))))



(defparameter *lisp-library-directory*  (uiop/os:getenv "LISP_LIB"))
;;
(when 
  (not
   #-clisp (probe-file *lisp-library-directory*)
   #+clisp (ext:probe-directory ;; clisp probe file doesn't do dirs
	    (concatenate 'string (string-right-trim
				  "/" *lisp-library-directory*)
			 "/")))
  (error "Software dir ~A does not exist"  *lisp-library-directory*))



(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,*lisp-library-directory*)
   (:exclude "Junk")
   :ignore-inherited-configuration))



;; set up asdf package home
(push (concatenate 'string *lisp-library-directory*  "modules/")
      asdf:*central-registry*)
(push *default-pathname-defaults* asdf:*central-registry*)
(require 'jutils) ;; our extra utils for cmucl/sbcl support
;; turn off infix banner at startup
(setf (get :infix :dont-print-copyright) t) 
(require 'infix)


#+sbcl
(defmethod asdf:perform :around ((o asdf:load-op)
				 (c asdf:cl-source-file))
  (handler-case (call-next-method o c)
    ;; If a fasl was stale, try to recompile and load (once).
    (sb-ext:invalid-fasl ()
      (asdf:perform (make-instance 'asdf:compile-op) c)
      (call-next-method))))	   


;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (concatenate 'string *lisp-library-directory* "/setup.lisp")))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))


;; figure out if we are on raspberry pi
#+(or (and sbcl arm arm-vfpv2 linux)
      (and ccl 32-bit-host 32-bit-target linux :linuxarm-host :linuxarm-target))
(pushnew :linux-raspberry-pi-arm32 *features*)


(require 'cffi)
;; add path for .so and .dylib files
(defparameter *dynamic-lib-architecture*
  (or #+(and darwin x86-64) "OSX-x86-64"
      #+(and arm64 darwin)  "OSX-arm64"
      #+(and linux x86-64) "linux-x86-64"
      #+(or (and linux-raspberry-pi-arm32)
	    (and arm 32-bit linux))
      "linux-raspberry-pi-arm32"
      #+(or (and linux-raspberry-pi-arm64)
	    (and arm 64-bit linux)
	    (and arm64 linux))
      "linux-raspberry-pi-arm64"
      (error "Architecture is not x86-64/arm64 Darwin or Linux-x86-64 or linux-raspberry-pi-32 or linux-raspberry-pi-64")))

(defparameter *dynamic-library-directory*  ;; must have trailing /
  (format nil "~A/dynamic-libraries/~A/"
	  (string-right-trim "/" *lisp-library-directory*)
	  *dynamic-lib-architecture* ))
(push  *dynamic-library-directory* cffi:*foreign-library-directories*)

;; add common library dirs
#+darwin
(push "/opt/local/lib/" cffi:*foreign-library-directories*)
#+linux
(push "/usr/local/lib/" cffi:*foreign-library-directories*)


(require 'pconfig) ;; set our our pre-loading configarator
;; load astrolisp config
(when (uiop/os:getenv "ASTROLISP")
  (load (format nil "~A/INIT-FILES/astro-lisp-init.lisp" *lisp-library-directory*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up lparallel to use NCPU/2 kernels
#+(or thread-support) ;; sbcl or ccl
(progn
  (require 'sb-cltl2) ;; lparallel fails without this, strangely
  (asdf:load-system "lparallel"))
  
#+(or thread-support) ;; sbcl or ccl
(progn
  (defparameter *ncpu* (or 
			(ignore-errors (parse-integer (uiop/os:getenv "NCPU")))
			4)) ;; most machines are 4 CPU
  (setf lparallel:*kernel* (lparallel:make-kernel (ash *ncpu* -1))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THE FOLLOWING IS NO LONGER NEEDED, AND IT CAUSES VERY SLOW
;; PROGRAM UNLOADING ON APPLE SILICON
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; try to load an older SSL (before v3) for Darwin because the new one
;; ;; messes up JPL Horizons, because it turns off unsafe renegotiation
;; ;; and the JPL public facing server is not updated
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;(asdf:load-system "cl+ssl/config")
;; ;; (and darwin bongobongo)
;; ;; this must be a macro because the underlying routines in
;; ;; cl+ssl/config are macros 
;; (macrolet
;;     ((set-the-ssl-default-libs ()
;;        ;; macro generates the code for the right library
;;        ;; with the library name hardwired in
;;        (let ((options '(("/opt/local/lib/openssl-1.1/libssl.dylib"
;; 			 "/opt/local/lib/openssl-1.1/libcrypto.dylib")
;; 			("/usr/local/lib/libssl.1.1.dylib"
;; 			 "/opt/local/lib/libcrypto.1.1.dylib")
;; 			("/opt/local/lib/openssl-1.0/libssl.dylib"
;; 			 "/opt/local/lib/openssl-1.0/libcrypto.dylib")
;; 			("/usr/local/lib/libssl.1.0.dylib"
;; 			 "/opt/local/lib/libcrypto.1.1.dylib"))))
;; 	 (loop for (libssl libcrypto) in options
;; 	       when (ignore-errors (and (probe-file libssl)
;; 					(probe-file libcrypto)))
;; 		 do
;; 		    (return
;; 		       `(progn
;; 			  (cl+ssl/config:define-libssl-path ,libssl)
;; 			  (cl+ssl/config:define-libcrypto-path ,libcrypto)))))))
;;   ;; and run the macro we just made
;;   (set-the-ssl-default-libs))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
