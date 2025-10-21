;;;; sbclrc.lisp
;;;;
;;;; SBCL initialization file for Docker containerization
;;;; This file sets up the Lisp environment before loading any packages
;;;;
;;;; Usage: sbcl --userinit sbclrc.lisp
;;;;
;;;; This replaces the manual ASDF registry pushnew operations that were
;;;; previously in coma-sci-backend.asd, as suggested by Jan Kleyna.

(in-package :cl-user)

;; Load ASDF first - this gives us UIOP/OS:GETENV
(require 'asdf)

(format t "~%Loading COMA Science Backend SBCL initialization...~%")

;; Get LISP_LIB from environment variable (following Jan's approach)
(defparameter *lisp-library-directory*
  (or (uiop/os:getenv "LISP_LIB")
      (error "LISP_LIB environment variable not set")))

(format t "  LISP_LIB directory: ~A~%" *lisp-library-directory*)

;; Set ASDF source registry to find all our packages
;; ASDF will recursively search these directories
(let ((base-dir *lisp-library-directory*))

  ;; Configure ASDF source registry (following Jan's pattern from GENERIC-INIT.lisp)
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,base-dir)
     (:exclude "Junk")
     :ignore-inherited-configuration))

  (format t "  ASDF source registry configured~%")

  ;; Set up ASDF central registry for specific directories (following Jan's approach)
  ;; Explicitly add the COMA-PROJECT directory where coma-json-server.asd lives
  (push (concatenate 'string
                     (namestring (truename base-dir))
                     "astro/COMA-PROJECT/")
        asdf:*central-registry*)

  (format t "  Added astro/COMA-PROJECT/ to ASDF central registry~%")

  ;; Load Quicklisp bundle if available (frozen dependencies)
  (let ((bundle-file (merge-pathnames "quicklisp-systems/bundle.lisp" base-dir)))
    (when (probe-file bundle-file)
      (format t "  Loading frozen Quicklisp bundle...~%")
      (load bundle-file)
      (format t "  Quicklisp bundle loaded~%"))))

(format t "SBCL initialization complete!~%~%")
