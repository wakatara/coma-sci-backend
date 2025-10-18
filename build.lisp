;;;; build.lisp
;;;;
;;;; Build script for creating standalone coma-json-server executable
;;;;
;;;; Usage: sbcl --load build.lisp

(require :asdf)

;; Load the system definition which registers all paths
(load (merge-pathnames "coma-sci-backend.asd" *load-truename*))

;; Quiet astorb loading message
(defparameter cl-user::astorb-quiet t)

;; Load Quicklisp if available
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; Load the system
(format t "~%==> Loading coma-sci-backend system...~%~%")
(asdf:load-system :coma-sci-backend)

(format t "~%==> coma-sci-backend loaded successfully!~%~%")

;; If we want to build an executable, we'd use buildapp or similar
;; For now, just confirm the system loads

(format t "~%To create an executable, use buildapp:~%")
(format t "  buildapp --output coma-json-server \\~%")
(format t "           --asdf-path . \\~%")
(format t "           --load-system coma-sci-backend \\~%")
(format t "           --entry coma-json-server:main~%~%")

(quit)
