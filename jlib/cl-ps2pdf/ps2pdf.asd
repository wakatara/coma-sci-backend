

;; asdf file for file-io

#-(or sbcl cmucl abcl openmcl)
(error "RUN-PROGRAM package is not defined for this lisp implementation.")

(asdf:defsystem ps2pdf
  ;; needs jutils.asd to work
  :depends-on (run-program)
    ;;
  :components ((:file "ps2pdf")))
