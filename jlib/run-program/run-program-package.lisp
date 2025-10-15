

(defpackage run-program
  (:use #:cl)
  (:export
   #:run-program-LISP-IMPLEMENTATION.lisp
   #:run-program
   #:process-p
   #:process-alive-p      #:process-close
   #:process-core-dumped  #:process-error
   #:process-exit-code    #:process-input
   #:process-kill         #:process-output
   ;; run-program-utils.lisp
   #:get-path-list
   #:find-program-in-path
   ;; run-program-to-string.lisp
   #:run-program-to-string
   ))
 
 
