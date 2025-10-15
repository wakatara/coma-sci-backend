

;; asdf file for file-io

#-(or sbcl cmucl abcl openmcl)
(error "RUN-PROGRAM package is not defined for this lisp implementation.")

(asdf:defsystem run-program
  ;;
  :depends-on (osicat bordeaux-threads)
  ;;
  :components
  ((:file "run-program-package")
   ;;
   #+(or cmucl sbcl)
   (:file "run-program-sbcl+cmucl"
    :depends-on ("run-program-package"))
   ;;
   #+abcl
   (:file "run-program-abcl"
    :depends-on ("run-program-package"))
   ;;
   #+openmcl
   (:file "run-program-openmcl"
    :depends-on ("run-program-package"))
   ;;
   ;; run-program-utils.lisp is here partly to allow subsequent
   ;; :DEPENDS-ON to work without referring to specific lisp
   ;; implementations
   (:file "run-program-utils"
    :depends-on ( "run-program-package"
		  #+(or cmucl sbcl)  "run-program-sbcl+cmucl"
		  #+abcl "run-program-abcl"
		  #+openmcl "run-program-openmcl"))
   ;;
   (:file "run-program-to-string"
    :depends-on ("run-program-utils" "run-program-package"))
   ;;
   #+nil
   (:file "posix-ffi"
    :depends-on ("run-program-package"))
   ;;
   #+nil
   (:file "run-program-in-child"
    :depends-on ("run-program-package" "posix-ffi"))
   
  ))
     




    
