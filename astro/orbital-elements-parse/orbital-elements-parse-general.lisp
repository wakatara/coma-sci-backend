#|

Try to parse any orbital elements string

|#


(in-package orbital-elements-parse)


(defun parse-general-elem-string (text)
  "Parses Lisp, MPC, JPL, Findorb elements to ORBITAL-ELEMENTS:COMET-ELEM"
  (declare (type string text))
  (when (> (length text) 1000)
    (error "Text too long to be orbital elements"))  
  (or

   ;; MPC elements
   (ignore-errors (parse-mpc-elem-string text))

   ;; JPL elements
   (ignore-errors (parse-jpl-elem-string text))

   ;; Findorb elements
   (ignore-errors (parse-findorb-elem-string text :convert-to-cometary t))
   
   ;; a lisp COMET-ELEM or ASTEROID-ELEM object - warning - should be using a
   ;; safe-read of some sort, like https://github.com/phoe/safe-read/blob/master/safe-read.lisp
   (let ((thing (ignore-errors (read-from-string text))))
     (cond ((typep thing 'orbital-elements:comet-elem)
	    thing) 
	   ((typep thing 'orbital-elements:asteroid-elem)
	    (slalib-ephem:convert-asteroid-elem-to-comet-elem thing))
	   (t
	    nil)))
   ;;
   (error "Could not parse orbital element text as Lisp ORBITAL-ELEMENTS, JPL, or FindOrb: ~S" text)))
   
	   
