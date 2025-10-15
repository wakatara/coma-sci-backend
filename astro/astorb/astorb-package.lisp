

(defpackage astorb
  (:use #:cl)
  (:export
   #:*astorb-info-output-stream* ;; set to NIL for quiet
   #:*the-astorb*
   #:get-the-astorb
   ;;
   #:astorb #:astorb-p #:astorb-n #:astorb-epoch-of-elements
   #:astorb-astnum #:astorb-name #:astorb-hmag #:astorb-g #:astorb-iras-km
   #:astorb-class 
   #:astorb-code1 #:astorb-code2 #:astorb-code3
   #:astorb-code4 #:astorb-code5 #:astorb-code6
   #:astorb-orbarc #:astorb-nobs #:astorb-epoch-osc #:astorb-mean-anomaly
   #:astorb-arg-peri #:astorb-orbinc #:astorb-ecc #:astorb-a #:astorb-orbit-date
   ;;
   #:get-comet-elem-for-nth-asteroid 
   #:search-for-asteroids-by-name 
   #:find-numbered-asteroid
   ;;
   ;; astorb-retrieve.lisp
   #:retrieve-newest-astorb-file
   ;;
   ;; proximity.lisp - find nearest asteroids on sky.  expensive startup
   #:prox #:make-prox #:prox-p
   #:find-nearest-asteroids-in-prox
   ))



