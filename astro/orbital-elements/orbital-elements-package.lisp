
(defpackage orbital-elements
  (:use #:cl)
  (:nicknames  #:orbelem)
  (:export
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; orbital-elements.lisp
   #:comet-elem #:make-comet-elem #:comet-elem-p
   #:make-comet-elem-from-jpl
   #:comet-elem-id #:comet-elem-epoch #:comet-elem-time-peri
   #:comet-elem-orbinc #:comet-elem-anode #:comet-elem-perih
   #:comet-elem-q #:comet-elem-e #:comet-elem-nongravs #:comet-elem-data
   ;;
   #:comet-elem+err #:make-comet-elem+err #:comet-elem+err-p
   #:comet-elem+err-time-peri-err
   #:comet-elem+err-orbinc-err #:comet-elem+err-anode-err #:comet-elem+err-perih-err
   #:comet-elem+err-q-err #:comet-elem+err-e-err
   ;;
   #:nongravs #:nongravs-p #:make-nongravs #:nongravs-dt
   #:nongravs-a1 #:nongravs-a2 #:nongravs-a3
   ;;
   ;; parent class for asteroid-desc and comet-desc
   #:object-desc #:object-desc-p #:object-desc-name #:object-desc-source
   ;;
   #:asteroid-desc #:asteroid-desc-p #:make-asteroid-desc
   #:asteroid-desc-name #:asteroid-desc-number #:asteroid-desc-source
   #:asteroid-desc-h #:asteroid-desc-g
   #:asteroid-desc-period #:asteroid-desc-albedo #:asteroid-desc-radius
   #:asteroid-desc-source
   ;;
   #:comet-desc #:comet-desc-p #:make-comet-desc
   #:comet-desc-name #:comet-desc-source
   
   ;;
   #:asteroid-elem #:make-asteroid-elem #:asteroid-elem-p
   #:asteroid-elem-id 
   #:asteroid-elem-epoch #:asteroid-elem-orbinc #:asteroid-elem-anode
   #:asteroid-elem-perih #:asteroid-elem-a #:asteroid-elem-e #:asteroid-elem-m
   #:asteroid-elem-data
   ;;
   #:asteroid-elem+err #:make-asteroid-elem+err #:asteroid-elem+err-p
   #:asteroid-elem+err-orbinc-err    #:asteroid-elem+err-anode-err
   #:asteroid-elem+err-perih-err    #:asteroid-elem+err-e-err
   #:asteroid-elem+err-m-err
   
   ;;
   #:univ-elem  #:make-univ-elem #:univ-elem-p 
   #:univ-elem-id #:univ-elem-velem #:univ-elem-data

   ))
 
