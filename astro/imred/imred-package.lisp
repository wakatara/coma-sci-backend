

(defpackage imred
  (:use #:cl)
  (:export
   ;; trim-image.lisp
   #:trim-image
   ;; copy-image.lisp
   #:copy-image
   #:copy-image-section
   ;; stack-image.lisp
   #:stack-images
   ;; normalize-gain.lisp
   #:normalize-gain
   ;; zerocombine.lisp
   #:zerocombine
   ;; flatcombine.lisp
   #:flatcombine
   ;; fringecombine.lisp
   #:fringecombine
   ;; fringecor.lisp
   #:fringecor
   ;; ccdproc
   #:ccdproc
   ;; reduction-plan.lisp
   #:reduction-plan #:reduction-plan-p ;; don't bother exporting accessors
   ;; process-set.lisp
   #:process-fits-list
   ;;
   ;; export these because a fileset is returned by process-fits-list
   #:fileset #:fileset-p #:fileset-bias-list #:fileset-flatsets
   #:fileset-output-fits-list
   #:fileset-output-bias-list
   #:fileset-output-flat-list
   #:fileset-output-fringe-list
   #:fileset-output-reduced-obj-list
   #:fileset-output-fringecorrected-obj-list
   ;;
   #:flatset #:flatset-p #:flatset-group #:flatset-flat-list

 
   ;; reduction plans for specific instruments
   #:reduction-plan-uh88-tek
   #:reduction-plan-smarts-7x7-1amp
   #:reduction-plan-suprime-cam
   #:reduction-plan-hypersuprime-cam
   #:reduction-plan-lowell-prism
   #:reduction-plan-hct-hfosc
   #:reduction-plan-gmos-n-e2v-chip2
   #:reduction-plan-gmos-full-chip-array
   #:reduction-plan-gmos-frac-chip-array
   #:reduction-plan-vlt-fors2 

   ;; function to make the right reduction plan for
   ;; an instrument id
   #:make-reduction-plan-for-instrument

   ;; utils.lisp
   ;;   function to break up an image list into COMPATIBLE-IMAGE-SETs, like images of
   ;;   the same instrument, for the same chip 
   #:compatible-image-set   #:compatible-image-set-p
   #:compatible-image-set-instrument
   #:compatible-image-set-chip-id
   #:compatible-image-set-extension-signature
   #:compatible-image-set-file-list
   #:divide-image-list-into-compatible-image-sets 
   )) 

 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package imred)

;; some global variables that bound when processing so that helper
;; functions can do stuff
(defvar *ff-list* nil "List of current cfitsio:fits-file structs being processed")
(defvar *fits-file-list* nil "List of current fits filenames being processed")
(defvar *hdu-num* 0 "number of HDU being processed")
(defvar *imsec-generator* nil "generator function for imsecs being processed")
	
 


(defmacro float-nan-p (x)
  `(float-utils:float-nan-p ,x))

(defun make-single-float-nan ()
  float-utils:*single-float-nan*)


;; value used internally to denote invalid pixels - don't use most-negative-single-float
;; because it will underflow
(defconstant +invalid-pixel-value+ #.(* 0.5 most-negative-single-float))    

;; define this here first to quell compiler complaints about undefined
;; variable
(defparameter *default-reduction-plan* nil)
