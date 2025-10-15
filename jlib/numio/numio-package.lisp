

(defpackage #:numio
  (:use #:common-lisp #:jk-parse-float)
  (:export ;; #:file-count-lines ;; this is no longer doable for a stream
   ;; numio.lisp
   #:read-double-cols
   #:read-cols
   #:read-object-from-stream
   #:read-objects-from-stream 
   #:read-2d-array
   #:parse-float ;; export this for compatibility with parse-float
   #:with-cols
   ))
