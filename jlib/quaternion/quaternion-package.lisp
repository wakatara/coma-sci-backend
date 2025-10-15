
(defpackage quaternion
  (:use #:cl)
  (:export
   ;; the functions that take and return double floats
   #:quaternion-multiply
   #:invert-quaternion
   #:make-rotation-quaternion
   #:rotate-around-vector
   #:quaternion-rotate
   #:compute-quaternion-for-transform
   #:compute-quaternion-for-angular-transform
   #:convert-quaternion-to-euler-axis-and-angle
   #:compute-euler-axis-and-angle-for-transform
   ;;
   ;; the functions that deal with quaternion structs and vectors
   #:quaternion #:quaternion-p #:make-quaternion
   #:quaternion-r #:quaternion-i #:quaternion-j #:quaternion-k
   #:make-rotation-quaternion-struct
   #:quaternion-struct-multiply
   #:invert-quaternion-struct
   #:convert-quaternion-struct-to-euler-axis-and-angle
   #:quaternion-struct-rotate
   #:compute-quaternion-struct-for-transform
   #:compute-quaternion-struct-for-angular-transform
   ;; quaternion-utils.lisp - some generally useful add-on utils
   #:regularize-equatorial-coordinates
   #:rotate-lon-lat-using-quaternion
   #:xyz->lon-lat
   #:lon-lat->xyz
   ))
