

(in-package jtypes)


;; integer types that avoid bound checking by being smaller than fixnum
(deftype fast-signed-int ()
  '(integer
    #.(ash most-negative-fixnum -1)  
    #.(ash most-positive-fixnum -1)))

(deftype fast-unsigned-int ()
  '(integer
     0
     #.(ash most-positive-fixnum -1)))

  