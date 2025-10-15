;; some photometric conversions

(defpackage phot-transforms
  (:use #:cl)
  (:export
   ;;
   ;; sdss.lisp
   ;; Jester transforms are more up to date, but not quite
   ;; mathematically consistent and apply only for Rc-Ic<1.15
   ;; WARNING - Jester *really* disagrees with Fukugita and Lupton, 
   ;; because of the r-i to Rc-Ic conversion.  
   #:sloan-gr-to-VB/jester
   #:VB-to-sloan-gr/jester
   #:B-V-to-sloan-g-r/jester
   #:Rc-Ic-to-sloan-r-i/jester
   #:Rc-Ic-to-sloan-r-z/jester
   #:sloan-r-i-to-Rc-Ic/jester ;; not consistent with Rc-Ic-to-sloan-r-i/jester
   #:sloan-r-z-to-Rc-Ic/jester
   #:BVRI-from-sloan-gri/jester
   #:test-sloan-conversions/jester
   ;;
   ;; fukugita transforms are outdated, but are more mathematically
   ;; consistent because we derived the inverse transforms from the transforms
   ;; themselves - also contain cases for extreme colors like Rc-Ic>1.15
   #:sloan-gr-to-VB/fukugita
   #:VB-to-sloan-gr/fukugita
   #:VRc-to-sloan-r/fukugita
   #:B-V-to-sloan-g-r/fukugita
   #:Rc-Ic-to-sloan-r-i/fukugita
   #:Rc-Ic-to-sloan-r-z/fukugita
   #:sloan-r-i-to-Rc-Ic/fukugita
   #:sloan-r-z-to-Rc-Ic/fukugita
   #:BVRI-from-sloan-gri/fukugita
   #:test-sloan-conversions/fukugita
   ;;
   ;; Lupton's conversions from 
   ;;  http://www.sdss.org/dr4/algorithms/sdssUBVRITransform.html#Lupton2005
   ;; these are up to date, and seem consistent with Fukugita.
   #:BVRI-from-sloan-gri/lupton
   #:sloan-u+g-to-Bj/lupton
   #:sloan-g+r-to-Bj/lupton
   #:sloan-u+g-to-Vj/lupton
   #:sloan-g+r-to-Vj/lupton
   #:sloan-g+r-to-Rc/lupton
   #:sloan-r+i-to-Rc/lupton
   #:sloan-r+i-to-Ic/lupton
   #:sloan-i+z-to-Ic/lupton
   ;;
   ;; 
   ;; ps1.lisp
   #:gsdss-from-grps1
   #:rsdss-from-grps1
   #:isdss-from-grips1
   #:zsdss-from-grzps1
   #:zsdss-from-gryps1
   #:rsdss-from-grwps1
   #:Bj-from-grps1
   #:Vj-from-grps1/r 
   #:Rc-from-grps1
   #:Ic-from-grips1
   #:Vj-from-grwps1
   #:Vj-from-grps1/g
   ;;
   ;;
   ;; atlas.lisp
   #:catlas-from-gab+rab
   #:oatlas-from-rab+iab
   #:gab-from-catlas+oatlas
   #:rab-from-catlas+oatlas
   #:iab-from-catlas+oatlas
   ;; dubious ones, depending on PS1 tranforms
   #:BJ-from-catlas+oatlas
   #:VJ-from-catlas+oatlas
   #:Rc-from-catlas+oatlas
   #:IC-from-catlas+oatlas
   ))

