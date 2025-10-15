
;; asdf for md-optim multidim uber-definitions

(asdf:defsystem md-optim
    :depends-on (bisection-root 
		 float-utils)  ;; for infinity, NaN
    ;;
    :components
    ((:file "md-optim")))


