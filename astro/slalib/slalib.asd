
(asdf:defsystem slalib
    ;; needs jutils.asd to work
    :depends-on (waaf-cffi float-utils) ;; use sbcl2cffi layer atop cffi
    ;;
    :components
    ((:file "slalib")))




;; if we trust slalib to be threadsafe, enable this. It will disable
;; locking on slalib calls, and allow concurrency in slalib. Patrick
;; Wallace (priv. comm.) says Olde Fortran Slalibbe does not preserve
;; state except for random functions (not used), and gfortran is
;; supposedly threadsafe in the absence of preserved state

(pushnew :slalib-is-threadsafe *features*)
