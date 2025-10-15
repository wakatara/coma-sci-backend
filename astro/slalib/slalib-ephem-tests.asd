
;; a package of some simple tests - we need more

(asdf:defsystem slalib-ephem-tests
    :depends-on (slalib-ephem)
    :components
    ((:file "slalib-ephem-tests" :depends-on ())))