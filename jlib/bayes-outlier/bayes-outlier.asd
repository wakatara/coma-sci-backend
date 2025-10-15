

(asdf:defsystem bayes-outlier
  :depends-on (golden-section stats bisection-root)
  :components 
  ((:file "bayes-outlier" :depends-on ())))
