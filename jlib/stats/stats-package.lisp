


(defpackage #:stats
  (:use #:common-lisp)
  (:export
   ;; stats-utils.lisp
   #:sum-of-elements
   #:mean-of-elements
   #:percentile-of-elements ;; OBSOLETE - use fraction-of-elements
   #:fraction-of-elements
   #:fast-double-float-vec-median
   #:fast-double-float-vec-fraction
   #:sum-scatter-of-elements
   #:variance-of-elements
   #:sigma-of-elements
   #:geometric-mean-of-elements
   #:sum-in-quadrature-of-elements
   #:seq-min  ;; prefer MIN-OF-ELEMENTS because it does arrays, not just vecs
   #:seq-max
   #:min-of-elements
   #:max-of-elements
   #:mode-of-elements
   #:median-of-elements
   #:hodges-lehman-estimator #:hodges-lehman-estimator/dbl ;; mean/median hybrid
   #:best-mean-and-sigma
   #:uncertainty-a/b
   #:uncertainty-a*b
   #:add-in-quadrature
   #:fisher-prob-combine
   #:sigma-from-quartiles
   ;;
   ;; stats-tests.lisp
   #:ks-test--sequences
   #:ks-test--vectors ;; deprecated
   #:ks-test--function
   ;;
   #:kuiper-test--sequences
   #:kuiper-test--vectors ;; deprecated
   #:kuiper-test--function
   #:chisqr-bin-count-prob
   ;;
   ;; stats-dists.lisp 
   #:gaussian-probability
   #:cumulative-gaussian-probability
   #:compute-one-sided-gaussian-z-value-for-prob
   #:compute-two-sided-gaussian-z-value-for-prob
   #:poisson-probability
   #:cumulative-poisson-probability
   #:binomial-probability
   #:cumulative-binomial-probability
   #:chi-square-probability
   #:complementary-chi-square-probability
   #:cumulative-probability 
   ;;
   #:pearson-r
   #:spearman-r

   ;; clipping.lisp
   #:z-score-clip-data
   ))

