

(asdf:defsystem imutils-test
  :depends-on (imutils
	       pgplot
	       plot)
    ;;
    :components
  ((:file "wiener-deconvolve-test")))

