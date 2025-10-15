
(defpackage slalib-ephem-tests
  (:use #:cl #:slalib-ephem)
  (:export
   #:*elem-elst-pizarro*
   #:*elem-juno*
   #:test-elst-piz
   #:test-juno
   ))

(in-package slalib-ephem-tests)

(defparameter *elem-elst-pizarro*
  (make-comet-elem       
   :id "Elst-Pizarro"
   :epoch (- 2452361.5d0 2400000.5d0)
   :time-peri (- 2452237.06493478d0 2400000.5d0)
   :orbinc 1.385555375885813d0
   :anode 160.2284256563968d0 
   :perih 132.9747580443433d0
   :q 2.635362196760693d0
   :e 0.165290822206447d0))

(defun test-elst-piz (&key (mjd-obs (- 2452426.5d0 2400000.5d0))
		           (ra-jpl 359.34757d0)					       
		           (dec-jpl -0.25043d0)
		           (r-jpl 2.91077495020406d0)
		           ;(olon 204.527800d0)
		           ;(olat 19.8261152d0)
		           (observatory "uh88")
		           (perturb nil)
		           ;(altitude 4200d0)
		           (convert-to-mean-epoch 2000d0))
  ;;
  (multiple-value-bind (ra dec r)
      (compute-radecr-from-comet-elem-for-observatory 
       *elem-elst-pizarro* mjd-obs observatory ;; olon olat
       :perturb perturb
       :convert-to-mean-epoch convert-to-mean-epoch
       ;;:altitude altitude
       )
    (format 
     t 
     "PREDICTED: ~% ra=~,6F~% dec=~,6F~% r=~,6F~%~%" ra dec r)
    (format 
     t 
     "JPL      : ~% ra=~,6F~% dec=~,6F~% r=~,6F~%~%" ra-jpl dec-jpl r-jpl)
    (format 
     t 
     "Disagreement:~% RA  (arcsec): ~,3F~% DEC (arcsec): ~,3F~% R  ~,3F~%"
     (* 3600 (- ra-jpl ra))
     (* 3600 (- dec-jpl dec))
     (- r-jpl r))))
  





(defparameter *elem-juno*
  (make-comet-elem       
   :id "Juno"
   :epoch (- 2445512.5d0 2400000.5d0)
   :time-peri (- 2445699.402415101d0 2400000.5d0)
   :orbinc 12.99563845810963d0 
   :anode 170.5822369920256d0
   :perih 247.0194896550651d0
   :q 1.981688070776453d0
   :e 0.2574420453110412d0 ))

(defun test-juno (&key (mjd-obs (+ 1000d0 (- 2445512.5d0 2400000.5d0)))
		           (ra-jpl 256.60701d0)					       
		           (dec-jpl -8.96147d0)
		           (r-jpl 2.94949593184846d0)
		           (observatory "uh88")
		           ;(olon 204.527800d0)
		           ;(olat 19.8261152d0)
		           (perturb nil)
		           (elem *elem-juno*)
		           ;(altitude 4200d0)
		           (convert-to-mean-epoch 2000d0))
  ;;
  (multiple-value-bind (ra dec r)
      (compute-radecr-from-comet-elem-for-observatory 
       elem mjd-obs  observatory ;olon olat
       :perturb perturb
       :convert-to-mean-epoch convert-to-mean-epoch
       ;:altitude altitude
       )
    (format 
     t 
     "PREDICTED: ~% ra=~,6F~% dec=~,6F~% r=~,6F~%~%" ra dec r)
    (format 
     t
     "JPL      : ~% ra=~,6F~% dec=~,6F~% r=~,6F~%~%" 
     ra-jpl dec-jpl r-jpl)
    (format 
     t 
     "Disagreement:~% RA  (arcsec): ~,5F~% DEC (arcsec): ~,5F~% R  ~,5E~%"
	    (* 3600 (- ra-jpl ra))
	    (* 3600 (- dec-jpl dec))
	    (- r-jpl r))))
  