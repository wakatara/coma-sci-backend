

(in-package orbital-mech)

(defun pericenter-from-a-e (a e)
  "Compute pericenter from semimajor axis A and eccentricity E"
  (* (- 1d0 e) a))

(defun a-from-pericenter-e (rperi e)
  "Compute semimajor axis A from pericenter RPERI and eccentricity E"
  (/ rperi (- 1d0 e)))

(defun a-from-apocenter-e (rapo e)
  "Compute semimajor axis A from apocenter RAPO and eccentricity E"
  (/ rapo (+ 1d0 e))) 

(defun apocenter-from-a-e (a e)
  "Compute apocenter from semimajor axis A and eccentricity E"
  (* (+ 1d0 e) a))

(defun a-from-apo-peri-centers (rperi rapo)
  "Compute semimajor axis A from pericenter RPERI and apocenter RAPO"
  (* 0.5d0 (+ rperi rapo)))

(defun e-from-apo-peri-centers (rperi rapo)
  "Compute eccentricity E from pericenter RPERI and apocenter RAPO"
  (/ (* 1d0 (- rapo rperi))
     (* 1d0 (+ rapo rperi))))


(defun qualities-infer (&key (rperi nil) (rapo nil) (a nil) (e nil))
  "Infer qualities RPERI, RAPO, A, E from those given"
  (let ((rperi* rperi) ;; stars are final inferred values
	(rapo*  rapo)
	(a* a)
	(e* (cond (e e)
		  ((and rperi rapo)
		   (e-from-apo-peri-centers rperi rapo))
		  ((and rperi a)
		   (- 1 (/ rperi a)))
		  ((and rapo a)
		   (- (/ rapo a) 1d0))
		  (t
		   nil))))	    
		  
    (when e* ;; don't infer this yet
      (when a
	(setf rperi* (pericenter-from-a-e a e*))
	(setf rapo*  (apocenter-from-a-e  a e*)))
      (when rperi
	(setf a* (a-from-pericenter-e rperi e*))
	(setf rapo*  (apocenter-from-a-e  a* e*)))
      (when rapo
	(setf a* (a-from-apocenter-e rapo e*))
	(setf rperi*  (pericenter-from-a-e  a* e*))))

    (format t "        IN      OUT~%")
    (format t "a      ~,3F     ~,3F~%"  (or a " --  ") a*)
    (format t "e      ~,3F     ~,3F~%"  (or e " --  ") e*)
    (format t "rperi  ~,3F     ~,3F~%"  (or rperi " --  ") rperi*)
    (format t "rapo   ~,3F     ~,3F~%"  (or rapo  " --  ") rapo*)
    ))
  



