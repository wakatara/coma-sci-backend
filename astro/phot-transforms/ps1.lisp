
(in-package phot-transforms)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PS1 to SDSS tranforms from Table 6 of 
;;  Tonry, J. L., Stubbs, C. W., Lykke, K. R., et al.,
;;  2012, ApJ, 750, 99

;; a transformation is of the form X=m0 + a0 + a1 (x-y) + a2 (x-y)^2
(defun %ps1-transform (m0 x y a0 a1 a2)
  (let ((color (- x y)))
    (+ m0 a0 
       (* a1 color)
       (* a2 color color))))

(defun gsdss-from-grps1 (gps1 rps1)
  (%ps1-transform gps1 gps1 rps1 0.013 0.145 0.019))

;; the PS1 DRAVG found that Tonry's transformation is off, reflecting
;; an error in either SDSS or PS1 AB mags.  Assuming PS1 doesn't change,
;; we will correct PS1 to SDSS using this correction
(defun rsdss-from-grps1 (gps1 rps1)
  (%ps1-transform rps1 gps1 rps1 
		  (+ -0.001 0.0233) ;; correction to Tonry et al
		  0.004 0.007))

(defun isdss-from-grips1 (gps1 rps1 ips1)
  (%ps1-transform ips1 gps1 rps1 -0.005 0.011 0.010))

(defun zsdss-from-grzps1 (gps1 rps1 zps1)
  (%ps1-transform zps1 gps1 rps1 0.013 -0.039 -0.012))

(defun zsdss-from-gryps1 (gps1 rps1 yps1)
  (%ps1-transform yps1 gps1 rps1 -0.031 0.111 0.004))

(defun rsdss-from-grwps1 (gps1 rps1 wps1)
  (%ps1-transform wps1 gps1 rps1 -0.024 -0.149 0.155))


(defun Bj-from-grps1 (gps1 rps1)
  (%ps1-transform gps1 gps1 rps1 0.212 0.556 0.034))

(defun Vj-from-grps1/r (gps1 rps1)
  "This version uses rps1 as the mag to correct with color"
  (%ps1-transform rps1 gps1 rps1 0.005 0.462 0.013))

(defun Rc-from-grps1 (gps1 rps1)
  (%ps1-transform rps1 gps1 rps1 -0.137 -0.108 -0.029))

(defun Ic-from-grips1 (gps1 rps1 ips1)
  (%ps1-transform ips1 gps1 rps1 -0.366 -0.136 -0.018))

(defun Vj-from-grwps1 (gps1 rps1 wps1)
  (%ps1-transform wps1 gps1 rps1 -0.021 0.299 0.187))

(defun Vj-from-grps1/g (gps1 rps1)
  "This version uses gps1 as the mag to correct with color"
  (%ps1-transform gps1 gps1 rps1 0.005 -0.536 0.011))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; we don't use this yet - perhaps should have automated it more cleverly
;; from Tonry 2012 ApJ 750;99, May 10, Table 6
;;  Y= A0 + A1.x + A2.x^2 = B0 + B1.x   (both linear and quadratic provided)
(defparameter *tonry-ps1-transforms*
  ;;     0                    1          2      3     4      5     6      7      8
  ;;     X                    Y          A0     A1    A2     +/-   B0     B1     +/-
  '(((:gsdss  :rsdss) (:gps1  :gsdss) -0.011 -0.125 -0.015 0.006 -0.012 -0.139 0.007)
    ((:gsdss  :rsdss) (:rps1  :rsdss) 0.001 -0.006 -0.002 0.002 0.000 -0.007 0.002)
    ((:gsdss  :rsdss) (:ips1  :isdss) 0.004 -0.014 0.001 0.003 0.004 -0.014 0.003)
    ((:gsdss  :rsdss) (:zps1  :zsdss) -0.013 0.040 -0.001 0.009 -0.013 0.039 0.009)
    ((:gsdss  :rsdss) (:yps1  :zsdss) 0.031 -0.106 0.011 0.023 0.031 -0.095 0.024)
    ((:gsdss  :rsdss) (:wps1  :rsdss) 0.018 0.118 -0.091 0.012 0.012 0.039 0.025)
    ;;
    ((:bj  :vj) (:gps1  :bj) -0.108 -0.485 -0.032 0.011 -0.104 -0.523 0.013)
    ((:bj  :vj) (:rps1  :vj ) 0.082 -0.462 0.041 0.025 0.077 -0.415 0.025)
    ((:bj  :vj) (:rps1  :rc) 0.117 0.128 -0.019 0.008 0.119 0.107 0.009)
    ((:bj  :vj) (:ips1  :ic) 0.341 0.154 -0.025 0.012 0.343 0.126 0.013)
    ((:j2mass  :h2mass) (:zps1  :j2mass) 0.418 1.594 -0.603 0.068 0.428 1.260 0.073)
    ((:j2mass  :h2mass) (:yps1  :j2mass) 0.528 0.962 -0.069 0.061 0.531 0.916 0.061)
    ;;
    ((:gps1  :rps1) (:gsdss  :gps1) 0.013 0.145 0.019 0.008 0.014 0.162 0.009)
    ((:gps1  :rps1) (:rsdss  :rps1) -0.001 0.004 0.007 0.004 -0.001 0.011 0.004)
    ((:gps1  :rps1) (:isdss  :ips1) -0.005 0.011 0.010 0.004 -0.004 0.020 0.005)
    ((:gps1  :rps1) (:zsdss  :zps1) 0.013 -0.039 -0.012 0.010 0.013 -0.050 0.010)
    ((:gps1  :rps1) (:zsdss  :yps1) -0.031 0.111 0.004 0.024 -0.031 0.115 0.024)
    ((:gps1  :rps1) (:rsdss  :wps1) -0.024 -0.149 0.155 0.018 -0.016 -0.029 0.031)
    ;;
    ((:gps1  :rps1) (:bj  :gps1) 0.212 0.556 0.034 0.032 0.213 0.587 0.034)
    ((:gps1  :rps1) (:vj  :rps1) 0.005 0.462 0.013 0.012 0.006 0.474 0.012)
    ((:gps1  :rps1) (:rc  :rps1) -0.137 -0.108 -0.029 0.015 -0.138 -0.131 0.015)
    ((:gps1  :rps1) (:ic  :ips1) -0.366 -0.136 -0.018 0.017 -0.367 -0.149 0.016)
    ((:gps1  :rps1) (:vj  :wps1) -0.021 0.299 0.187 0.025 -0.011 0.439 0.035)
    ((:gps1  :rps1) (:vj  gps1) 0.005 -0.536 0.011 0.012 0.006 -0.525 0.012)))













