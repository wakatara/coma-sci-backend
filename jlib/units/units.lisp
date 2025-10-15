
;; various physical constants


(defpackage "UNITS"
  (:use "COMMON-LISP")
  (:export
   #:cgs-c #:cgs-G #:cgs-h #:cgs-hbar #:cgs-e #:cgs-me #:cgs-mp
   #:cgs-k #:cgs-Rgas #:cgs-a
   ;;
   #:cgs-sigma #:cgs-ev
   ;;
   #:cgs-micron #:cgs-angstrom #:cgs-meter #:cgs-km #:cgs-kg #:cgs-ton
   ;;
   #:cgs-AU #:cgs-pc #:cgs-yr
   #:cgs-Msun #:cgs-Rsun #:cgs-Lsun #:cgs-Mearth #:cgs-Rearth
   #:cgs-jansky #:cgs-newton #:cgs-watt #:cgs-joule
   #:cgs-solar-constant
   ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   #:mks-c #:mks-G #:mks-h #:mks-hbar #:mks-e #:mks-me #:mks-mp
   #:mks-k #:mks-Rgas #:mks-a
   ;;
   #:mks-sigma #:mks-ev
   ;;
   #:mks-micron #:mks-angstrom #:mks-meter #:mks-km #:mks-kg #:mks-ton
   ;;
   #:mks-AU #:mks-pc #:mks-yr
   #:mks-Msun #:mks-Rsun #:mks-Lsun #:mks-Mearth #:mks-Rearth
   #:mks-jansky #:mks-newton  #:mks-watt #:mks-joule
   #:mks-solar-constant

   #:0C-in-kelvin
   ))

(in-package units)

;; physical constants, prefixed with system

       
(defconstant cgs-c     2.99792458D10 "speed of light cm/s")       ;; cm/sec
(defconstant mks-c     2.99792458D8  "speed of light cm/s")       ;; m/sec

(defconstant cgs-G     6.67300D-8 "Grav constant in cm^3/g/s^2")
(defconstant mks-G     6.67300D-11 "Grav constant in m^3/kg/s^2")

(defconstant cgs-h     6.62620D-27 "Planck's constant in erg/s")
(defconstant mks-h     6.62620D-34 "Planck's constant in J/s")

(defconstant cgs-hbar  (/ cgs-h (* 2 PI)) "Planck's constant in erg/s /2pi")
(defconstant mks-hbar  (/ mks-h (* 2 PI)) "Planck's constant in J/s /2pi")

(defconstant cgs-e     4.803242D-10 "electron charge, esu")     
(defconstant mks-e     1.60217646D-19 "electron charge, Coulombs")

;; mass of electron
(defconstant cgs-me    9.10938188D-28 "mass of electron, g")             
(defconstant mks-me    9.10938188D-31 "mass of electron, kg")             

;; mass of proton
(defconstant cgs-mp    1.67262158D-24 "mass of proton, g") 
(defconstant mks-mp    1.67262158D-27  "mass of proton, kg") 

;; Boltzmann constant
(defconstant cgs-k     1.3806503D-16  "Boltzmann constant,  erg/K")
(defconstant mks-k     1.3806503D-23  "Boltzmann constant,  J/K")
;;

;; universal gas constant R
(defconstant cgs-Rgas 8.314472e7 "universal gas constant R in erg/(K.mol)")
(defconstant mks-Rgas 8.314472   "universal gas constant R in J/(K.mol)")

;; 8 pi^5 k^4/(15 c^4 ^3)
(defconstant cgs-a     7.5657D-15 "8 pi^5 k^4/(15 c^4 ^3),  erg/cm^3/K^4")
(defconstant mks-a     7.5657D-16 "8 pi^5 k^4/(15 c^4 ^3),  J/cm^3/K^4")   

(defconstant cgs-sigma (* cgs-a cgs-c 0.25D0)
      "Stefan-Boltzman constant,  g  s^-3  K^-4")
(defconstant mks-sigma (* mks-a mks-c 0.25D0)
      "Stefan-Boltzman constant, kg  s^-3 / K^-4")

;; electron volt
(defconstant cgs-ev 1.60217646d-12 "electron volt, ergs")
(defconstant mks-ev 1.60217646d-19 "electron volt, J")


(defconstant cgs-micron 1D-4)                    ;; cm
(defconstant mks-micron 1D-6)                    ;; m

(defconstant cgs-angstrom 1D-8)                  ;; cm
(defconstant mks-angstrom 1D-10)                 ;; m

(defconstant cgs-meter 1D2)
(defconstant mks-meter 1d0)

(defconstant cgs-km    1D5)
(defconstant mks-km    1D3)

(defconstant cgs-kg    1D3)
(defconstant mks-kg    1D0)

(defconstant cgs-ton   1D6)
(defconstant mks-ton   1D3)

(defconstant cgs-gram   1D0)
(defconstant mks-gram   1D-3)


;; latest value since 2012
(defconstant cgs-AU      1.495978707d13 "Astronomical unit, cm") ;; cm
(defconstant mks-AU      1.495978707d11 "Astronomical unit, m") ;; cm

;(defconstant cgs-AU      1.4959787069d13)     ;; cm
;(defconstant mks-AU      1.4959787069d11)     ;; m

(defconstant cgs-pc      3.08568025D18 "Parsec, cm")       ;; cm
(defconstant mks-pc      3.08568025D16 "Parsec, m")       ;; m

(defconstant cgs-yr      #.(* 365.25d0 24 3600) "year in seconds")
(defconstant mks-yr      #.(* 365.25d0 24 3600) "year in seconds")

(defconstant cgs-Msun    1.98892D33 "mass of Sun, g")       ;; gram
(defconstant mks-Msun    1.98892D30 "mass of Sun, kg")       ;; kg

(defconstant cgs-Rsun    6.96D10 "radius of sun, cm")       ;; cm
(defconstant mks-Rsun    6.96D8  "radius of sun, m")        ;; m

;; IS THIS VALUE CORRECT?  MAYBE TO 1% OR SO
(defconstant cgs-Lsun    3.8268D33 "luminiosity of sun, erg/s, +-1 1%")  
(defconstant mks-Lsun    3.8268D26 "luminiosity of sun, J/s, +-1 1%")  

(defconstant cgs-Mearth  5.98D27 "Mass of Earth, g")  
(defconstant mks-Mearth  5.98D24 "Mass of Earth, kg")  

(defconstant cgs-Rearth  6.37D8 "Radius of Earth, g")          
(defconstant mks-Rearth  6.37D6 "Radius of Earth, kg")          


(defconstant cgs-watt 1.0d7) ;; watts in ergs
(defconstant mks-watt 1.0d0)

(defconstant cgs-joule 1.0d7)
(defconstant mks-joule 1.0d0)

;; newtons in terms of dynes
(defconstant cgs-newton 1.0d5)
(defconstant mks-newton 1.0d0)



(defconstant cgs-jansky  1D-23 "Jansky, in dyne/cm^2 hz^-1")
(defconstant mks-jansky  1D-26 "Jansky, in ;; erg/m^2 hz^-1")


;; at 1 AU
(defconstant cgs-solar-constant (/ cgs-Lsun (* 4.0D0 PI cgs-AU cgs-AU)))
(defconstant mks-solar-constant (/ mks-Lsun (* 4.0D0 PI mks-AU mks-AU)))

(defconstant 0C-in-kelvin 273.15d0 "0 degrees Centigrade in Kelvin")








  


