;; routines for converting RA and Dec sexagesimal formats
;;
;;  NOTE: modified 2004-06-08 Time: 14:28:05 so that some routines
;;        use VALUES and not VECTOR to return split ra/dec - watch out
;;        for broken legacy code.


(defpackage "RA-DEC"
  (:use "COMMON-LISP")
  (:export
   #:hms->deg    #:hms-seq->deg
   #:dms->deg    #:dms-seq->deg
   #:deg->hms    #:deg->hms-vec
   #:deg->dms    #:deg->dms-vec
   #:hms-string->deg
   #:hms-string->hms
   #:dms-string->deg
   #:dms-string->dms
   #:dms->dms-string  #:dms-vec->dms-string
   #:hms->hms-string  #:hms-vec->hms-string
   #:deg->dms-string
   #:deg->hms-string
   #:deg->apj-ra-string #:deg->apj-dec-string
   ;; generic convertors from any format (float, vector, list, string)
   #:ra->deg #:ra->ra-string
   #:dec->deg #:dec->dec-string
   ))

(in-package ra-dec)
	

(defmacro is-sequence (thing)
  `(or (vectorp ,thing) (listp ,thing)))

;; f-modulo takes a real number, and returns a "floating point 
;;   modulo" in Q of it.  Conceptually, (fmod x Q) adds/subtracts
;;   Q to/from x until the result is in the range [0,Q).
;;   The behavior of f-modulo is consistent with that of modulo
;;   with respect to the sign of x and Q.
;;
(defun f-modulo (x q)
  (- x (* (floor (/ x q)) q)))

;;
;; routine to round a floating point number to N digits of precision
;;
;; this preserves the funny behavior of round that (round 1.5) => 2.0
;;                                                 (round 2.5) => 2.0
;;
(defun round-n (x n)
  (setf x (coerce x 'double-float))
  (setf n (coerce n 'fixnum))
  (if (> n 15)
      x
    (let ((p  (* 1.d0 (expt 10 n))))
      (/ (round (* x p)) p))))


;; put degrees in range (-180 to 180]
(defun degrees-180 (deg)
  (let ((deg-360 (f-modulo deg 360.0d0)))
    (if (> deg-360 180.0d0)
        (- deg-360 360.0d0)
      deg-360)))

;; put degrees in range [0,360)
(defmacro degrees-360 (deg)
  `(f-modulo ,deg 360.0d0))

;; convert hours min sec to degrees in range [0,360)
(defun hms-seq->deg (hms-seq)
  "convert a sequence with HR MIN SEC to decimal degrees"
  (if (is-sequence hms-seq)
      (let ((hr  (elt hms-seq 0))
            (min (elt hms-seq 1))
            (sec (elt hms-seq 2)))
        (f-modulo (+ (* hr 15.0d0)
                     (* min 0.25d0)
                     (* sec 0.004166666666666667d0))
                  360.0d0))
      (error "Non-sequence passed to hms->deg")))

(defun hms->deg (hr min sec)
  "convert HR MIN SEC to decimal degrees"
  (f-modulo (+ (* hr 15.0d0)
	       (* min 0.25d0)
	       (* sec 0.004166666666666667d0))
	    360.0d0))


;; convert deg min sec to degrees in range (-180,180]
(defun dms-seq->deg (dms-seq)
  "convert a sequence with SIGN DEG MIN SEC to decimal degrees"
  (if (is-sequence dms-seq)
      (let ((sign (elt dms-seq 0)) 
            (deg (elt dms-seq 1))
            (min (elt dms-seq 2)) 
            (sec (elt dms-seq 3))) 
        (degrees-180 (* sign (+ deg
                               (/ min 60.0d0)
                               (/ sec 3600d0)))))
      (error "Non-sequence passed to dms->deg"))) 

(defun dms->deg (sign deg min sec)
  "convert SIGN DEG MIN SEC to decimal degrees"
  (degrees-180 (* sign (+ deg
			 (/ min 60.0d0)
			 (/ sec 3600d0)))))
    


;;
;; converts degrees to HMS, forcing degrees to lie in [0,360) beforehand,
;; and rounding seconds to 10 decimal places 
;;
(defun deg->hms (deg-dec)
  "return (VALUES HR MIN SEC) converted from decimal degrees"
  (let* ((hr-adj (/ (f-modulo deg-dec 360d0) 15.0d0) )
         (hr (floor hr-adj))
         (min (floor (* (- hr-adj hr) 60d0)))
         (sec (round-n (* 3600d0 (- hr-adj hr (/ min 60.0d0))) 10)))
    (if (>= sec 60.0d0)  
        (progn (setf sec (- sec 60.0d0))
               (setf min (+ min 1))
               (if (>= min 60) 
                   (progn (setf min (- min 60))
                          (setf hr (+ hr 1))
                          (if (>= hr 24) 
                              (setf hr (- hr 24)))))))                  
    (values hr min sec)))

(defun deg->hms-vec (deg-dec)
  "return (VECTOR HR MIN SEC) converted from decimal degrees"
  (multiple-value-bind (hr min sec) (deg->hms deg-dec)
    (vector hr min sec)))


;;
;; converts degrees to DMS, placing no constraints on input degrees because
;; what the user wants is left undefined; sec are rounded off, and
;; min and sec are boosted up appropriately if sec rolls over
;;
(defun deg->dms (deg-dec)
  "return (VALUES DEG MIN SEC) converted from decimal degrees"
  (let* ((sign (if (< 0 deg-dec) 1 -1))
         (deg-abs (abs deg-dec))
         (deg (floor deg-abs))
         (min (floor (* (- deg-abs deg) 60.d0)))
         (sec (round-n (* 3600.d0 (- deg-abs deg (/ min 60.d0))) 10 )))
    (if (>= sec 60.0d0)  ;; if rounding took us, say, from 59.9999999 to 60.0
        (progn (setf sec (- sec 60.0d0))
               (setf min (+ min 1))
               (if (>= min 60) 
                   (progn (setf min (- min 60))
                          (setf deg (+ deg 1))))));;deg can cross 180 or 360
    (values sign deg min sec)))


(defun deg->dms-vec (deg-dec)
  "return (VECTOR SIGN DEG MIN SEC) converted from decimal degrees"
  (multiple-value-bind (sign deg min sec) (deg->dms deg-dec)
    (vector sign deg min sec)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; routines for converting strings, and their helpers

(defmacro is-char-numeric (c)
  `(and (char>= ,c #\0) (char<= ,c #\9)))

(defmacro char-to-int (c)
  `(- (char-code ,c) (char-code #\0)))

;; convert a string to a number (integer or double, but no support for exponential
;;   notation) -- must be a string of the form 12313 or 12334. or .23434 or 234.234
(defun string->number (s &optional (s-start nil) (s-end nil))
  (declare (type string s))
  (let ((start (if s-start s-start 0))
	(end   (if s-end s-end (length s)))
	(before-decimal 0)
	(after-decimal 0)
	(divisor 1)
	(c #\x)
	(decimal nil) )
    (declare (type fixnum start end)
	     (type integer before-decimal after-decimal divisor)
	     (type character c)
	     (type (or NIL T) decimal))
  (do ((i start (+ i 1)))
      ((= i end) T)
    (setf c (aref s i))
    (cond ((char= c #\.) (setf decimal t))
	  ((not decimal)
	   (setf before-decimal (+ (* 10 before-decimal) (char-to-int c))))
	  (T
	   (setf divisor (* divisor 10))
	   (setf after-decimal  (+ (* 10 after-decimal) (char-to-int c))))))
  (if (not decimal)
      before-decimal
    (+ (* 1.d0 before-decimal) (/ after-decimal divisor)))))
       
	

;; munches a number at v[0] in string str; returns the number
;; and increments v[0] appropriately; throws err-func if there is more
;; than one decimal point, or if we are starting on a non-number
(defun munch-number (str n v err-func)
  (if (> (aref v 0) n) (funcall err-func)) ;; no number to parse
  (let* ((start (aref v 0))
         (end (- start 1))
         (c (aref str start))
         (have-decimal nil))
    (if (not (or (char= c #\:) (is-char-numeric c)))
        (funcall err-func))
    (loop
      (if (and (< end n) 
	       (progn (setf c (aref str (+ end 1)))
		      (or (is-char-numeric c) (char= c #\.))))
	  (progn
	   (if (char= c #\.) 
	       (if have-decimal (funcall err-func)
		 (setf have-decimal t)))
	   (setf end (+ end 1)))
	(return)))
    (setf end (+ end 1))
    (setf (aref v 0) end)
    (string->number str start end )))


(defmacro char-is-whitespace (c)
  `(or (char= ,c #\space) (char= ,c #\tab)))

(defun munch-whitespace (str n v)
  (let* ((start (aref v 0))
         (end (- start 1)))
    (loop
     (if (and (< end n) 
	      (char-is-whitespace (aref str (+ end 1))))
	   (setf end (+ end 1))
	   (return)))
      (setf end (+ end 1))
      (setf (aref v 0) end)))


;; munch 
;; [optional whitespace] [one or fewer in a list of separators] 
;; [optional whitespace]
(defun munch-separator (str n v sep-list)
  (munch-whitespace str n v)
  (if (and (<= (aref v 0) n)
           (member (aref str (aref v 0)) sep-list))
      (progn
        (setf (aref v 0) (+ 1 (aref v 0)))
        (munch-whitespace str n v))))


;; munch [optional whitespace] [optional sign] [optional whitespace]
;; returning +/- 1
(defun munch-sign (str n v)
  (munch-whitespace str n v)
  (let ((sign 1.0)
        (c #\a))
    (if  (<= (aref v 0) n)
         (progn (setf c (aref str (aref v 0)))
                (if (or (char= c #\- ) (char= c #\+))
                    (progn
                      (if (char= c #\- ) (setf sign -1.0))
                      (setf (aref v 0) (+ 1 (aref v 0)))
                      (munch-whitespace str n v)))))
    sign))

;; check that there is nothing to the end except whitespace; if
;; there is other stuff, call err-func
(defun whitespace-to-end (str n v err-func)
  (do ((i (aref v 0) (+ i 1)))
      ((> i n))
    (if (not (char-is-whitespace (aref str i)))
        (funcall err-func))))


;; returns #t or #f if only if the tail end of the list (from v[0])
;; consists of [whitespace][one or fewer from these-list][whitespace]
(defun only-these (str n v these-list)
  (let ((num-these 0)
        (c #\a))
    (do ((i (aref v 0) (+ i 1)))
        ((> i n))
      (setf c (aref str i))
      (if (member c these-list)
          (setf num-these (+ num-these 1))
          (if (not (char-is-whitespace c))   ;; if non-white & not in these-list
              (setf num-these 2))))        ;;   then bomb
    (if (< num-these 2) T NIL)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hms-string->deg (str &key (strict t))
  "Convert an HMS string to degrees; if STRICT is T, then demand
that HH,MM be integers and SS exist.  Otherwise, we can parse fractional
hours, no minutes or seconds, etc."
  (declare (type string str))
  (let ((h nil) (m nil) (s nil)
        (v (vector 0))
        (n (- (length str) 1))
        (bad-string-err 
         #'(lambda () (error (concatenate 'string "Bad HMS string in hms-string->hms: "
					  str)))))
    (munch-whitespace str n v)
    (setf h (munch-number str n v bad-string-err))
    (if (or (not (integerp h))  ;; if hours inexact, must not have m and s
            (only-these str n v '(#\h #\H)))
        (progn
          (munch-separator str n v '(#\h #\H))
          (whitespace-to-end str n v bad-string-err) )
        (progn
          (munch-separator str n v '(#\h #\H #\:))
          (setf m (munch-number str n v bad-string-err))
          (if (or (not (integerp m))
                  (only-these str n v '(#\m #\M)))
              (progn
                (munch-separator str n v '(#\m #\M))
                (whitespace-to-end str n v bad-string-err) )
              (progn
                (munch-separator str n v '(#\m #\M #\:))
                (setf s (munch-number str n v bad-string-err))
                (munch-separator str n v '(#\s #\S))))))

    (when (and strict
	       (not (integerp h))
	       (not (integerp m))
	       (not (realp s)))
      (error "STRICT is set, and HOUR=~A MIN=~A SEC=~A are not integer,integer,real"
	     h m s))
			   
    (setf h (or h 0)
	  m (or m 0)
	  s (or s 0))
    (degrees-360 (* 15 (+ h (/ m 60.0d0) (/ s 3600.0d0))))))
    
(defun hms-string->hms (str &key (strict t))
    "Convert a HMS string to (VALUES HOUR MIN SEC).  If STRICT is set, 
each of HOUR MIN SEC must be present, and an INTEGER INTEGER REAL"
  (deg->hms (hms-string->deg str :strict strict)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dms-string->deg (str &key (strict t))
    "Convert an DMS string to degrees; if STRICT is T, then demand
that DD,MM be integers and SS exist.  Otherwise, we can parse fractional
degrees, no minutes or seconds, etc."
  (declare (type string str))
  (let ((d nil) (m nil) (s nil)
        (v (vector 0))
        (sign 1.0d0)
        (n (- (length str) 1))
        (bad-string-err 
         (lambda () (error (concatenate 'string "Bad DMS string in dms-string->deg: "
                                   str)))))
    (setf sign (munch-sign str n v))
    (setf d (munch-number str n v bad-string-err))
    (if (or (not (integerp d))  ;; if degrees inexact, must not have m and s
            (only-these str n v '(#\d #\D)))
        (progn
          (munch-separator str n v '(#\d #\D))
          (whitespace-to-end str n v bad-string-err) )
        (progn
          (munch-separator str n v '(#\d #\D #\:))
          (setf m (munch-number str n v bad-string-err))
          (if (or (not (integerp m))
                  (only-these str n v '(#\m #\M)))
              (progn
                (munch-separator str n v '(#\m #\M))
                (whitespace-to-end str n v bad-string-err) )
              (progn
                (munch-separator str n v '(#\m #\M #\:))
                (setf s (munch-number str n v bad-string-err))
                (munch-separator str n v '(#\s #\S))))))

   (when (and strict
	       (not (integerp d))
	       (not (integerp m))
	       (not (realp s)))
      (error "STRICT is set, and DEG=~A MIN=~A SEC=~A are not integer,integer,real"
	     d m s))

    (setf d (or d 0)
	  m (or m 0)
	  s (or s 0))

    (degrees-180  (* sign (+ d (/ m 60.0d0) (/ s 3600.0d0))))))
    
(defun dms-string->dms (str &key (strict t))
  "Convert a DMS string to (VALUES DEG MIN SEC).  If STRICT is set, 
each of DEG MIN SEC must be present, and an INTEGER INTEGER REAL"
  (deg->dms (dms-string->deg str :strict strict)))



;; convert a number to a string; 
;; if a number has fewer than 2 digits before decimal point, pad with a 
;; leading "0" ; only works for 100>x>=0
(defun num->string (x)
  (if (>= x 10) 
      (format nil "~A" x)
    (format nil "0~A" x)))

;; same as above, but ensure that there are exactly n digits after decimal
;; require n>=1
(defun decnum->string (x n)
  (cond ((plusp n)
	 (if (>= x 10) 
	     (format nil "~,vF" n x)
	     (format nil "0~,vF" n x)))
	((zerop n)
	 (format nil "~2,'0D" (round x)))
	(t
	 (error "n=~A not a valid number of rounding digits" n))))
      
  

;; fix rounding problem (ie, avoid having 60 seconds)
(defun fix-dms (dms rounding-n)
  (setf (aref dms 3) (round-n (aref dms 3) rounding-n))
  (if (= (floor (aref dms 3)) 60.0d0)
      (progn (setf (aref dms 3) 0.0d0)
             (setf (aref dms 2) (+ (aref dms 2) 1))
             (if (= (aref dms 2) 60)
                 (progn (setf (aref dms 2) 0)
                        (setf (aref dms 1) (+ (aref dms 1) 1))
                        (if (= (aref dms 1) 360)
                            (setf (aref dms 1) 0))))))
  dms)



(defun fix-hms (hms rounding-n)
  (setf (aref hms 2) (round-n (aref hms 2) rounding-n))
  (if (= (floor (aref hms 2)) 60.0d0)
      (progn (setf (aref hms 2) 0.0d0)
             (setf (aref hms 1) (+ (aref hms 1) 1))
             (if (= (aref hms 1) 60)
                 (progn (setf (aref hms 1) 0)
                        (setf (aref hms 0) (+ (aref hms 0) 1))
                        (if (= (aref hms 0) 24)
                            (setf (aref hms 0) 0))))))
  hms)




(defparameter *default-dms-separators*  #(":" ":" ""))
(defun dms-vec->dms-string (dms &key (rounding 4)
				  (separator-strings *default-dms-separators*))
  (fix-dms dms rounding)
  (let ((sign (if (< (aref dms 0) 0) "-" "+"))
        (deg  (num->string (aref dms 1)))
        (min  (num->string (aref dms 2)))
        (sec (decnum->string  (aref dms 3) rounding )))
    (concatenate 'string  sign deg
		 (aref separator-strings 0)
		 min
		 (aref separator-strings 1)
		 sec
		 (aref separator-strings 2))))


(defun dms->dms-string (sign deg min sec
			&key (rounding 4)
			  (separator-strings *default-dms-separators*))
  (let ((dms (vector sign deg min sec)))
    (dms-vec->dms-string dms :rounding rounding :separator-strings separator-strings)))



(defparameter *default-hms-separators*  #(":" ":" ""))
(defun hms-vec->hms-string (hms &key (rounding 4)
			    (separator-strings *default-hms-separators*))
  (fix-hms hms rounding)
  (let ((hr   (num->string (aref hms 0)))
        (min  (num->string (aref hms 1)))
        (sec (decnum->string (aref hms 2) rounding )))
    (concatenate 'string hr
		 (aref separator-strings 0)
		 min
		 (aref separator-strings 1)
		 sec
		 (aref separator-strings 2))))


(defun hms->hms-string (hr min sec
			&key (rounding 4)
			  (separator-strings *default-hms-separators*))
  (let ((hms (vector hr min sec)))
    (hms-vec->hms-string hms :rounding rounding :separator-strings separator-strings)))





(defun deg->dms-string (deg &key (rounding 1)  (separator-strings #(":" ":" "")))
  (dms-vec->dms-string (deg->dms-vec deg) :separator-strings separator-strings
		   :rounding rounding))

(defun deg->hms-string (deg  &key (rounding 2)  (separator-strings #(":" ":" "")))
  (hms-vec->hms-string (deg->hms-vec deg) :separator-strings separator-strings
		   :rounding rounding))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; latex format ra/dec in apj form
(defun deg->apj-ra-string (deg &key  (rounding 2))
  "Format RA decimal degrees into a string of ApJ latex form"
  (let* ((s (deg->hms-string deg :rounding rounding
			     :separator-strings #("^{\\rm h}" "^{\\rm m}" "")))
	 (n (position #\. s))
	 (dollar "$"))
    (declare (type string s))
    (format nil
	    "~A~A~A~A~A"
	    dollar
	    (subseq s 0 n)
	    "$\\rlap{$^{\\rm s}$}.$"
	    (subseq s (1+ n))
	    dollar)))

(defun deg->apj-dec-string (deg &key  (rounding 1))
  "Format Dec decimal degrees into a string of ApJ latex form"
  (let* ((s (deg->dms-string deg :rounding rounding
			     :separator-strings #("^{\\circ}" "^{\\prime}" "")))
	 (n (position #\. s))
	 (dollar "$"))
    (declare (type string s))
    (format nil
	    "~A~A~A~A~A"
	    dollar
	    (subseq s 0 n)
	    "$\\rlap{$^{\\prime\\prime}$}.$"
	    (subseq s (1+ n))
	    dollar)))



(defun ra->deg  (ra &key (error-on-fail t))
  "Convert any style RA to decimal degrees - returns NIL on no valid conversion
if ERROR-ON-FAIL is NIL.

RA can be a real (deg), a sequence of [HH MM SS], or a string of HH:MM:SS HH.xx HH.MM.xx

Warning: string '12.5' is 12.5 hours, not 12.5 degrees, but float 12.5 is 12.5 degrees."
  (when (typep ra 'list)
    (setf ra (coerce ra 'vector)))
  (let ((ra-deg
	  (cond
	    ;; already degrees
	    ((realp ra)
	     (float ra 1d0))
	    ;; vector
	    ((and (vectorp ra) (= (length ra) 3))
	     (ignore-errors
	      (hms->deg (aref ra 0) (aref ra 1) (aref ra 2))))
	    ;; string HH:MM:SS
	    ((stringp ra)
	     (ignore-errors (hms-string->deg ra :strict nil)))
	    (t
	     nil))))
    (if (and (not ra-deg) error-on-fail)
	(error "Invalid RA ~S" ra))
    ra-deg))

(defun ra->ra-string (ra &key (rounding 2) (separator-strings *default-dms-separators*)
			 (error-on-fail t))
    "Convert any style RA to a string - returns NIL on no valid
conversion if ERROR-ON-FAIL is NIL.

RA can be a real (deg), a sequence of [HH MM SS], or a string of
HH:MM:SS, HH.xx, HH.MM.xx.

Warning: string '12.5' is 12.5 hours, not 12.5 degrees, but float 12.5
is 12.5 degrees."
  (let ((ra-deg (ra->deg ra :error-on-fail error-on-fail)))
    (when ra-deg
      (deg->hms-string ra-deg :rounding rounding :separator-strings separator-strings))))
     
    
(defun dec->deg  (dec &key (error-on-fail t))
      "Convert any style DEC to a double float - returns NIL on no valid conversion 
if ERROR-ON-FAIL is NIL.

DEC can be a real (deg), a sequence of [DD MM SS], or a string of DD:MM:SS, DD.xx, DD:MM.xx"
  (when (typep dec 'list)
    (setf dec (coerce dec 'vector)))
  (let ((dec-deg
	  (cond
	    ;; already degrees
	    ((realp dec)
	     (float dec 1d0))
	    ;; vector
	    ((and (vectorp dec) (= (length dec) 4))
	     (ignore-errors
	      (dms->deg (aref dec 0) (aref dec 1) (aref dec 2) (aref dec 3))))
	    ;; string HH:MM:SS
	    ((stringp dec)
	     (ignore-errors (dms-string->deg dec :strict nil)))
	    (t
	     nil))))
    (if (and (not dec-deg) error-on-fail)
	(error "Invalid DEC ~S" dec))
    dec-deg))
    

(defun dec->dec-string (dec &key (rounding 2) (separator-strings *default-dms-separators*)
			    (error-on-fail t))
        "Convert any style DEC to a double float - returns NIL on no valid conversion if
ERROR-ON-FAIL is not NIL.

DEC can be a real (deg), a sequence of [DD MM SS], or a string of DD:MM:SS, DD.xx, DD:MM.xx"
  (let ((dec-deg (dec->deg dec :error-on-fail error-on-fail)))
    (when dec-deg
      (deg->dms-string dec-deg :rounding rounding :separator-strings separator-strings))))
