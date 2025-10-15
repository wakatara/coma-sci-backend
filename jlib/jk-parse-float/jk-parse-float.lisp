

(defpackage #:jk-parse-float
  (:use #:common-lisp)
  (:export 
   #:parse-float
   #:validate-float-string))

(in-package jk-parse-float)



;; error codes that indicate that an float is incomplete but not necessarily 
;; invalid
(defparameter *incomplete-float-errors*
  '(premature-end-at-start premature-end-after-sign premature-end-after-exponent
    premature-end-after-exponent-sign premature-end-zero-mantissa-digits))


;; define some fast integers just a bit smaller than a fixnum
(deftype nice-int ()
  '(integer #.(ash most-negative-fixnum -1) #.(ash most-positive-fixnum -1)))
(deftype nice-uint ()
  '(integer 0 #.(ash most-positive-fixnum -1)))




(declaim (inline %whitespace-p))
(defun %whitespace-p (c)
  (declare (type character c))
  (member c '(#\space #\tab #\cr #\lf)))

(declaim (inline %skip-whitespace))
(defun %skip-whitespace (string &optional (start 0))
  (declare (type simple-string string)
	   (type nice-uint start)
	   (optimize (speed 3) (safety 1)))
  (loop for i of-type nice-uint from start below (length string)
	until (not (%whitespace-p (aref string i)))
	finally (return i)))
	
  

(declaim (inline analyze-float-string))

(defun analyze-float-string (num-string 
			     &key 
			       (ignore-leading-whitespace nil)
			       (start 0) (end nil)
			       (permitted-exponent-characters "eEDdsSlL"))
  "Analyzes NUM-STRING as a floating point number of the format:


 {+-}NNN{.}
 {+-}MMM.NNN
 {+-}MMM.NNN
 {+-}MMM.NNN[X]{+-}NNN

where NNN represents an arbitrary number of digits 1-10; MMM.NNN is a
run of digits with a decimal point and at least one digit before or
after decimal point; X is any allowed character representing an
exponent; [ABC] represents the mandatory presence of at least one of
ABC; and {ABC} represents the optional presence of ABC.

If SKIP-WHITESPACE is set, then any leading whitespace is ignored.

Returns
  (VALUES NIL-OR-ERR 
          SIGN 
          N-FIRST-MANT-DIGIT N-LAST-MANT-DIGIT 
          N-DECIMAL
          EXPT-CHAR EXPT-SIGN EXPT-INT
          N-LAST-CHAR)

Where 

   NIL-OR-ERR is NIL for a valid float, or a symbol 
     representing an error.
   SIGN is +-1, the sign in front of the float
   N-FIRST-MANT-DIGIT is the index of the first 
       non-zero digit  of the mantissa
   N-LAST-MANT-DIGIT is 1+ the index of the last digit 
       of the mantissa, with trailing zeros stripped
   N-DECIMAL-POINT is the index of the decimal point
       of the mantissa (inserting an imaginary decimal 
       point if none present
   EXPT-CHAR is the character that represents an exponent, 
      if present. It must be one of PERMITTED-EXPONENT-CHARACTERS.
   EXPT-SIGN is +-1, the sign of the exponent, or +1 if none.
   N-START-EXP is the index of the start of the exponent digits.
   N-END-EXP is 1+ the index of the end of the exponent digits.
   N-LAST-CHAR is the index of the last character, or 1+ the last 
      index of the string."


  (declare (type string num-string permitted-exponent-characters)
	   (type nice-uint start)
	   (type (or null nice-uint) end) 
	   (optimize speed))  

  (let* ((string (if (typep num-string 'simple-string)
		     num-string
		     (coerce num-string 'simple-string)))
	 (permitted-exponent-characters
	   (if (typep permitted-exponent-characters 'simple-string)
	       permitted-exponent-characters
	       (coerce permitted-exponent-characters 'simple-string)))
	 (sign +1)
	 (nstring (length string))
	 (ns (if end (min end nstring)  nstring))
	 (n (if ignore-leading-whitespace ;; current position in string
		(%skip-whitespace string start)
		start)) 
	 ;;
	 (have-decimal? nil)
	 (expt-char nil)
	 (expt-sign +1)
	 ;; start and end of digits before decimal
	 (n-start-before-dec 0)
	 (n-end-before-dec 0)
	 ;; number of digits in mantissa
	 (n-mantissa-digits 0)
	 ;; start and end of digits after decimal
	 (n-start-after-dec 0)
	 (n-end-after-dec 0)
	 ;; start and end of exponent 
	 (n-start-exp 0)
	 (n-end-exp 0))
	 
    (declare (type (unsigned-byte 26) n ns n-start-before-dec n-end-before-dec
		   n-start-after-dec n-end-after-dec n-start-exp n-end-exp
		   n-mantissa-digits)
	     (type simple-string string)
	     (type (integer -1 1) sign expt-sign)
	     (type nice-int))

    (block nil
      
      ;; first traverse string and note the location of its components
      ;;
      (tagbody 
	 (when (= n ns)  (return 'premature-end-at-start))
	 
	   ;; case of junk-allowed and an invalid char, so return NIL
	   (let ((c (aref string n)))
	     (when (and (not (member c '(#\- #\+ #\.)))
			(not (digit-char-p c)))
	       (return 'invalid-char-at-start)))
	   ;;
	   ;; test for sign
	   (cond ((char= (aref string n) #\+)
		  (incf n))
		 ((char= (aref string n) #\-)
		  (setf sign -1)
		  (incf n)))
	   (when (= n ns)
	     (return 'premature-end-after-sign))

	   ;; eat digits before decimal
	   (setf n-start-before-dec n
		 n-end-before-dec  n)
	   (loop
	     for i of-type nice-uint = n then (1+ i)
	     do
		(when (or (= i ns) (not (digit-char-p (aref string i))))
		  (setf n-end-before-dec i)
		  (setf n i)
		  (return))
		(incf n-mantissa-digits))
	   ;; if no more chars, go to end
	   (when (= n ns)
	     (setf n-start-after-dec ns
		   n-end-after-dec ns)
	     (go end))
	   ;; check if the next char is a decimal
	   (when (char= (aref string n) #\.)
	     (setf have-decimal? t)
	     (incf n))
	   ;; set up the start and of the stuff after the decimal
	   (setf n-start-after-dec n
		 n-end-after-dec n); (1+ n))
	   ;; if no decimal go to exponent
	   (when (not have-decimal?) (go parse-exponent))
	   ;; now get the digits after the decimal
	   (loop
	     for i of-type nice-uint = n then (1+ i)
	     do
		(when (or (= i ns) (not (digit-char-p (aref string i))))
		  (setf n-end-after-dec i)
		  (setf n i)
		  (return))
		(incf n-mantissa-digits))
	   (when (= n ns) 
	     (go end))
	   ;;
	 parse-exponent ;; begin parsing of exponent
	   ;; if not e,E,d,D then go to end
	   (when (not (position (aref string n) permitted-exponent-characters))
	     (go end))
	   (setf expt-char (aref string n)) ;; c has exponent type
	   (incf n)
	   ;;
	   ;; flag zero mantissa digits as a true error, because we have an exponent
	   ;; so it can't be an incomplete float
	   (when (zerop n-mantissa-digits)
	     (return 'zero-mantissa-digits))
	   ;;
	   (when (= n ns)
	     (return 'premature-end-after-exponent))
	   ;; grab the sign of the exponent
	   (cond ((char= (aref string n) #\+)
		  (incf n))
		 ((char= (aref string n) #\-)
		  (setf expt-sign -1)
		  (incf n)))
	   (when (= n ns)
	     (return 'premature-end-after-exponent-sign))

	   ;; if the next char is not a digit, then exponent is bogus
	   (when (not (digit-char-p (aref string n)))
	     (return 'invalid-char-in-exponent))

	   ;; and get the exponent
	   (loop
	     initially (setf n-start-exp n)
	     for i of-type nice-uint = n then (1+ i)
	     do
		(when (or (= i ns) (not (digit-char-p (aref string i))))
		  (setf n-end-exp i)
		  (setf n i)
		  (return)))
	 
	 end ;; completed string traversal

	 (when (zerop n-mantissa-digits)
	   (if (= n ns)
	       (return 'premature-end-zero-mantissa-digits)
	       (return 'zero-digits-in-mantissa)))
			


	(when (= (1+ n-start-before-dec) n-start-exp)
	  (return 'float-has-no-mantissa))

	 ) ;; end of tagbody
	
	;; now adjust n-start-before-dec and n-start-exp to get
	;; rid of un-necessary leading zeros
	(loop
	  with max = (1- n-end-before-dec)
	  while (and  (< n-start-before-dec max)
		      (char= #\0 (aref string n-start-before-dec)))
	  do (incf n-start-before-dec))
	(loop
	  with max = (1- n-end-exp)
	  while (and  (< n-start-exp max)
		      (char= #\0 (aref string n-start-exp)))
	  do (incf n-start-exp)) 
	;; adjust n-end-after-dec to get rid of all trailing zeros
	(loop
	  for j of-type nice-uint = (1- n-end-after-dec) then (1- j)
	  while (and  (>= j n-start-after-dec)
		      (char= #\0 (aref string j)))
	  do (decf n-end-after-dec))
	
	;; 
	(values nil ;; NIL=no error
		sign 
		n-start-before-dec n-end-after-dec ;; range of mant.digis
		(1- n-start-after-dec) ;; decimal location
		expt-char expt-sign n-start-exp n-end-exp
		n))))
 





;; internal function to parse mantissa, returning a floating point integer
;; so that 0.001234 becomes (values 1234d0 -6) ie 0.1234d-6
;; This parses only POSITIVE mantissas; the sign is parsed elsewhere.
(declaim (inline %parse-mantissa))
;;
(defun %parse-mantissa (mstring &optional (start 0) (end nil))
  (declare (type simple-string mstring)
	   (type fixnum start)
	   (type (or null fixnum) end)
	   (optimize (speed 3) (safety 1)))
  (loop 
    with end of-type fixnum = (or end (length mstring))
    ;; making accum a double float is faster than a 64 bit int
    with accum of-type double-float = 0d0
    with num-nonzero-digits of-type nice-uint = 0 ;; counter for nonzero digits
    with saw-dec = nil
    with expadj of-type (signed-byte 26) = 0
    for i of-type nice-uint from start below end
    until (= num-nonzero-digits 19) ;; don't get more precision than 19 digits
    for c = (aref mstring i) 
    for digit = (digit-char-p c)
    do (cond (digit
	      (setf accum (+ (* 10 accum) digit))
	      (when (not (zerop digit)) (incf num-nonzero-digits))
	      (when saw-dec (decf expadj)))	      
	     ((char= c #\.)
	      (setf saw-dec t))
	     (t ;; should not happen if this is called properly
	      (error "non-digit in %parse-mantissa of <~A>,~A,~A~%" mstring start end)))
    finally (return (values accum expadj))))





;; construct a double float from a mantissa (double-float) and an
;; exponent (integer).  This routine is used only for very small
;; absolute value floats, because the math loses a few digits of
;; precision (which don't exist for denormalized floats)
(declaim (inline %construct-double-float-with-scale-float
		 %construct-double-float))
(defun %construct-double-float-with-scale-float (sign mantissa exp)
  (declare (type (member -1 1) sign)
	   (type double-float mantissa)
	   (type nice-int exp)
	   (optimize (speed 3) (safety 1)))
  ;; express exponent in base 2, and split into integer
  ;; part and fractional part 'f'.  The integer part becomes the
  ;; base-2 exponent of the float, and 2^f is combined into the
  ;; mantissa.
  (multiple-value-bind (n-exp2 f-exp2)
      (floor (* exp #.(log 10d0 2))) 
    (declare (type nice-int n-exp2)
	     (type (double-float 0d0 1d0) f-exp2))
    (let ((mant2 (* mantissa (expt 2d0 f-exp2)))) ;; could use lookup table for expt
      ;; take the mantissa apart using decode float
      ;; and combine exponents - this seems to get us closer
      ;; to the full range of floats supported by Lisp
      (multiple-value-bind (mant-new exp-mant) ;; sign is always 1
	  (decode-float mant2)
	(* sign
	   (scale-float (* sign mant-new) (+ n-exp2 exp-mant)))))))
  



;; reasonably precise (to last decimal place) version of float 
;; conversion that uses. MANTISSA must be a double float
;; that is an integer. Otherwise loss of precision for small
;; numbers will result.
(defun %construct-double-float (sign mantissa exp)
  (declare (type (member -1 1) sign)
	   (type double-float mantissa)
	   (type nice-int exp)
	   (optimize (speed 3) (safety 1)))
  (cond ((zerop exp)
	 (* sign mantissa))
	((zerop mantissa)
	 0d0)
	;; double float math is accurate for large exponent
	((<= -300 exp 350) ;; expt is fast only for reasonable values of the exponent
	 (* sign mantissa (expt 10d0 exp)))
	((> exp 350) ;; very large exponents
	 (* sign mantissa 
	    float-utils:+double-float-positive-infinity+)) ;; infinity
	;; for small values, use the scale-float method, which loses 
	;; a few digits of precision, but these don't exist for 
	;; denormalized floats anyway
	(t ;; ie, (< exp -300)
	 (the double-float
	      (%construct-double-float-with-scale-float sign mantissa exp)))))

(eval-when (:load-toplevel :compile-toplevel)
  (when (not (boundp '+default-single-float-exponent-chars+))
    (defconstant +default-single-float-exponent-chars+ "eE")
    (defconstant +default-double-float-exponent-chars+ "dD")))



;; parse a small integer quickly - otherwise punt to (CL:PARSE-INTEGER ...)
(declaim (inline %parse-pos-integer))
(defun %parse-pos-integer (str start end)
  (declare (type simple-string str))
  (loop with n of-type (unsigned-byte 20) = 0
	for i from start below end
	do (setf n (+ (* n 10) (digit-char-p (aref str i))))
	;; when n is too big, punt to Lisp parse-integer
	when (> n 1000000) do (return (parse-integer str :start start :end end))
	finally (return n)))


  
(defun parse-float  (num-string &key 
				  (start 0) 
				  (end nil) 
				  (junk-allowed  nil)
				  (single-float-exponent-chars
				   +default-single-float-exponent-chars+)
				  (double-float-exponent-chars
				   +default-double-float-exponent-chars+)
				  ;; when no exponent given
				  (default-float-format 'double-float)
				  ;; coerce to this type if not NIL
				  (output-type 'double-float) 
				  (ignore-leading-whitespace t))
  "Parses NUM-STRING into a floating point number of the format:

 {+-}NNN{.}
 {+-}MMM.NNN
 {+-}MMM.NNN
 {+-}MMM.NNN[X]{+-}NNN

where NNN represents an arbitrary number of digits 1-10; MMM.NNN is a
run of digits with a decimal point and at least one digit before or
after decimal point; X is any allowed character representing an
exponent; [ABC] represents the mandatory presence of at least one of
ABC; and {ABC} represents the optional presence of ABC.

JUNK-ALLOWED causes NIL to be returned if the float is bad, and a
float to be returned if the leading characters are a float, but the
tail of NUM-STRING has junk, similar to PARSE-INTEGER.

SINGLE-FLOAT-EXPONENT-CHARS and DOUBLE-FLOAT-EXPONENT-CHARS are simple
strings that denote which characters are allowed to be exponents for
the corresponding numerical type.

DEFAULT-FLOAT-FORMAT determines how a number without an exponent is
parsed.  It must be 'CL:SINGLE-FLOAT or 'CL:DOUBLE-FLOAT.  It is
overriden by OUTPUT-TYPE if the latter is not NIL (below).

OUTPUT-TYPE determines the final output type if not NIL.  It must be
'CL:SINGLE-FLOAT or 'CL:DOUBLE-FLOAT or NIL."

  (declare (type nice-uint start)
	   (type string num-string)
	   (type (or nice-uint null) end)
	   (type simple-string single-float-exponent-chars 
		        double-float-exponent-chars)
	   (type (member cl:single-float cl:double-float)
		 default-float-format)
	   (type (member nil cl:single-float cl:double-float)
		 output-type)
	   (optimize (speed 3) (safety 1)))

  (let* ((permitted-exponent-chars ;; build the general exponent char string
	   ;; without consing
	   (if (and (eq single-float-exponent-chars
		       +default-single-float-exponent-chars+)
		   (eq double-float-exponent-chars
		       +default-double-float-exponent-chars+))
	      "eEdD"
	      (concatenate 'string single-float-exponent-chars 
			   double-float-exponent-chars)))
	;; make into a simple string
	(%num-string (if (simple-string-p num-string) 
			 num-string
			 (coerce num-string 'simple-string))))

    (declare (type simple-string permitted-exponent-chars %num-string))

		    
    (multiple-value-bind
	  (err sign n0-mant n1-mant n-dec 
	   expt-char expt-sign n-start-exp n-end-exp n-end)
	(analyze-float-string  
	 %num-string 
	 :start start :end end 
	 :ignore-leading-whitespace ignore-leading-whitespace
	 :permitted-exponent-characters permitted-exponent-chars)
      (declare (type (integer -1 1) sign expt-sign)
	       (type (or null character) expt-char)
	       (type (or null nice-uint)
		     n0-mant n1-mant n-dec n-start-exp n-end-exp n-end))
      (cond ((and err junk-allowed)
	     nil)
	    (err
	     (error "Error ~A in parse-float of <~A>" err %num-string))
	    ;; can't be junk at end of string, if (NOT JUNK-ALLOWED)
	    ((and (not junk-allowed)
		  (not (or (= n-end (length %num-string))
			   (eql n-end end)
			   ;; all remaining chars must be spaces
			   (loop for i of-type nice-uint from n-end
				   below (or end (length %num-string))
				 when (not 
				       (%whitespace-p (aref %num-string i)))
				   do (return nil) ;; bad
				 finally (return t)))))
	       (error "Junk in parse-float at end of <~A>" %num-string))
	    (t 
	     (multiple-value-bind (mantissa exp-adj)
		 (%parse-mantissa %num-string n0-mant n1-mant)
	       (declare (type double-float mantissa)
			(type (signed-byte 20) exp-adj))
	       (let* ((exponent (if expt-char
				    (%parse-pos-integer 
				     %num-string n-start-exp n-end)
				    0))
		      (n-exp (+ (* expt-sign exponent) exp-adj))
		      (fdbl (the double-float
				 (%construct-double-float sign mantissa n-exp)))
		      (retval
			(cond ((eq output-type 'double-float)
			       fdbl) ;; it's already a double-float 
			      ((eq output-type 'single-float)
			       (float fdbl 1.0))
			      ((not expt-char)
			       (if (eq default-float-format 'single-float) 
				   (float fdbl 1.0)
				   fdbl)) ;; it's already a double-float
			      ((find expt-char single-float-exponent-chars)
			       (float fdbl 1.0))
			      (t ;; expt-char must be in
			       ;; double-float-exponent-characters
			       fdbl))))
		 (declare (type fixnum exponent n-exp)
			  (type double-float fdbl)
			  (type (or double-float single-float) retval))
		 (values retval n-end)))))))) 


			    
	       
	       
(defun validate-float-string (num-string  
			      &key 
				(ignore-leading-whitespace nil)
				(start 0) (end nil)
				(permitted-exponent-characters "eEdD"))
  "Analyze a NUM-STRING to see whether if represents a valid floating point number.
See documentation to PARSE-FLOAT.

Returns (VALUES VALID INCOMPLETE ERROR-SYMBOL EXPT-CHAR INDEX-OF-LAST-CHAR)

Where 
   VALID  (T or NIL) is whether this is a valid float
   INCOMPLETE (T or NIL) is whether this string could be the first part of a float
   ERROR-SYMBOL is a symbol describing the error
   EXPT-CHAR is the character for an exponent, if any
   INDEX-OF-LAST-CHAR is next character after the parsing stopped, and may be beyond
     the end of the string."

  (multiple-value-bind
	(err sign n0-mant n1-mant n-dec 
	 expt-char expt-sign n-start-exp n-end-exp n-end)
      (analyze-float-string  
       num-string 
       :start start :end end 
       :ignore-leading-whitespace ignore-leading-whitespace
       :permitted-exponent-characters permitted-exponent-characters)
    (values (not err)
	    ;; it is incomplete if this is one of the *INCOMPLETE-FLOAT-ERRORS*
	    (if (find err *incomplete-float-errors*)
		t nil)
	    err
	    expt-char 
	    n-end)))
	    
       
	     

