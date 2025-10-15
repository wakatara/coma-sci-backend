;; string utilities
(defpackage #:string-utils
  (:use #:common-lisp)
  (:export 
   #:split-string
   #:find-first-substring
   #:find-all-substrings
   #:replace-substring
   #:string-starts-with
   #:string-ends-with
   #:string-contains

   #:split-string-into-words

   #:fuzzy-match
   ))


(in-package string-utils)

(defun split-str-helper (seq pred &key (start 0) end key strict)
  "Return a list of subseq's of SEQ, split on predicate PRED.
Start from START, end with END.  If STRICT is non-nil, collect
zero-length subsequences too.
  (split-seq SEQ PRED &key (start 0) end key strict)"
  (declare (optimize speed)) 
  (declare (type string seq)
	   (type (function (t) t) pred) (fixnum start))
  (loop :for st0 = (if strict start
                       (position-if-not pred seq :start start
                                        :end end :key key))
        :then (if strict (if st1 (1+ st1))
                  (position-if-not pred seq :start (or st1 st0)
                                   :end end :key key))
        :with st1 = 0 :while (and st0 st1) :do
        (setq st1 (position-if pred seq :start st0 :end end :key key))
        :collect (subseq seq st0 st1)))

(defun split-string (str chars &key  (start 0) end key strict trim-chars)
  "Split the simple string on chars.  If STRICT is set, return zero-length subsequences.
If TRIM-CHARS (string) is set then STRING-TRIM the output strings by these chars."
  (declare (string str) (sequence chars) (sequence trim-chars))
  (let ((output-list
	  (split-str-helper str  (lambda (ch) (declare (character ch)) (find ch chars))
			    :start start :end end :key key :strict strict)))
    (if trim-chars
	(mapcar (lambda (str) (string-trim trim-chars str)) output-list)
	output-list)))





;; return location of first substring in string, or nil
;; (remarkably, these are all the declarations cmucl needs to compile
;; the whole thing into ~55 machine ops!)
;; start and end are the bounds in str within which to search
;; where 'end' is (length str) by default.
;; eg (find-first-substring "foo" "foobar" 0 2) does NOT find foo
;; because this searches in foobar[0..2]
(defun find-first-substring (substr str &optional (start 0) (end nil))
  (declare (string str substr)
	   (type fixnum start)
	   (type (or null fixnum) end)
	   (optimize (speed 3) (safety 3)))
  (let ((ns (length str))
	(nss (length substr))
	(match nil)
	(c1 (aref substr 0)))
    (if (not end) (setf end ns))
    (if (> nss end)
	nil
	(progn
	  (if (or (< start 0) (> start (+ -1 ns)) (< end 0) (> end ns))
	      (error "Bad start or end in find-first-substring"))
	  
	  (loop for i from start upto (- end nss) do
	    (if (char= c1 (aref str i))
		(progn
		  (setf match i)
		  (loop with ii = i
			for j from 1 upto (- nss 1) do
			  (incf ii)
			  (if (not (char= (aref str ii) (aref substr j)))
			      (setf match nil)))))
	    (if match (return match)))))
	match))


;; return list of all starting indices of substr in str
(defun find-all-substrings (substr str &optional (start 0) (end nil))
  (declare (string str substr)
	   (type fixnum start)
	   (type (or null fixnum) end)
	   (optimize (speed 3) (safety 0)))
  (do ((outlist nil) (j start))
      ((not j) (nreverse outlist))
    (declare (type (or null fixnum) j))
    (setf j (find-first-substring substr str j end))
    (if j (progn (push j outlist)
		 (setf j (+ j (length substr)))))))

(defun replace-substring
  (substr replacement-str str &key (start 0) (end nil) (replace-all nil))
  (declare (string str replacement-str substr)
	   (type fixnum start)
	   (type (or null fixnum) end)
	   (optimize (speed 2) (safety 3)))
  (locally
   (declare (optimize (speed 3) (safety 1)))
   (let ((n (find-first-substring substr str start end)))
     (declare (type (or null fixnum) n))
     (if (not n) ;; if no element, just duplicate original string
	 (copy-seq str)
       (let ((s (make-string (+ (length str)
				(- (length replacement-str)
				   (length substr)))))
	     (j 0))
	 (loop for i from 0 upto (+ -1 n) do
	       (setf (aref s j) (aref str i))
	       (incf j))
	 (loop for i from 0 upto (- (length replacement-str) 1) do
	       (setf (aref s j) (aref replacement-str i))
	       (incf j))
	 (loop for i from (+ n (length substr)) upto (- (length str) 1) do
	       (setf (aref s j) (aref str i))
	       (incf j))
	 (if (or (not replace-all)
		 (>= (+ n (length replacement-str))
		      (length s)))
	     s
	     (replace-substring substr replacement-str s
				:start (+ n (length replacement-str))
				:replace-all t))
	 )))))






(defun string-starts-with (string start &key (test 'string=))
  "Returns T if STRING starts with START, using TEST STRING= by default"
  (declare (type string string start))
  (when (>= (length string) (length start))
    (funcall test string start :end1 (length start))))

(defun string-ends-with (string end &key (test 'string=))
  "Returns T if STRING ends with END, using TEST STRING= by default"
  (declare (type string string end))
  (when (>= (length string) (length end))
    (funcall test  end string :start2 (- (length string) (length end)))))

(defun string-contains (string substring  &key (test 'char=))
  "Returns T if STRING contains SUBSTRING, using character test TEST"
  (declare (type string string substring))
  (not (not (search substring string :test test))))






(defun split-string-into-words
    (text &key
	    (punctuation-test
	     (lambda (c) (declare (type character c)
				  (optimize speed))
	       (find c ",.;:\"?!#$%&*()[]-'")))
	    ;;
	    (anywhere-word-char-test 
	     (lambda (c) (declare (type character c)
				  (optimize speed))
	       (alphanumericp c)))
	    ;;
	    (inside-word-char-test 
	     (lambda (c) (declare (type character c)
				  (optimize speed))
	       (find c "'-")))
	    ;;
	    (max-word-length 1000))

"Split up a string TEXT into a list of words.  Not super-efficient, but good
enough.  

PUNCTUATION-TEST is a function of a one char that returns non-NIL if a
  character is a punctuation character to keep as a separate string.

ANYWHERE-WORD-CHAR-TEST is a function of one char that returns non-NIL if 
  character is a valid component of word at any location

INSIDE-WORD-CHAR-TEST is a function that tests for characters
  that are valid inside a word only (eg ' and -), when surrounded
  by an ANYWHERE-WORD-CHAR

All other characters are taken to be whitespace."

  
  (declare (type string text))

  (let ((word-accum (make-string max-word-length)) ;; current word
	(i-accum 0) ;; counter for how many chars in current word
	(outlist nil))

    (flet ((grab-word ()
	     (when (plusp i-accum)
	       (push (subseq word-accum 0 i-accum) outlist)
	       (setf i-accum 0)))
	   ;;
	   (collect-word-char (c)
	     (setf (aref word-accum i-accum) c)
	     (if (= i-accum max-word-length)
		 (error "Word longer than MAX-WORD-LENGTH")
	     (incf i-accum))))
      
      (loop with nmax = (1- (length text))
	    with in-word = nil
	    for c across text
	    for i from 1
	    for cnext = (if (<= i nmax) (aref text i))
	    do (cond
		 ;; this char is good anywhere in a word
		 ((funcall anywhere-word-char-test c)
		  (collect-word-char c)
		  (setf in-word t))
		 ;; this is a valid char INSIDE this word
		 ((and in-word  
		       (funcall inside-word-char-test c)
		       cnext
		       (funcall anywhere-word-char-test cnext))
		  (collect-word-char c))
		 ;; or separate punctuation
		 ((funcall punctuation-test c)
		  (setf in-word nil)
		  (progn (grab-word)
			 (push (string c) outlist)))

		 (t ;; all others are whitespace
		  (setf in-word nil)
		  (grab-word)))
	       
	    finally
	       (grab-word)
	       (return (reverse outlist))))))



(defun fuzzy-match (string1 string2 &key (start-penalty 0.9)
			     (use :all))
  "Returns a number between 0.0 and 1.0 if STRING1 resembles STRING2, based
on having case-insensitive characters at or near the same location.

USE tells the routine whether to use :ALL characters, or just
:ALPHABETIC.

START-PENALTY is the extra multiplicative penalty for not starting
with correct character.  0 means none; 1 means total (returns 0.0 if
strings don't start with same character)."
  (declare (type string string1 string2)
	   (type (member use :all :alphabetic))
	   (type (real 0.0 1.0) start-penalty))
  (let* ((str1 (if (eq use :all)
		   string1
		   (remove-if-not #'alpha-char-p string1)))
	 (str2 (if (eq use :all)
		   string2
		   (remove-if-not #'alpha-char-p string2))))
  ;; take average of asymmetric version of code
    (* 0.5
       (+
	(%fuzzy-match/asymmetric str1 str2 :start-penalty start-penalty)
	(%fuzzy-match/asymmetric str2 str1 :start-penalty start-penalty)))))
 
;; asymmetric version - typpically
;; (%strings-alike-p/asymmetric s1 s2) != (%strings-alike-p/asymmetric s2 s1)
(defun %fuzzy-match/asymmetric  (str1 str2 &key (start-penalty 0.9))
  (declare (type string str1 str2)
	   (type (real 0.0 1.0) start-penalty))
	   
  (let* ((n1 (length str1))
	 (n2 (length str2)))
    ;; zero strings produce a zero match
    (cond
      ((or (zerop (length str1))
	   (zerop (length str2)))
       0.0)
      (t
       (let* ((start-penalty-multiplier
		(if (char-equal (aref str1 0) (aref str2 0))
		    1.0
		    (- 1.0 (float start-penalty 1.0))))
	      ;; penalty for mis-matched length
	      ;; eg (10 10)=>1.0,  (10 8)=>0.71, (5 3)=>0.46
	      (length-penalty-multiplier
		(- 1.0
		(expt
		 (/ (abs (- n1 n2))
		    (max n1 n2))
		 1.5))))
		 
      (labels ((charat2 (c k) ;; is char at position k of str2 c?
		 (and (>= k 0) (< k n2) (char-equal c (aref str2 k))))
	       ;; how strongly does character C appear in STR2 at
	       ;; fractional position P?  1.0 for exactly there, 0.8 for
	       ;; next-to, 0.25 for 2 chars over, 0.125 for 3 chars over
	       (match-at (c p)
		 (declare (type (single-float 0.0 1.0) p))
		 (let ((k (if (= n2 1)
			      0
			      (round (* p (1- n2))))))
		   (cond ((charat2 c k)
			  1.0)
			 ((or (charat2 c (- k 1))
			      (charat2 c (+ k 1)))
			  0.8) ;; simple transpositions favored
			 ((or (charat2 c (- k 2))
			      (charat2 c (+ k 2)))
			  0.25)
			 ((or (charat2 c (- k 3))
			      (charat2 c (+ k 3)))
			  0.125)
			 (t 0.0)))))

	(let ((raw-match 
		(loop for i below n1
		      for p = (if (= n1 1)
				  0.0
				  (/ (float i) (1- n1)))
		      for score = (match-at (aref str1 i) p)
		      sum score
			of-type single-float)))
	  (* start-penalty-multiplier
	     length-penalty-multiplier
	     (expt (/ raw-match (max n1 n2)) 1))
	  )))))))

      
      
  
	  
