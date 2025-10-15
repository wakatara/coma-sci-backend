

(in-package instrument-id)

(defun insert-standard-headers-for-fits-file (fits-file)
  "Insert standard headers into a fits file, including
    [full initial WCS]
    STDHEAD.OBJECT
    STDHEAD.OBJECT-TYPE
    STDHEAD.EXPTIME
    STDHEAD.STDFILTER
    STDHEAD.MJDSTART
    STDHEAD.MJDMID
    STDHEAD.UTSTART
    STDHEAD.INSTRUMENT
    STDHEAD.GAIN
    STDHEAD.TRIMSEC
    STDHEAD.DATASEC
    STDHEAD.STATSEC
    STDHEAD.CHIPID
"

  (cf:with-open-fits-file (fits-file ff :mode :io)
    (loop for i from 1 to (cf:fits-file-num-hdus ff)
	  do (cf:move-to-extension ff i)
	     (when (not (identify-instrument ff))
	       (error "Cannot IDENTIFY-INSTRUMENT for EXTENSION number ~A  in ~A"
		      i fits-file)))
    ;;
    (cf:move-to-extension ff 1) ;; back to start
    
    
    (flet ((write-single-and-multichip-headers (inst fff)
	     (cf:write-fits-header fff "STDHEAD.COMMENT"
				   "Standardized hdrs, from INSTRUMENT-ID")
	     (cf:write-fits-header fff "STDHEAD.OBJECT"
				   (get-object-for-instrument inst fff))
	     (cf:write-fits-header fff "STDHEAD.OBJECT-TYPE"
				   (get-object-type-for-instrument inst fff))
	     (cf:write-fits-header fff "STDHEAD.EXPTIME"
				   (get-exptime-for-instrument inst fff))
	     (cf:write-fits-header fff "STDHEAD.STDFILTER" 
				   (string (get-standard-filter-for-instrument inst fff))
				   :comment "Filter in standard form")
	     (cf:write-fits-header fff "STDHEAD.MJDSTART" 
				   (get-mjd-start-for-instrument inst fff))
	     (cf:write-fits-header fff "STDHEAD.MJDMID" 
				   (get-mjd-mid-for-instrument inst fff))
	     (cf:write-fits-header fff "STDHEAD.GAIN"
				   (get-gain-for-instrument inst fff
							    :extension nil))
	     (cf:write-fits-header fff "STDHEAD.UTSTART" 
				   (astro-time:mjd-to-ut-string
				    (get-mjd-start-for-instrument inst fff)))
	     (cf:write-fits-header fff "STDHEAD.INSTRUMENT" (string (type-of inst))))
	   ;;
	   (write-onechip-headers (inst fff)
	     (cf:write-fits-header fff "STDHEAD.TRIMSEC"
				   (let ((v (get-trimsec-for-instrument inst fff)))
				     (format nil "[~A:~A,~A:~A]" 
					     (aref v 0) (aref v 1) (aref v 2) (aref v 3)))
				   :comment "Image section to keep")
	     (cf:write-fits-header fff "STDHEAD.STATSEC"
				   (let ((v (get-statsec-for-instrument inst fff)))
				     (format nil "[~A:~A,~A:~A]" 
					     (aref v 0) (aref v 1) (aref v 2) (aref v 3)))
				   :comment "Image section for image statistics")
	     (cf:write-fits-header fff "STDHEAD.DATASEC"
				   (let ((v (get-trimsec-for-instrument inst fff)))
				     (format nil "[~A:~A,~A:~A]" 
					     (aref v 0) (aref v 1) (aref v 2) (aref v 3)))
				   :comment "Image section with image data")
	     (cf:write-fits-header fff "STDHEAD.CHIPID"
				   (get-chip-id-for-instrument inst fff))
	     (insert-initial-wcs-for-instrument inst fff)))
      
       (loop for i from 1 to (cf:fits-file-num-hdus ff)
	     do (cf:move-to-extension ff i)
		(let ((inst (identify-instrument ff)))
		  (write-single-and-multichip-headers inst ff)
		  (when (test-if-image-at-extension-for-fits ff :extension i)
		    (write-onechip-headers inst ff)))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; routines to manipulate standard headers from outside programs (like
;; IMRED package)

(defparameter *standard-headers*
  '((:exptime     "STDHEAD.EXPTIME")
    (:object      "STDHEAD.OBJECT")
    (:object-type "STDHEAD.OBJECT-TYPE")
    ;; "uj" "bj" "vj" "rc" "ic" "usdss" "gsdss" "rsdss" "isdss" "zsdss"
    (:filter      "STDHEAD.STDFILTER") 
    (:mjd-start   "STDHEAD.MJDSTART")
    (:mjd-mid     "STDHEAD.MJDMID")
    (:ut-start    "STDHEAD.UTSTART") ;; this is not accessed with a per-inst method
    (:instrument  "STDHEAD.INSTRUMENT")
    (:gain        "STDHEAD.GAIN")
    (:trimsec     "STDHEAD.TRIMSEC")  ;; these should be updated on trimming, and they
    (:datasec     "STDHEAD.DATASEC")  ;; will override the standard values
    (:statsec     "STDHEAD.STATSEC")
    (:chip-id     "STDHEAD.CHIPID")
    ;; some additional useful headers for use by other packages
    (:trimmed     "STDHEAD.TRIMMED")
    (:debiased    "STDHEAD.DEBIASED")
    (:flattened   "STDHEAD.FLATTENED")
    (:fringe-corrected   "STDHEAD.FRINGECORRECTED")
    ))
    

(defparameter *allowed-standard-filters*
  '("uj" "bj" "vj" "rc" "ic" "usdss" "gsdss" "rsdss" "isdss" "zsdss"
    "open" "unknown" "open-uh88tek" "gri" "wide"))


(defun set-standard-header (fits-file header val &key (extension 1))
  "Set a standard header in FITS-FILE, where HEADER is a keyword in 
 *STANDARD-HEADERS*"
  (when (not (assoc header *standard-headers*))
    (error "Header ~A is not one of ~{~S ~}" header (mapcar 'car *standard-headers*)))

  (when (eq header :filter)
    (when (not (member val *allowed-standard-filters* :test 'equalp))
      (error "STHEAD-FILTER = ~S not one of ~A" val *allowed-standard-filters*))
    (setf val (string val)))
  ;;
  (when (eq header :instrument)
    (cond ((or (symbolp val) (stringp val))
	   (let ((inst (make-instrument-for-name val)))
	     (when (not inst)
	       (error "STDHEAD.INSTRUMENT = ~S is not known." val))
	     (setf val (type-of inst))))
	  ((typep val 'instrument)
	   (setf val (type-of val)))
	  (t
	   (error "Invalid value ~S for STDHEAD.INSTRUMENT" VAL)))
    (setf val (string val)))
  ;;
  (cf:write-fits-header fits-file (second (assoc header *standard-headers*)) val
			:extension extension))


(defun get-standard-header (fits-file header  &key (extension 1))
  "Get a standard header in FITS-FILE, where HEADER is a keyword in 
 *STANDARD-HEADERS*"

  ;; when fits-file is NIL, it might be a dummy file intended to
  ;; get to a method that doesn't need the file, so just return NIL
  (when (not fits-file)
    (return-from get-standard-header nil))

  
  (when (not (assoc header *standard-headers*))
    (error "Header ~A is not one of ~{~S ~}" header (mapcar 'car *standard-headers*)))
  (let ((header-value
	  (cf:read-fits-header fits-file (second (assoc header *standard-headers*))
			       :extension extension)))
    ;;
    (when (and header-value (equalp header :filter))
      (if (member header-value *allowed-standard-filters* :test 'equalp)
	  (setf header-value (intern (string-upcase header-value) :keyword))
	  (error "Standard header for STDHEAD.STDFILTER = ~S is not a member of ~A"
		 header-value *allowed-standard-filters*)))
    ;;
    (when (and header-value (equalp header :instrument))
      (when (not (stringp header-value)) (error "STDHEAD.INSTRUMENT is not a string."))
      (let* ((instrument-obj
	       (or (make-instrument-for-name header-value)
		   (error "STDHEAD.INSTRUMENT = ~S is not a known instrument in the INSTRUMENT-ID package."
			  header-value)))
	     (instrument-symbol
	       (type-of instrument-obj)))
	(setf header-value instrument-symbol)))

    ;;
    header-value))
