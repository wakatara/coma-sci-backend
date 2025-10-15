
;; HSC reduced by their pipeline, into a projected chunk

(in-package instrument-id)

(defclass subaru-hypersuprime-cam-hscpipe (imaging-instrument onechip #+nil preprocessed)
  ((name :initform "Subaru HyperSuprimeCam HSCPIPE reduced")
   (aperture :initform 8.2)
   (observatory :initform "Subaru")
   ;; we don't have an example of how hscpipe fitting works
   #+nil (wcs-origin :initform "HSCPIPE")
   #+ nil (phot-calib-origin :initform "HSCPIPE")
   ))



  

(defun %hypersuprime-cam-hscpipe-identify-instrument (fits-file)
  (when (cf:read-fits-header fits-file "HSCPIPE_VERSION")
    (make-instance 'subaru-hypersuprime-cam-hscpipe)))

(%add-instrument-id-function '%hypersuprime-cam-hscpipe-identify-instrument)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-critical-headers-for-instrument
    ((inst subaru-hypersuprime-cam-hscpipe) fits-file)
  (declare (ignore inst fits-file))
  (remove-duplicates
   (append
    '("HSCPIPE_VERSION" "TIME-MID")
    (call-next-method))
   :test 'equalp))

(defmethod is-reduced-for-instrument ((inst subaru-hypersuprime-cam-hscpipe) fits-file)
  (declare (ignorable  inst fits-file))
  :subaru-hscpipe)

(defmethod get-standard-filter-for-instrument
    ((inst subaru-hypersuprime-cam-hscpipe) fits-file)
  (let ((filter (%gethead-or-error fits-file "FILTER")))
    (when (or (not filter) (not (stringp filter))  (< (length filter) 1))
      (error "FILTER keyword not found in image ~A" fits-file))
    (cond ((equalp filter "g") :gsdss)
	  ((equalp filter "r") :rsdss)
	  ((equalp filter "i") :isdss)
	  ((equalp filter "z") :zsdss)
	  ((equalp filter "y") :ysdss)
	  ;;
	  (t NIL))))

;; generic version OK
;(defmethod get-exptime-for-instrument ((inst subaru-hypersuprime-cam-hscpipe) fits-file) ... )

(defmethod get-object-for-instrument ((inst subaru-hypersuprime-cam-hscpipe) fits-file)
  nil) 

(defmethod get-object-type-for-instrument ((inst subaru-hypersuprime-cam-hscpipe) fits-file)
  :OBJECT)

(defmethod get-mjd-start-for-instrument ((inst subaru-hypersuprime-cam-hscpipe) fits-file)
  (- (get-mjd-mid-for-instrument inst fits-file)
     (* 0.5d0
	(/ (get-exptime-for-instrument inst fits-file) (* 24d0 3600d0)))))


(defmethod get-mjd-mid-for-instrument ((inst subaru-hypersuprime-cam-hscpipe) fits-file)
  (let ((time-mid (%gethead-or-error fits-file "TIME-MID")))
    (astro-time:parse-ut-date-and-time-string-to-mjd (string-right-trim "Z" time-mid))))


(defmethod get-gain-for-instrument
    ((inst subaru-hypersuprime-cam-hscpipe) fits-file &key extension)
  (declare (ignore extension))
  1.0) ;; not given, so we guess

;; this is the TRUE wcs, but it lives in the image extension
(defmethod get-initial-wcs-for-instrument
    ((inst subaru-hypersuprime-cam-hscpipe) fits-file &key (extension 2))
  (when (not (= extension 2))
    (error "Hypersuprime-cam HSCPIPE reduced data must have WCS at extension 2"))
  (cf:read-wcs fits-file :extension 2))



(defmethod get-chip-id-for-instrument
    ((inst subaru-hypersuprime-cam-hscpipe) fits-file &key (extension nil))
  (declare (ignore extension))
  0) ;; can't get the original chip, because chips are mixed


;; the image extension is in EXT1, not EXT0
(defmethod get-image-extension-for-onechip-instrument
    ((inst subaru-hypersuprime-cam-hscpipe)
     fits-file)
  (declare (ignore inst fits-file))
  2) ;;  HDU=2, equal to image.fits[1]

