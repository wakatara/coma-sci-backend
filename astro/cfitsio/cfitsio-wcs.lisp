


#|

Note that FITS has pixels starting at (1,1) not (0,0) so that
all pixel values must have -1 added to them to correspond to
array indices.

2011-11-02 - We now use WCS package for wcs conversions, so this file
contains only reading and writing of wcs.  

|#




(in-package cfitsio)

#|

required headers for this to work are the following


CTYPE1  = 'RA---TAN'           / WCS Coordinate type 
CTYPE2  = 'DEC--TAN'           / WCS Coordinate type 
CRVAL1  =      167.33354166667 / WCS Ref value (RA in decimal degrees) 
CRVAL2  =      5.1438888888889 / WCS Ref value (DEC in decimal degrees) 
CRPIX1  =     2089.12560385517 / WCS Coordinate reference pixel  
CRPIX2  =      4061.7257269967 / WCS Coordinate reference pixel 
CD1_1   =  5.6982401266804E-05 / WCS Coordinate scale matrix  
CD1_2   = -1.7078929065786E-07 / WCS Coordinate scale matrix  
CD2_1   = -1.9414308811087E-08 / WCS Coordinate scale matrix  
CD2_2   = -5.6926540188930E-05 / WCS Coordinate scale matrix

the following wcs is also accepted, but is converted into the
CD1_1 type above:

CTYPE1  = 'RA---TAN'
CRVAL1  =        227.988171482 
CRPIX1  =             511.0000 
CDELT1  =         -0.000176265 
CROTA1  =         -0.198976305 
CTYPE2  = 'DEC--TAN' 
CRVAL2  =         67.384332889 
CRPIX2  =             511.0000 
CDELT2  =          0.000176253 
CROTA2  =         -0.198976305       

note that EQUINOX is also searched for, and is assumed 2000 if
not specified

A Linear WCS is also supported,
CTYPE1  = 'LINEAR'
CRVAL1  =        1.0
CRPIX1  =        511.0000 
CDELT1  =       0.6  / Angstroms per pix


|#

(defun %shift-wcs-for-fp (wcs fp)
  "modify a WCS-1D or WCS-2D using the FP first pixel vector to account for it
not being the first pixel of the image"
  (declare (type (or wcs-1d wcs-2d) wcs)
	   (type vector fp))
  (let ((new-wcs (copy-structure wcs)))
    (cond ((typep wcs 'wcs-1d)
	   (incf (wcs-1d-crpix1 new-wcs)
		 (- 
		  (- (float (aref fp 0) 1d0) 1d0))))
	  ;;
	  ((typep wcs 'wcs-2d)
	   (incf (wcs-2d-crpix2 new-wcs)  ;; crpix2 is Y and is FP[1]
		 (- 
		  (- (float (aref fp 1) 1d0) 1d0)))
	   (incf (wcs-2d-crpix1 new-wcs)  ;; crpix2 is X and is FP[0]
		 (- 
		  (- (float (aref fp 0) 1d0) 1d0)))))
	   ;;
    new-wcs))





;; get the generic info for a 2d trying to respect CDi_j, CROTA, CDELT, PC00m00n
(defun get-generic-2d-wcs-from-fits-file (ff wcs)
  (declare (type fits-file ff)
	   (type wcs-2d wcs))
  (let ((ctype1 (read-fits-header ff "CTYPE1"))
	(ctype2 (read-fits-header ff "CTYPE2"))
	(crval1 (read-fits-header ff "CRVAL1"))
	(crval2 (read-fits-header ff "CRVAL2"))
	;;
	(crpix1 (read-fits-header ff "CRPIX1"))
	(crpix2 (read-fits-header ff "CRPIX2"))
	(cd1_1 (read-fits-header ff  "CD1_1"))
	(cd1_2 (read-fits-header ff  "CD1_2"))
	(cd2_1 (read-fits-header ff  "CD2_1"))
	(cd2_2 (read-fits-header ff  "CD2_2"))
	;;
	(equinox (or (read-fits-header ff  "EQUINOX")
		     2000d0))
	;;
	;; grab alternative wcs representation too
	(cdelt1 (read-fits-header ff  "CDELT1"))
	(cdelt2 (read-fits-header ff  "CDELT2"))
	;; these are stupid and ancient
	(pc001001 (read-fits-header ff "PC001001"))
	(pc001002 (read-fits-header ff "PC001002"))
	(pc002001 (read-fits-header ff "PC002001"))
	(pc002002 (read-fits-header ff "PC002002"))
	;;
	(crota1 (read-fits-header ff  "CROTA1")) 
	(crota2 (read-fits-header ff  "CROTA2"))
	(crota 0d0) ;; the final rotation angle
	;; are radec in reversed order?
	(radec-reversed nil))

    (setf radec-reversed
	  (and (equalp (ignore-errors (subseq ctype2 0 3)) "RA-")
	       (equalp (ignore-errors (subseq ctype1 0 4)) "DEC-")))
	

    ;; we need at least 2 terms of the CD matrix, on a diagonal
    ;; from each other - otherwise, degenerate matrix
    (cond ((and cd1_1 cd2_2) ;; diagonal terms are OK, so fix off-diag
	   (setf cd1_2 (or cd1_2 0d0))
	   (setf cd2_1 (or cd2_1 0d0)))
	  ((and cd1_2 cd2_1) ;; off diag OK, so fix diag
	   (setf cd1_1 (or cd1_1 0d0))
	   (setf cd2_2 (or cd2_2 0d0)))
	  ((or cd1_1 cd1_2 cd2_1 cd2_2) ;; screwy CD matrix!
	   (setf cd1_2 (or cd1_2 0d0))
	   (setf cd2_1 (or cd2_1 0d0))
	   (setf cd1_1 (or cd1_1 0d0))
	   (setf cd2_2 (or cd2_2 0d0))))

    
    ;; now convert alternative rep if necessary and possible
    (when (and (every #'realp (list crval1 crval2 crpix1 crpix2))
	       (notany #'realp (list cd1_1 cd1_2 cd2_1 cd2_2))
	       ;; require only crota1
	       (every #'realp (list cdelt1 cdelt2))
	       ;;
	       ;; if crota1,2 present, must be equal, and we use
	       ;; crota1 - Mink's wcstools does this.
	       (or (and (not crota1) (not crota2)) ;; having neither crota is OK
		   (not (and crota1 crota2))       ;; one but not both is OK
		   (eql crota1 crota2))            ;; both being equal is OK
	       ;;
	       ;; may NOT have both crota and PC00m00n present
	       (not (and (or crota1 crota2)
			 (or pc001001 pc001002 pc002001 pc002002))))
      ;; make pc00m00n matrix diagonal if undefined
      (setf pc001001 (or pc001001 1d0))
      (setf pc002002 (or pc002002 1d0))
      (setf pc001002 (or pc001002 0d0))
      (setf pc002001 (or pc002001 0d0))
      ;; and set crota to be THE angle
      (setf crota (or crota1 crota2))) 
      
      ;;
      (when (not cd1_1)
	(cond 
	  ;; either the crota method
	  (crota
	       (let ((cosr (cos (* crota 0.017453292519943295d0)))
		     (sinr (sin (* crota 0.017453292519943295d0))))
		 (setf cd1_1 (* pc001001 cosr cdelt1))
		 (setf cd1_2 (* pc001002 -1d0 sinr cdelt2))
		 (setf cd2_1 (* pc002001 sinr cdelt1))
		 (setf cd2_2 (* pc002002 cosr cdelt2))))
	  ;; or the PC00m00n method
	  (t ;; all the PCs defined above
	   (setf cd1_1 (* pc001001 cdelt1))
	   (setf cd1_2 (* pc001002 cdelt2))
	   (setf cd2_1 (* cdelt1))
	   (setf cd2_2 (*  cdelt2)))))
    ;;
    ;; if ra,dec dimensions are reversed, then swap dimensions back by
    ;; reversing rows of CD matrix -- the pixel dimensions remain
    ;; untouched so the columns stay the same
    (when radec-reversed
      (rotatef cd1_1 cd2_1)
      (rotatef cd1_2 cd2_2)
      (rotatef ctype1 ctype2))
    ;;
    (cond ((not (or (and (equal ctype1 "RA---TAN")
			 (equal ctype2 "DEC--TAN"))
		    (and (equal ctype1 "RA---SIN")
			 (equal ctype2 "DEC--SIN"))
		    (and (equal ctype1 "RA---ZPN")
			 (equal ctype2 "DEC--ZPN"))
		    (every #'realp (list crval1 crval2 crpix1 crpix2
					 cd1_1 cd1_2 cd2_1 cd2_2))))
	   nil) ;; no valid WCS
	  ;;
	  (t ;; otherwise fill wcs
	  ;;
	   (setf (wcs-2d-crval1 wcs) (* 1d0 crval1))
	   (setf (wcs-2d-crval2 wcs) (* 1d0 crval2))
	   (setf (wcs-2d-crpix1 wcs) (* 1d0 crpix1))
	   (setf (wcs-2d-crpix2 wcs) (* 1d0 crpix2))
	   (setf (wcs-2d-cd1_1 wcs) (* 1d0 cd1_1))
	   (setf (wcs-2d-cd1_2 wcs) (* 1d0 cd1_2))
	   (setf (wcs-2d-cd2_1 wcs) (* 1d0 cd2_1))
	   (setf (wcs-2d-cd2_2 wcs) (* 1d0 cd2_2))
	   (setf (wcs-2d-equinox wcs) (* 1d0 equinox))
	   ;;
	   (values wcs radec-reversed)))))
  

;; get the PV1,2 vectors returning (VALUES PV1-VEC PV2-VEC)
(defun %get-pv-vectors-for-wcs-radec-tan (ff)
  (declare (type fits-file ff))
  ;;
  (flet ((get-pvec (k)
	   (loop
	     with pvec = (make-array  100 :element-type
				      'double-float :initial-element 0d0)
	     with biggest-i = nil
	     for i below (length pvec)
	     for header = (format nil "PV~D_~D" k i)
	     for val = (read-fits-header ff header)
	     when (and val (realp val))
	       do 
		  (setf (aref pvec i) (float val 1d0))
		    (setf biggest-i i)
	     finally
		(return
		  (when biggest-i
		    (subseq pvec 0 (1+ biggest-i)))))))
    ;;
    (values (get-pvec 1)
	    (get-pvec 2))))
      
	
	   



;; also gets the ZPN type, but leaves bits unfilled
(defun get-sin-proj-wcs-from-fits-file (ff)
  (declare (type fits-file ff))
  (let ((ctype1 (read-fits-header ff "CTYPE1"))
	(ctype2 (read-fits-header ff "CTYPE2")))
    ;;
    (when (or (and (equalp ctype1 "RA---SIN")
		   (equalp ctype2 "DEC--SIN"))
	      (and (equalp ctype2 "RA---SIN")
		   (equalp ctype1 "DEC--SIN")))
      (get-generic-2d-wcs-from-fits-file ff (make-wcs-radec-sin)))))

(defun get-tan-proj-wcs-from-fits-file (ff)
  (declare (type fits-file ff))
  (let ((ctype1 (read-fits-header ff "CTYPE1"))
	(ctype2 (read-fits-header ff "CTYPE2")))
    ;;    
    (when (or (and (equalp ctype1 "RA---TAN")
		   (equalp ctype2 "DEC--TAN"))
	      (and (equalp ctype2 "RA---TAN")
		   (equalp ctype1 "DEC--TAN"))
	      ;;
	      (and (equalp ctype1 "RA---TPV")
		   (equalp ctype2 "DEC--TPV"))
	      (and (equalp ctype2 "RA---TPV")
		   (equalp ctype1 "DEC--TPV"))
	      ;; note - no such thing as a RA---PV, it's just RA--TAN with PV terms
	      )

      (multiple-value-bind (pv1vec pv2vec)
	  (%get-pv-vectors-for-wcs-radec-tan ff)
	(let ((wcs (cond
		     ;; TPV type with radial terms - both the NASA RA---TPV convention
		     ;; and Terapix RA---TAN + PV terms use the same PV distortion,
		     ;; so we say that a TA---TAN plus PV is a TAN---TPV
		     ((or pv1vec pv2vec)
		      (make-wcs-radec-tan-tpv
		       :pv1vec (or pv1vec (make-array 0 :element-type 'double-float))
		       :pv2vec (or pv2vec (make-array 0 :element-type 'double-float))))
		     ;;
		     ;; otherwise, plain TAN
		     (t
		      (make-wcs-radec-tan)))))
	  (multiple-value-bind (wcs radec-reversed)
	      (get-generic-2d-wcs-from-fits-file ff wcs)
	    ;; swap the PV vectors if we swapped RA,DEC from a reversed system
	    (when radec-reversed
	      (when (wcs-radec-tan-tpv-p wcs)
		(rotatef (wcs-radec-tan-tpv-pv1vec wcs)
			 (wcs-radec-tan-tpv-pv2vec wcs))))
	    wcs))))))
			   
	      


(defun %read-sip-headers (ff sip-key)
  (let* ((n (cf:read-fits-header ff (format nil "~A_ORDER" sip-key))))
    (when n
      (let ((sc (wcs:make-sip-cor
		 :order n
		 :matrix (make-array (list (1+ n) (1+ n))
				     :element-type 'double-float
				     :initial-element 0d0))))
	(loop for i to n
	      do (loop for j to n
		       for hdr = (format nil "~A_~D_~D" sip-key i j)
		       for val = (cf:read-fits-header ff hdr)
		       when val
			 do (setf (aref (sip-cor-matrix sc) i j) (* 1d0 val))))
	sc))))
  

(defun get-tan-proj-sip-wcs-from-fits-file (ff)
  (declare (type fits-file ff))
  (let ((ctype1 (read-fits-header ff "CTYPE1"))
	(ctype2 (read-fits-header ff "CTYPE2")))
    ;;    
    (when (or (and (equalp ctype1 "RA---TAN-SIP")
		   (equalp ctype2 "DEC--TAN-SIP"))
	      (and (equalp ctype2 "RA---TAN-SIP")
		   (equalp ctype1 "DEC--TAN-SIP")))
      (let ((wcs (make-wcs-radec-tan-sip
		  :a (%read-sip-headers ff "A")
		  :b (%read-sip-headers ff "B")
		  :ap (%read-sip-headers ff "AP")
		  :bp (%read-sip-headers ff "BP"))))
	;; this should take care of the ENTIRE issue of RA/DEC
	;; reversal potential of CTYPE1/2, because the SIP correction
	;; affects only pixel-space
	(get-generic-2d-wcs-from-fits-file ff wcs)))))



(defun get-zpn-proj-wcs-from-fits-file (ff)
  (declare (type fits-file ff))
  (let ((ctype1 (read-fits-header ff "CTYPE1"))
	(ctype2 (read-fits-header ff "CTYPE2"))
	(pvec (make-array 100 :element-type 'double-float :initial-element 0d0))
	(wcs-zpn (make-wcs-radec-zpn)))
    
    (when (or (and (equalp ctype1 "RA---ZPN")
		   (equalp ctype2 "DEC--ZPN"))
	      (and (equalp ctype2 "RA---ZPN")
		   (equalp ctype1 "DEC--ZPN")))
      ;; read the pv array 
      (loop 
	 with biggest-i = 0
	 for i below 100
	 for header = (format nil "PV2_~D" i)
	 for val = (read-fits-header ff header)
	 when (and val (realp val))
	 do 
	     (setf (aref pvec i) (float val 1d0))
	   (setf biggest-i i)
	 finally
	   (setf pvec (subseq pvec 0 (1+ biggest-i))))
      ;;
      (setf (wcs-radec-zpn-pvec wcs-zpn) pvec)
      (setf (wcs-radec-zpn-np   wcs-zpn) (length pvec))
      (get-generic-2d-wcs-from-fits-file ff wcs-zpn)
      wcs-zpn)))



(defun get-linear-wcs-from-fits-file (ff)
  (declare (type fits-file ff))
  (let* ((ctype1 (read-fits-header ff "CTYPE1"))
	 (crval1 (read-fits-header ff "CRVAL1"))
	 (crpix1 (read-fits-header ff "CRPIX1"))
	 (cd1_1 (read-fits-header  ff "CD1_1"))
	 (cdelt1 (or (read-fits-header  ff "CDELT1") cd1_1)))
    ;;
    (cond ((not (and (equal ctype1 "LINEAR")
		     (every #'realp (list crval1 crpix1 cdelt1))))
	   nil) ;; no valid WCS
	  (t
	   (make-wcs-linear
	    :crval1 (* 1d0 crval1)
	    :crpix1 (* 1d0 crpix1)
	    :cdelt1  (* 1d0 cdelt1))))))

;; is KEY like PREFIX+"123"+"X"?  if 'm' is true, then there MUST be
;; digits. If 'm' is false, then there cannot be digits.  If 'a' is
;; true, then there MAY be a trailing char in A-Z
(defun %prefix+num? (prefix key m j) 
  (declare (type string prefix key))
  (and (eql (search prefix key) 0) ;; starts with prefix
       (loop
	 with ndigit = 0 and nlast = (1- (length key))
	 with bad-char = nil
	 for i from (length prefix) to nlast
	 for c = (aref key i)
	 for is-digit = (digit-char-p c)
	 if is-digit
	   do (incf ndigit)
	 else
	   do (when
		  (or (not j) ;; no terminating A-Z allowed
		      (not (= i nlast)) ;; non-digit is not last char
		      (not (char<= #\A c #\Z))) ;; non-digit is not in A-Z
		(setf bad-char t))
	 finally
	    (return
	      (if (or bad-char
		      (and (not m) (plusp ndigit)) ;; digits not allowed
		      (and m (zerop ndigit))) ;; required digits not found
		  nil
		  key)))))


;; test if a function is a WCS header
(defun wcs-header-p (key)
  "Test if KEY is a WCS header, in a mostly rigorous manner, returning NIL or KEY"
  (declare (type string key))
  (or (find key '("RADECSYS" "WCSAXES" 
		  "A_ORDER" "B_ORDER" "AP_ORDER" "BP_ORDER")
	    :test 'equalp)
      ;; headers that may have a 'alternate' suffix A-Z but don't have digits
      (some (lambda (prefix) (%prefix+num? prefix key nil t))
	    '("LONPOLE" "LATPOLE"  "EQUINOX" "RADESYS"))
      ;; headers that must have a digit and may have an A-Z alternate suffix
      (some (lambda (prefix) (%prefix+num? prefix key t t))
	    '("CTYPE" "CRVAL" "CRPIX" "CD1_" "CD2_" "CD3_" "CD4_"
	      "CROTA" "CDELT" "CUNIT"
	      "PC0_" "PC1_"  "PC2_"
	      "PV0_" "PV1_"  "PV2_"
	      "LONP" "LATP" "RADE" "EQUI"))
      ;; headers that must have digits, but can't have a A-Z suffix
      (some (lambda (prefix) (%prefix+num? prefix key t nil))
	    ;; https://fits.gsfc.nasa.gov/registry/zpxwcs/zpx-hdr.txt
	    '("WAT0_"  "WAT1_"  "WAT2_"
	      ;; https://fits.gsfc.nasa.gov/registry/sip/sipsample.txt
	      "A_0_" "A_1_"    "A_2_" "A_3_"
	      "B_0_" "B_1_"    "B_2_" "B_3_"
	      "AP_0_" "AP_1_"  "AP_2_" "AP_3_"
	      "BP_0_" "BP_1_"  "BP_2_" "BP_3_"))))


	
(defgeneric read-wcs (ff &key extension)
  (:documentation
    "read a wcs system from a fits file, returning the appropriate type
of structure"))

(defmethod read-wcs ((ff fits-file) &key (extension nil))
  (with-fits-extension (ff extension)
    (let ((ctype1 (read-fits-header ff "CTYPE1"))
	  (ctype2 (read-fits-header ff "CTYPE2")))
      (cond ((or (and (equalp ctype1 "RA---TAN") (equalp ctype2 "DEC--TAN"))
		 (and (equalp ctype2 "RA---TAN") (equalp ctype1 "DEC--TAN"))
		 ;; allow IRAF TNX sytem, but for now we ignore corrections
		 (and (equalp ctype1 "RA---TNX") (equalp ctype2 "DEC--TNX"))
		 (and (equalp ctype2 "RA---TNX") (equalp ctype1 "DEC--TNX"))
		 ;; and TPV system
		 (and (equalp ctype1 "RA---TPV") (equalp ctype2 "DEC--TPV"))
		 (and (equalp ctype2 "RA---TPV") (equalp ctype1 "DEC--TPV")))
	     (get-tan-proj-wcs-from-fits-file ff))
	    ;;
	    ((or (and (equalp ctype1 "RA---TAN-SIP") (equalp ctype2 "DEC--TAN-SIP"))
		 (and (equalp ctype2 "RA---TAN-SIP") (equalp ctype1 "DEC--TAN-SIP")))
	     (get-tan-proj-sip-wcs-from-fits-file ff))
	    ;;
	    ((or (and (equalp ctype1 "RA---SIN") (equalp ctype2 "DEC--SIN"))
		 (and (equalp ctype2 "RA---SIN") (equalp ctype1 "DEC--SIN")))
	     (get-sin-proj-wcs-from-fits-file ff))
	    ;;
	    ((or (and (equalp ctype1 "RA---ZPN") (equalp ctype2 "DEC--ZPN"))
		 (and (equalp ctype2 "RA---ZPN") (equalp ctype1 "DEC--ZPN")))
	     (get-zpn-proj-wcs-from-fits-file ff))
	    ;;
	    ((equalp ctype1 "LINEAR")
	     (get-linear-wcs-from-fits-file ff))))))


(defmethod read-wcs ((ff string) &key (extension nil))
  (let (fitsfile)
    (unwind-protect
	(progn
	  (setf fitsfile (open-fits-file ff :throw-error t))
	  (read-wcs fitsfile :extension extension))
      (when (and fitsfile (fits-file-is-open fitsfile))
	(close-fits-file fitsfile :throw-error t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 

(defgeneric write-wcs (wcs fits-file &key extension)
  (:documentation "Write a WCS object to a fits-file"))

;; any write-wcs with a string is changed to one with a fits-file
(defmethod write-wcs (wcs (fits-file string) &key (extension nil))
  (cf:with-open-fits-file (fits-file ff :mode :io)
    (write-wcs wcs ff :extension extension)))

;; a generic writer
(defun %write-wcs-radec-xxx (wcs fits-file &key
					     (ctype1 "RA---TAN")
					     (ctype2 "DEC--TAN"))
				 
  (cf:write-fits-header fits-file "CTYPE1" ctype1)
  (cf:write-fits-header fits-file "CTYPE2" ctype2)
  (cf:write-fits-header fits-file "CRVAL1" (wcs-radec-tan-crval1 wcs))
  (cf:write-fits-header fits-file "CRVAL2" (wcs-radec-tan-crval2 wcs))
  (cf:write-fits-header fits-file "CRPIX1" (wcs-radec-tan-crpix1 wcs))
  (cf:write-fits-header fits-file "CRPIX2" (wcs-radec-tan-crpix2 wcs))
  (cf:write-fits-header fits-file "CD1_1" (wcs-radec-tan-cd1_1 wcs))
  (cf:write-fits-header fits-file "CD1_2" (wcs-radec-tan-cd1_2 wcs))
  (cf:write-fits-header fits-file "CD2_1" (wcs-radec-tan-cd2_1 wcs))
  (cf:write-fits-header fits-file "CD2_2" (wcs-radec-tan-cd2_2 wcs))
  (cf:write-fits-header fits-file "EQUINOX" (wcs-radec-tan-equinox wcs))
  ;; delete the unwanted oldstyle headers
  (cf:delete-fits-header fits-file "CDELT1")
  (cf:delete-fits-header fits-file "CDELT2")
  (cf:delete-fits-header fits-file "CROTA1")
  (cf:delete-fits-header fits-file "CROTA2"))

(defmethod write-wcs ((wcs wcs-radec-tan) (ff fits-file) &key (extension nil))
  (with-fits-extension (ff extension)
    (%write-wcs-radec-xxx wcs ff)))


(defmethod write-wcs ((wcs wcs-radec-tan-tpv) (ff fits-file)  &key (extension nil))
  (with-fits-extension (ff extension)
    (%write-wcs-radec-xxx wcs ff ;; all the things generic to wcs-radec-tan
			  :ctype1 "RA---TPV"
			  :ctype2 "DEC--TPV")
    ;; and then do the PV1,2 vectors
    (flet ((do-one-pv (k pv-vec)
	     ;; delete any old PV headers
	     (when (cf:read-fits-header-list
		    ff
		    :keyword-wildcard (format nil "PV~D_*" k))
	       (loop for i below 100
		     do (cf:delete-fits-header ff (format nil "PV~D_~D" k i))))
	     ;;
	     (loop for pv across pv-vec
		   for i from 0
		   for header = (format nil "PV~D_~D" k i)
		   when (not (zerop pv))
		     do (cf:write-fits-header ff header pv))))
      (do-one-pv 1 (wcs-radec-tan-tpv-pv1vec wcs))
      (do-one-pv 2 (wcs-radec-tan-tpv-pv2vec wcs)))))

(defmethod write-wcs ((wcs wcs-radec-tan-sip) (ff fits-file)  &key (extension nil))
  (with-fits-extension (ff extension)
    (%write-wcs-radec-xxx wcs ff ;; all the things generic to wcs-radec-tan
			  :ctype1 "RA---TAN-SIP"
			  :ctype2 "DEC--TAN-SIP")
    ;; and do SIP headers
    (flet ((write-sip-header (sip-cor sip-key)
	     (when sip-cor ;; could be null
	       (cf:write-fits-header ff (format nil "~A_ORDER" sip-key)
				     (wcs:sip-cor-order sip-cor))
	       (loop
		 with n = (wcs:sip-cor-order sip-cor)
		 for i to n
		 do
		    (loop for j to n
			  when (<= (+ i j) n) ;; SIP only defined up to this
			    do
			       (let ((hdr (format nil "~A_~D_~D" sip-key i j))
				     (val (aref (sip-cor-matrix sip-cor) i j)))
				 (cf:write-fits-header ff hdr val)))))))
      ;;
      (write-sip-header (wcs-radec-tan-sip-a wcs)  "A")
      (write-sip-header (wcs-radec-tan-sip-b wcs)  "B")
      (write-sip-header (wcs-radec-tan-sip-ap wcs) "AP")
      (write-sip-header (wcs-radec-tan-sip-bp wcs) "BP"))))
				     

    
 
(defmethod write-wcs ((wcs wcs-radec-sin) (ff fits-file)  &key (extension nil))
  (with-fits-extension (ff extension)
    (%write-wcs-radec-xxx wcs ff
			  :ctype1 "RA---SIN"
			  :ctype2 "DEC--SIN")))


(defmethod write-wcs ((wcs wcs-radec-zpn) (ff fits-file)  &key (extension nil))
  (with-fits-extension (ff extension)
    (%write-wcs-radec-xxx wcs ff
			  :ctype1 "RA---ZPN"
			  :ctype2 "DEC--ZPN")
    ;; and write the nonzero coefficients
    (loop
      with pvec = (wcs-radec-zpn-pvec wcs)
      for i below (wcs-radec-zpn-np wcs)
      for p across pvec
      ;;when (not (zerop p)) ;; don't write zero coefficients
      do
	 (cf:write-fits-header
	  ff
	  (format nil "PV2_~d"  i)
	  p))))


(defmethod write-wcs ((wcs wcs-linear) (ff fits-file)  &key (extension nil))
  (with-fits-extension (ff extension)
    (cf:write-fits-header ff "CTYPE1" "LINEAR")
    (cf:write-fits-header ff "CRVAL1" (wcs-linear-crval1 wcs))
    (cf:write-fits-header ff "CDELT1" (wcs-linear-cdelt1 wcs))
    (cf:write-fits-header ff "CRPIX1" (wcs-linear-crpix1 wcs))))


;; FIXME - there are probably many more
(defparameter *potential-wcs-headers*
  '("CTYPE1" "CTYPE2" "CDELT1" "CDELT2" "CRVAL1" "CRVAL2" "CD1_1" "CD1_2" "CD2_1" "CD2_2"
    "CROTA1" "CROTA2" "CRPIX1" "CRPIX2"))

(defun %delete-pv-headers (ff &key (extension nil))
  (let ((pv-headers 
	  (mapcar 'first
		  (read-fits-header-list ff :keyword-wildcard "PV*_*"
					    :extension extension))))
    (loop 
      for i below 100
      do (loop for k below 100
	       for key =  (format nil "PV~D_~D" k i)
	       when (find key pv-headers :test 'equalp)
		 do (delete-fits-header ff key :extension extension)))))
    
(defun %delete-wat-headers (ff &key (extension nil))
  (let ((wat-headers 
	  (mapcar 'first
		  (read-fits-header-list ff :keyword-wildcard "WAT*_*"
					    :extension extension))))
    (loop 
      for i below 100
      do (loop for k below 100
	       for key =  (format nil "WAT~D_~3,'0D" k i)
	       when (find key wat-headers :test 'equalp)
		 do (delete-fits-header ff key :extension extension)))))	


(defmethod delete-wcs ((ff fits-file) &key (extension nil))
  (loop for key in *potential-wcs-headers*
	do (delete-fits-header ff key :extension extension))
  (%delete-pv-headers ff :extension extension)
  (%delete-wat-headers ff :extension extension))

(defmethod delete-wcs ((fits string) &key (extension nil))
  (with-open-fits-file (fits ff :mode :io)
    (delete-wcs ff :extension extension)))
