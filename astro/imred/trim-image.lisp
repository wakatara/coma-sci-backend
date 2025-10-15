
(in-package imred)

;; return a new vector of bounds 

#+nil ;; no longer necessary because instrument-id:get-trimsec-for-fits is used

(defun get-trimsec-from-ccdsec+biassec (ccdsec biassec)
  (let* ((xb0 (aref biassec 0))
	 (xb1 (aref biassec 1))
	 (yb0 (aref biassec 2))
	 (yb1 (aref biassec 3))	
	 (xc0 (aref ccdsec 0))
	 (xc1 (aref ccdsec 1))
	 (yc0 (aref ccdsec 2))
	 (yc1 (aref ccdsec 3))
	 (x0 xc0) (x1 xc1)
	 (y0 yc0) (y1 yc1))
    ;; clip only on edge - unfortunately,
    ;; we clip the first edge, but not the best
    ;; possible case in the case of degeneracies,
    ;; but such degeneracies should not happen (much?)
    (cond ((< xb0 xc0 xb1) (setf x0 xb1))
	  ((< yb0 yc0 yb1) (setf y0 yb1))
	  ((< xb0 xc1 xb1) (setf x1 xb0))
	  ((< yb0 yc1 yb1) (setf y1 yb0)))
    ;;
    (vector x0 x1 y0 y1)))

	
  
(defun trim-one-extension (ff ff-out extdesc type trimsec)
  (let* ((fp (vector (aref trimsec 0) (aref trimsec 2)))
	 (lp (vector (aref trimsec 1) (aref trimsec 3)))
	 ;; get current extension from input file FF because FF-OUT
	 ;; may be in indeterminate state
	 (extnum (cf:fits-file-current-hdu-num ff))
	 ;; we always read to float because of bzero issue
         (read-type :float)
	 (write-type (or type (extdesc-image-type extdesc)))
	 (imsec
	  (cf:read-image-section  ff :fp fp :lp lp  :type read-type))
	 (data (cf:image-section-data imsec))
	 (bzero (float (or (cf:read-fits-header ff "BZERO") 0.0) 1.0))
	 (bscale (float (or (cf:read-fits-header ff "BSCALE") 1.0) 1.0))
	 (naxes (vector (array-dimension data 1) (array-dimension data 0)))
	 (output-image-is-float (member write-type '(:float :double))))

    (declare (type (simple-array single-float (* *)) data)
	     (type single-float bzero bscale))

    ;; we have to fix the BZERO,BSCALE issue before writing float data
    ;; back, but for float images we just get rid of BZERO,BSCALE
    (when (not output-image-is-float)
      (when (not (and (= bzero 0.0) (= bscale 1.0)))
	(locally (declare (optimize speed))
	  (loop 
	    with k = (array-total-size data)
	    for i of-type fixnum below k 
	    do
	       (setf (row-major-aref data i)
		     (/ (- (row-major-aref data i) bzero)
			bscale))))))
    
    (cf:add-image-to-fits-file    
     ff-out write-type   
     naxes
     :create-data  data)

    ;; THEN copy headers
    (copy-headers ff ff-out
		  :exclude (append
			    '("DATASEC" "BIASSEC" 
			      "NAXIS" "NAXIS1" "NAXIS2" "BITPIX")
			    ;; get rid of BZERO,BSCALE for float
			    ;; output images
			    (if output-image-is-float
				'("BZERO" "BSCALE"))))
    ;;
    (cf:write-fits-header ff-out "TRIM" 
			  (format 
			   nil "~A  ~A" 
			   (sec-to-string trimsec)
			   (astro-time:ut-to-date-string (get-universal-time))))

    (instrument-id:set-standard-header ff-out
				       :trimmed "YES" 
				       :extension extnum)

    ;; write the new DATASEC and STATSEC into the headers, as standard headers
    (let* ((full-imsec (vector 1 (aref naxes 0) 1 (aref naxes 1)))
	   (full-imsec-string (format nil "[~A:~A,~A:~A]"
				     (aref full-imsec 0) (aref full-imsec 1)
				     (aref full-imsec 2) (aref full-imsec 3)))
	   ;; we computed the statsec in the extdesc to be the TRIMMED statsec
	   (statsec-trimmed (extdesc-statsec extdesc))
	   (statsec-trimmed-string
	     (format nil "[~A:~A,~A:~A]"
		     (aref statsec-trimmed 0) (aref statsec-trimmed 1)
		     (aref statsec-trimmed 2) (aref statsec-trimmed 3))))
      (instrument-id:set-standard-header ff-out :datasec full-imsec-string
						:extension extnum)
      (instrument-id:set-standard-header ff-out :statsec statsec-trimmed-string
						:extension extnum)
      ;; also write ordinary TRIMSEC header because others might expect them
      (cf:write-fits-header ff-out "TRIMSEC" full-imsec-string)
      (cf:write-fits-header ff-out "DATASEC" full-imsec-string))
      
      
    
    (cf:write-fits-header ff-out "TRIMSEC" (sec-to-string trimsec))

    #+nil ;; no longer preserve ccdsec and biassec
    (progn
      (cf:write-fits-comment    
       ff-out
       (format nil "Original ccdsec=~A"
	       (if ccdsec (sec-to-string ccdsec) "NONE")))
      (cf:write-fits-comment    
       ff-out
       (format nil "Original biassec=~A"
	       (if biassec (sec-to-string biassec) "NONE"))))
			   
    ;; fix WCS cripix - it looks like the convention is to define crpix on the
    ;; image array coordinate system, ignoring BIASSEC etc
    (let ((crpix1 (cf:read-fits-header ff "CRPIX1"))
	  (crpix2 (cf:read-fits-header ff "CRPIX2")))
      (when crpix1  (cf:write-fits-header ff-out "CRPIX1" (1+ (- crpix1 (aref fp 0)))))
      (when crpix2  (cf:write-fits-header ff-out "CRPIX2" (1+ (- crpix2 (aref fp 1))))))
    ff-out))

    
    



(defun trim-image (fits fits-out
		   &key (type nil)
		     (trimsec nil)				   
		     (if-exists :error))
  "Trim an image extension by extension, keeping only the part inside
CCDSEC, and removing DATASEC and BIASEC headers, and put in TRIM
header.  IF-EXISTS is :ERROR or SUPERSEDE.

TRIMSEC should be a vector, or a string to use a specific header, or
NIL to use the default trimsec from the generated EXT-DESC, from
instrument-id:get-trimsec-for-fits

It may not be possible to read and write a SHORT image because of
BZERO issues."
  (declare (type (member :error :supersede) if-exists))

  (when (probe-file fits-out)
    (if (eq if-exists :error)
	(error "Output file ~A exists" fits-out)
	(delete-file fits-out)))

  (let ((fits (fullfile fits))
	(fits-out (fullfile fits-out :create t :delete t))) ;; see fullfile in utils.lisp
	
    (when (equal fits fits-out)
      (error "output file ~A would overwrite input file ~A" fits-out fits))

    
    (with-temporary-output-file (fits-out fits-out-tmp :extra-suffix "_TMP")
      (cf:with-new-fits-file   (fits-out-tmp ff-out :overwrite t :make-primary-headers nil)
	(cf:with-open-fits-file (fits ff :mode :input)
	  (loop 
	    with extdesc-list = (build-extdesc-list-for-fits fits)
	    for ihdu from 1 to (cf:fits-file-num-hdus ff)
	    for extdesc in extdesc-list
	    do 
	       (cf:move-to-extension ff ihdu)
	       (cond 
		 ;; just copy any non-image extension
		 ((not (extdesc-reduce-p extdesc))
		  (cfitsio:copy-current-extension ff ff-out))
		 (t
		  (trim-one-extension ff ff-out extdesc type
				      (or
				       (%get-trimsec-at-extension ff trimsec ihdu)
				       (extdesc-trimsec extdesc)))))))))))
	   
