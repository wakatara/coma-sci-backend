
(in-package imred)


(defun copy-one-extension (ff ff-out extdesc type &key fp lp (write-wcs t))
  (let* ( ;; we always read to float because of bzero issue
	 (read-type :float)
	 (write-type (or type (extdesc-image-type extdesc)))
	 (imsec
	  (cf:read-image-section  ff :type read-type :fp fp :lp lp))
	 (data (cf:image-section-data imsec))
	 (bzero (float (or (cf:read-fits-header ff "BZERO") 0.0) 1.0))
	 (bscale (float (or (cf:read-fits-header ff "BSCALE") 1.0) 1.0))
	 (naxes (vector (array-dimension data 1) (array-dimension data 0)))
	 (output-image-is-float (member write-type '(:float :double))))

    (declare (type (simple-array single-float (* *)) data)
	     (type single-float bzero bscale))

    ;; we have to fix the bzero,bscale issue before writing data
    ;; back because cfitsio applied ARRAY=BZERO+BSCALE+FITS
    ;; so we need to change back to FITS=(ARRAY-BZERO)/BSCALE.
    ;; But for float output images, we get rid of bzero, bscale
    ;; because IRAF can't handle them
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
    
    ;;
    (cf:add-image-to-fits-file    
     ff-out write-type   
     naxes
     :create-data  data)
    
    ;; THEN copy headers
    (copy-headers ff ff-out
		  :exclude (append '("NAXIS" "NAXIS1" "NAXIS2" "BITPIX")
				   ;; get rid of BZERO,BSCALE for float
				   ;; output images
				   (if output-image-is-float
				       '("BZERO" "BSCALE"))))

   				       
    ;;
    ;; write wcs in imsec (may have been fixed by
    ;; cf:read-image-section if FP, LP are set)
    (when (and write-wcs (cf:image-section-wcs imsec))
      (cf:write-wcs (cf:image-section-wcs imsec) ff-out))
    ;;
    (cf:write-fits-header
     ff-out "COPIED" 
     (subseq  (cf:fits-file-filename ff)
	      (max 0 (- (length (cf:fits-file-filename ff)) 20))
	      (length (cf:fits-file-filename ff)) ))))

    
    



(defun copy-image (fits fits-out &key (type nil)
		   (if-exists :error))
  "Copy an image extension by extension, possibly changing 
to TYPE.

It may not be possible to read and write a SHORT image because of
BZERO issues, but it should work."
  (declare (type (member :error :supersede) if-exists))

  (when (probe-file fits-out)
    (if (eq if-exists :error)
	(error "Output file ~A exists" fits-out)
	(delete-file fits-out)))

  (let ((fits (fullfile fits))
	(fits-out (fullfile fits-out :create t))) 
	
    (when (equal fits fits-out)
      (error "output file ~A would overwrite input file ~A" fits-out fits))

    (cf:with-new-fits-file   (fits-out ff-out :overwrite t) 
      (cf:with-open-fits-file (fits ff :mode :input)
	(loop 
	   with extdesc-list = (build-extdesc-list-for-fits fits)
	   for ihdu from 1 to (cf:fits-file-num-hdus ff)
	   for extdesc in extdesc-list
	   do 
	   (cf:move-to-extension ff ihdu)
	   (cond
	     ;; just copy any non-image extension directly
	     ((not (extdesc-reduce-p extdesc))
	      (cfitsio:copy-current-extension ff ff-out))
	     ;; otherwise, need to do it the hard way because
	     ;; we might be changing image type
	     (t
	      (copy-one-extension ff ff-out extdesc type))))))))
	   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-image-section (fits fits-out &key fp lp extension 
			   (type nil)
			   (if-exists :error))
  "Copy a section (default is all) of an image to a new image

FP,LP are the usual first and last pixel vectors, as FP=#(XMIN YMIN)
and LP=#(XMAX YMAX).  If not given, then the whole image is copied.

The WCS will be fixed to reflect pixel-range"
   (declare (type (member :error :supersede) if-exists))

  (when (probe-file fits-out)
    (if (eq if-exists :error)
	(error "Output file ~A exists" fits-out)
	(delete-file fits-out)))

  (let ((fits (fullfile fits))
	(fits-out (fullfile fits-out :create t))) 
	
    (when (equal fits fits-out)
      (error "output file ~A would overwrite input file ~A" fits-out fits))
    
    (cf:with-new-fits-file   (fits-out ff-out :overwrite t) 
      (cf:with-open-fits-file (fits ff :mode :input)
	(when extension (cf:move-to-extension ff extension))
	(let* ((extdesc (build-extdesc-at-current-hdu ff))
	       (fp (or fp #(1 1)))
	       (lp (or lp (extdesc-image-size extdesc))))
	  (cond
	    ;; must be an image extension
	    ((not (eq (extdesc-type extdesc) :image))
	     (error "current extension ~A/~A of-type ~A is not an image"
		    (cf:fits-file-current-hdu-name ff-out)
		    (cf:fits-file-current-hdu-num ff-out)
		    (cf:fits-file-current-hdu-type ff-out)))
	    ((not (extdesc-image-size extdesc))
	     (error "IMAGE-SIZE not found - weird"))
	    (t
	     ;;
	     (when (or (< (aref fp 0) 1)
		       (< (aref fp 1) 1)
		       (> (aref fp 0) (aref lp 0))
		       (> (aref fp 1) (aref lp 1))
		       (> (aref lp 0) (aref (extdesc-image-size extdesc) 0))
		       (> (aref lp 1) (aref (extdesc-image-size extdesc) 1)))
	       (error "FP=~A LP=~A does not fit into image of SIZE=~A"
		      fp lp (extdesc-image-size extdesc)))
	     (copy-one-extension ff ff-out extdesc type :fp fp :lp lp))))))))
  
  
