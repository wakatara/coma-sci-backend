
(in-package imred)


;; filter function that just retrieves all the imsecs in imsec-generator
(defun %null-stacker-filter-function (imsec-generator extdesc)
  (declare (ignore extdesc))
  (loop for i from 0
	for imsec = (funcall imsec-generator)
	until (not imsec)
	collect imsec))





;; (imsec-generator i) returns i'th imsec, and (imsec-generator nil) returns;
;; the number of imsecs available
(defun stack-one-extension (ff-out imsec-generator template-fits ihdu
			    &key
			      (stack-type :median)
			      (clipping-function nil)
			      (count-header nil)
			      (extdesc nil)
			      (output-null-val 0.0) ;; 
			      (imsec-filter-function nil)
			      (final-array-function nil)
			      (nstack-min 1))

  (declare (type (member :median :mean) stack-type)
	   (type (or null function) clipping-function))

  (let* ((imsec-list-filtered
	   (funcall (or imsec-filter-function #'%null-stacker-filter-function)
		    imsec-generator extdesc))
	 (nstack (length imsec-list-filtered)) 
	 ;; the next line uses OR to get the zeroth image, if none in
	 ;; the good list, to prevent complete failure - the output
	 ;; stack will still be bad, because it will be a stack of
	 ;; zero images.  But it will exist.
	 (imsec0 (or (or (first imsec-list-filtered)
		     (funcall imsec-generator 0))))
	 (dims (array-dimensions
		(cf:image-section-data imsec0)))
	 (im-out (imutils:make-image (first dims) (second dims) 
				     :initial-value output-null-val))
	 (naxes (vector (array-dimension im-out 1) (array-dimension im-out 0)))
	 (arrays (mapcar 'cf:image-section-data imsec-list-filtered)))
   

    (when arrays
      (float-utils:with-float-traps-masked (:all t)
	(imutils:image-stack
	 arrays
	 :stack-type stack-type
	 :ignore-val +invalid-pixel-value+
	 :null-val output-null-val
	 :clipping-function clipping-function
	 :img-out im-out)))

    (when (and arrays final-array-function) (funcall final-array-function im-out))


    (cf:add-image-to-fits-file  ff-out :float naxes :create-data  im-out)

    (cf:write-fits-header ff-out "IMRED.STACK_OK"
			  (>= nstack nstack-min)
			  :comment "Are there enough images in this stack?")
    (cf:write-fits-header ff-out "IMRED.STACK_BAD"
			  (not (>= nstack nstack-min))
			  :comment "Are there enough images in this stack?")

    (cf:write-fits-header ff-out "IMRED.NULLVAL" 
			  (if (float-nan-p output-null-val) "NaN" output-null-val)
			  :comment "Value of NULL pixels")
    ;; THEN copy headers
    (cf:with-open-fits-file (template-fits ff)
      (cf:move-to-extension ff ihdu)
      (copy-headers ff ff-out
		    :exclude '(;;"DATASEC" "BIASSEC" ;; why exclude these?
			       "BZERO" "BSCALE"
			       "NAXIS" "NAXIS1" "NAXIS2" "BITPIX")))
    (when count-header
      (cf:write-fits-header ff-out count-header (length arrays) :comment "Number in stack"))
    (cf:write-fits-comment ff-out (format nil "IMRED: ~A stacked ~A out of possible ~A"
					  stack-type (length arrays)
					  (funcall imsec-generator nil)))))
    



  

(defun stack-images (fits-list fits-out
		     &key
		       (stack-type :median) 
		       (reduction-plan *default-reduction-plan*)
		       (clipping-function nil)
		       (template-fits nil)
		       (count-header NIL)
		       (imsec-filter-function nil)
		       (final-array-function nil)
		       (if-exists :error)
		       (nstack-min 1))
  "Stack images in fits-list to out-list, using STACK-TYPE (:MEDIAN
or :MEAN) stacking, ignoring values with IGNORE-VAL and outputing
NULL-VAL for all pixels that have no value.


IMSEC-FILTER-FUNCTION is an optional function of the input imsecs 
called  as (FUNCALL IMSEC-FILTER-FUNCTION IMSEC-LIST EXTDESC), that may 
modify its data array, and MUST return a possibly modified list 
of input image sections.

FINAL-ARRAY-FUNCTION is an optional function of the output array, and
modifies it before writing.

The variables *FF-LIST* *FITS-FILE-LIST* *IMSEC-LIST* and *HDU-NUM*
are bound, and visible to FINAL-ARRAY-FUNCTION and IMSEC-FILTER-FUNCTION.

TEMPLATE-FITS is the file used as a header template; otherwise, 
the first fits file is used.

COUNT-HEADER is the header that is written for the number of images stacked

CLIPPING-FUNCTION for removing outliers is as in IMUTILS:IMAGE-STACK

Arrays are always single-float."

  (declare (type (member :error :supersede) if-exists))
  
  (when (probe-file fits-out)
    (if (eq if-exists :error)
	(error "Output file ~A exists" fits-out)
	(delete-file fits-out)))
  
  (let ((fits-list (mapcar 'fullfile fits-list))
	(fits-out (fullfile fits-out :create t :delete t))) ;; see notes for fullfile in utils.c

    (when (not fits-out)
      (error "Cannot create output file ~A in stack-images." fits-out))
    
    (when (member nil fits-list)
      (error "Some input files do not exist"))

    (when (not (all-images-compatible-p fits-list :reduction-plan reduction-plan))
      (error "Images in ~A are not compatible in size" fits-list))
 
    (when (member fits-out fits-list :test 'equal)
      (error "Output file ~A is in list of inputs ~A" fits-out fits-list))
  

      (flet ((read-one-imsec (ff ihdu) (cf:move-to-extension ff ihdu)
			     (let ((imsec (cf:read-image-section ff :type :single-float)))
			       ;; now intelligently null out pixels, depending on
			       ;; whether its a reduced file or not
			       (mark-bad-pixels 
				(cf:image-section-data imsec)
				(cf:read-fits-header ff "IMRED.NULLVAL")
				(reduction-plan-saturation-value reduction-plan)
				(reduction-plan-invalid-pixel-function reduction-plan)
				(reduction-plan-output-null-pixel-value reduction-plan))
			       imsec)))

	(let ((ff-list nil)) ;; list of open files
	  (unwind-protect    ;; ensure files are closed
	       (progn
		 (setf ff-list
		       (loop for fits in fits-list
			  collect (cf:open-fits-file fits :mode :input)))
		 ;;
		 ;; use a temporary output file so we don't leave a broken incomplete
		 ;; file in case of failure
		 (with-temporary-output-file (fits-out fits-out-tmp :extra-suffix "_TMP") 
		   (cf:with-new-fits-file  (fits-out-tmp ff-out :overwrite t) 
		     (loop 
		       with template = (or template-fits (first fits-list))
		       with extdesc-list = (build-extdesc-list-for-fits (first fits-list))
		       for extdesc in extdesc-list
		       for ihdu = (extdesc-n-ext extdesc)
		       ;; function to retrieve imsecs
		       for imsec-generator = (lambda (n)
					       (cond
						 ;; N=NIL queries number available
						 ((not n)
						  (length ff-list))
						 ;; if N is too big, return NIL=No_more_avail
						 ((>= n (length ff-list))
						  nil)
						 ;; else return Nth imsec
						 (t
						  (read-one-imsec (nth n ff-list) ihdu))))
		       do 
			  (cond
			    ;; just copy any non-image extension from the first image
			    ((not (extdesc-reduce-p extdesc))
			     (cf:with-open-fits-file ((first fits-list) ff)
			       (cf:move-to-extension ff ihdu)
			       (cfitsio:copy-current-extension ff ff-out)))
			    (t
			     ;; bind global variables
			     (let ((*ff-list* ff-list)
				   (*imsec-generator* imsec-generator)
				   (*fits-file-list* fits-list)
				   (*hdu-num* ihdu))
			       (stack-one-extension 
				ff-out imsec-generator
				template ihdu
				:output-null-val 
				(float (reduction-plan-output-null-pixel-value
					reduction-plan)
				       1.0)
				:clipping-function clipping-function
				:stack-type stack-type
				:count-header count-header
				:imsec-filter-function imsec-filter-function
				:extdesc extdesc
				:final-array-function final-array-function
				:nstack-min nstack-min))))))))
	    ;; unwind protected form
	    (loop 
	       for ff in ff-list
	       when (cf:fits-file-is-open ff)
	       do (cf:close-fits-file ff)))))))
		
	
