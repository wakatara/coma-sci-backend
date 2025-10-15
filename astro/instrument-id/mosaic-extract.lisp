

(in-package instrument-id)

(defgeneric extract-one-image-from-mosaic-for-instrument
    (instrument fits-file extname/extnum output-fits-file
     &key decompress overwrite)
  (:documentation
   "Extract a single chip of an instrument to an output fits file. CHIP-ID can be the 
image extension number (starting with 1), or a string for an EXTNAME.

It preserves a separate primary extension if present in original.

If applied to a single-chip file, it can be used to decompress (decompression is T 
by default).  In this case, EXTNAME/EXTNUM is ignored.

A method (PATCH-EXTRACTED-EXTENSION-HEADERS 
          INSTRUMENT FITS-FILE EXTNAME/EXTNUM OUTPUT-FITS-FILE)
is called to patch any extra headers on a per-instrument basis."))

(defmethod extract-one-image-from-mosaic-for-instrument
    ((inst imaging-instrument) fits-file  extname/extnum output-fits-file
     &key (decompress t) (overwrite t))

  (let ((extension
	  (cond ((typep inst 'onechip)
		 (get-image-extension-for-onechip-instrument inst fits-file))
		(t
		 extname/extnum))))
    (cf:extract-fits-extension fits-file output-fits-file
			       :extension extension
			       :overwrite overwrite
			       :decompress decompress
			       :preserve-primary-extension t)
    
    (patch-extracted-extension-headers 
     inst fits-file extname/extnum output-fits-file)
    
    output-fits-file))



(defgeneric patch-extracted-extension-headers (inst fits-file extname/extnum output-fits-file)
  (:documentation
   "Internal method to patch up headers after an extension has been extracted.  By default,
it does nothing."))

(defmethod patch-extracted-extension-headers ((inst t) fits-file extname/extnum output-fits-file)
  (declare (ignore inst fits-file extname/extnum output-fits-file)))
			     


(defun extract-one-image-from-mosaic-fits (fits-file extname/extnum output-fits-file
					   &key (decompress t) (overwrite t))
  (extract-one-image-from-mosaic-for-instrument
   (or (identify-instrument fits-file)
       (error "Cannot identify instrument for ~A" fits-file))
   fits-file
   extname/extnum
   output-fits-file
   :overwrite overwrite
   :decompress decompress))

;; call this function inside %extract-mosaic-move-to-extension method
;; the telescope has repeated headers
#+nil ;; why do we need this again?
(defun %extract-mosaic-move-to-extension-tedious (ff extension-id &key (extname-header "EXTNAME"))
  "Tedious way to find extension, if extname-header is repeated (like CFHT)."
  (block retblock
    (loop for i from 1 to (cf:fits-file-num-hdus ff)
	  do (cf:move-to-extension ff i)
	     (loop with hlist = (cf:read-fits-header-list ff :extension i)
		   for h in hlist
		   for key = (first h) and val = (second h)
		   when (and (equalp key extname-header)
			     (equalp val extension-id))
		     do
			(return-from retblock val)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

