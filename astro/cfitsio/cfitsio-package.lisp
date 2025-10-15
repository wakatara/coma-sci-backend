 
;; package definition for cfitsio
(defpackage #:cfitsio
  (:nicknames #:cf)
  (:use #:common-lisp #:wcs)
  (:export
   ;; cfitsio-ffi.lisp
   #:cfitsio-is-multithreaded
   #:cfitsio-version
   #:*threadlock-cfitsio-calls*
   ;;
   ;; cfitsio.lisp  
   #:*cfitsio-libraries*
   ;; 
   #:fits-file #:fits-file-p #:fits-file-filename  #:fits-file-is-open
   #:fits-file-current-hdu-num         #:fits-file-current-hdu-name
   #:fits-file-current-hdu-type        #:fits-file-current-image-type
   #:fits-file-current-image-ndims     #:fits-file-current-image-size
   #:fits-file-current-table-nrows     #:fits-file-current-table-ncols
   #:fits-file-current-table-columns   #:fits-file-current-table-types
   #:fits-file-current-table-widths    #:fits-file-current-table-repeats
   #:fits-file-current-table-nitems/entry
   #:fits-file-current-table-length-col
   #:fits-file-num-hdus
   ;;
   #:image-section #:image-section-p #:make-image-section
   #:image-section-fits-file #:image-section-orig-fits-file
   #:image-section-type #:image-section-ndim #:image-section-size
   #:image-section-parent-size #:image-section-whole
   #:image-section-fp #:image-section-lp #:image-section-wcs
   #:image-section-data #:image-section-headers
   ;;
   #:open-fits-file #:close-fits-file
   #:with-open-fits-file #:maybe-with-open-fits-file #:with-new-fits-file
   #:get-header-space
   #:read-fits-header #:delete-fits-header #:write-fits-header
   #:read-fits-headers-as-string
   #:fits-free-memory  ;; if read-fits-headers-as-string returns C-STRING not LISP
   #:write-fits-comment #:write-fits-history
   #:read-fits-header-list
   #:move-to-extension
   #:with-fits-extension
   #:delete-current-extension
   #:copy-current-extension
   #:read-image-section #:write-back-image-section #:write-image-section
   #:create-fits-file #:add-image-to-fits-file
   #:add-table-to-fits-file
   #:read-column-from-fits-table
   #:write-column-to-fits-table
   #:compressed-image-p
   #:iterate-over-image-sections
   ;;
   ;; cfitsio-wcs.lisp
   #:read-wcs #:write-wcs #:delete-wcs
   #:wcs-header-p

   ;;
   ;;cfitsio-wcs-extras.lisp
   #:compute-pa-direction-for-wcs 
   #:wcs-pixel-direction-vector-for-PA 
   ;;
   ;; cfitsio-extras.lisp
   #:extract-fits-extension
   #:extract-image-subsection
   #:concatenate-fits-extensions
   #:write-2d-image-to-new-fits-file
   #:list-extensions
   #:iterate-over-image-sections
   #:print-fits-table
   #:delete-multiline-header
   #:write-multiline-header
   #:read-multiline-header
   #:parse-image-section-string
   #:read-fits-image-subsection-into-array
   ))  
