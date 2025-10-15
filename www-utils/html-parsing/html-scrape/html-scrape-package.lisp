
(defpackage html-scrape
  (:use #:cl)
  (:export

   ;; extract.lisp
   #:extract-text-from-parsed-html
   #:extract-value-from-text
   #:extract-value-from-parsed-html
   #:make-extractor-for-parsed-html ;; make lambda 

   ;; find-tags.lisp
   #:find-tag
   
   ;; scrape-tables.lisp
   ;; the main function to turn HTML into an array
   #:scrape-html-table-into-array
   ;; a lower level function to return a list of TABLE-ELEMENTS
   ;; corresponding to <TD> and <TH> tags
   #:convert-html-table-into-table-element-list
   ;;
   ;; the internals of a TABLE-ELEMENT
   #:table-element #:table-element-p #:table-element-contents
   #:table-element-name #:table-element-colspan #:table-element-rowspan
   #:table-element-irow #:table-element-icol
   #:table-element-keywords
   
   ))



  
