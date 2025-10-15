
(defun test-make-image (&key (file "test-img.fits") (nx 20) (ny 30))
  (cf:with-new-fits-file (file ff :overwrite t)
    (let ((data (make-array (list ny nx) :element-type 'single-float)))
      ;;
      (loop for ix below nx
	 do
	   (loop for iy below ny
	      do
		(setf (aref data iy ix)
		      (+ 1 ix (* iy 100.0)))))
      ;;
      (cf:add-image-to-fits-file ff  :float 
				 (vector nx ny) ;; note reversed indices
				 :create-data data ))))
  

(defun test-read-image (&key (file "test-img.fits"))
  (let ((ff (cf:open-fits-file file)))
    (format t "Opened ~A~%~%" ff)
    (multiple-value-bind (val comment hdrname)
	(cf:read-fits-header ff "NAXIS")
      (format t "reading NAXIS header -   NAME=~A  val=~A  comment=~A~%~%"
	      hdrname val comment)
    (let ((imsec (cf:read-image-section ff)))
      (format t "Imsec=~A~%~%" imsec)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-make-table  (&key (file "test-table.fits"))
  (cf:with-new-fits-file (file ff
			  :overwrite t
			  ;; IMPORTANT - must make primary headers
			  ;; SIMPLE, BITPIX, NAXIS=0
			  :make-primary-headers t)
    (let* ((nrow 10)
	   (icol (make-array nrow :initial-contents 
			     (loop for i from 1 to nrow collect i)))
	   (dcol (make-array nrow :initial-contents 
			     (loop for i from 1 to nrow collect (* pi i))))
	   (scol (make-array nrow :initial-contents 
			     (loop for i from 1 to nrow 
				collect (format nil "string~A" i))))
	   (i64col (make-array nrow :initial-contents 
			       (loop for i from 1 to nrow 
				  collect (+ (expt 2 34) i)))))
				  
      ;;
      (cf:add-table-to-fits-file ff #(("INTEGER_COL" :long)
				      ("DBL_COL" :double-float)
				      ("STRING_COL" :string 10)
				      ("INTEGER64_COL" :long-long))
				 :nrows nrow )
      (cf:write-column-to-fits-table  ff "INTEGER_COL" icol)
      (cf:write-column-to-fits-table  ff "DBL_COL" dcol)
      (cf:write-column-to-fits-table  ff "STRING_COL" scol)
      (cf:write-column-to-fits-table  ff "INTEGER64_COL" i64col)
      )))

     
	 			   
    
(defun test-read-table (&key (file "test-table.fits"))
  (cf:with-open-fits-file (file ff)
    (cf:move-to-extension ff 2)
    (loop for col across (cf:fits-file-current-table-columns ff)
	 collect (cf:read-column-from-fits-table ff col))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-headers (&key (file "test-headers.fits"))
  (cf:with-new-fits-file (file ff :overwrite t)
    (cf:add-image-to-fits-file ff  :float 
				 (vector 1 1) ;; note reversed indices
				 :create-data (make-array '(1 1) 
							  :element-type 'single-float
							  :initial-element 0.0 ))

    (cf:write-fits-header ff "TBOOL" T  :comment "TRUE bool")
    (cf:write-fits-header ff "FBOOL" NIL  :comment "FALSE bool")
    (cf:write-fits-header ff "STRING"  "I'm a String" :comment "A string")
    (cf:write-fits-header ff "LONG" 9999999  :comment "A long")
    (cf:write-fits-header ff "FLOAT"  2.71828  :comment "A float")
    (cf:write-fits-header ff "COMPLEX"  #C(2.71828 3.141)  :comment "A complex"))
  ;;
  (cf:with-open-fits-file (file ff)
    (loop for key in '("TBOOL" "FBOOL" "STRING" "LONG" "FLOAT" "COMPLEX")
	 do
	 (multiple-value-bind (val comment)
	     (cf:read-fits-header ff key)
	   (format t "KEY=~A  VAL=~S    COMMENT=~S~%" key val comment)))))

    
  
