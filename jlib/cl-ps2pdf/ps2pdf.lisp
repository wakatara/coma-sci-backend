

(defpackage ps2pdf
  (:use #:cl)
  (:export
   #:ps2pdf))

(in-package ps2pdf)


;; the new version that uses gs directly
(defun ps2pdf (psfile &key (outfile nil) (delete-psfile nil) (timeout 300)
		      (crop t) (orientation nil))
  "Convert a PS file to PDF using PS2PDF (searched for in path).
Optionally, delete original ps if DELETE-PSFILE is set. Run with a TIMEOUT
in case something goes wonky.

If CROP is set (the default), then use the -dEPSCrop option to crop whitespace."
  (let* ((psfilens (namestring psfile))
	 (ndot (when (not outfile)
		 (or (position #\. psfilens :from-end t)
		     (error "PostScript file ~A does not have an extension"
			    psfilens))))
	 (basename (when (not outfile)
		     (subseq psfilens 0 ndot)))
	 (pdffile  (or outfile (format nil "~A.pdf" basename)))
	 (gs-prog 
	   (or (run-program:find-program-in-path "gs")
	       (error "gs (ghostcript) program not found in PATH")))
	 (norient ;; orientation number
	   (cond ((eq orientation :portrait)  0)
		 ((eq orientation :landscape) 1)
		 ((eq orientation :upside-down) 2)
		 ((eq orientation :seascape) 3)
		 (orientation ;;
		  (error "Invalid orientation ~A - valid are :portrait, :landscape, :upside-down, :seascape" orientation)))))

    (run-program:run-program-to-string
     gs-prog
     (remove nil (list "-P-" "-dSAFER"
		       "-q" "-P-" "-dNOPAUSE" "-dBATCH" "-sDEVICE=pdfwrite" "-sstdout=%stderr"
		       (when crop "-dPDFSETTINGS=/prepress")
		       (when crop "-dEPSCrop")
		       (format nil "-sOutputFile=~A" pdffile)
		       "-P-" "-dSAFER"
		       ;; -c .setpdfwrite  was removed - see
		       ;;   https://bugzilla.redhat.com/show_bug.cgi?id=1965717
		       ;; "-c" ".setpdfwrite"
		       ;;
		       ;; this increase VM threshold, which was what .setpdfwrite did
		       "-c" "3000000" "setvmthreshold"
		       (when orientation "-c")
		       (when orientation (format nil "<</Orientation ~D>> setpagedevice" norient))
		       "-f" psfilens))
     :timeout timeout)

    (if (not (probe-file pdffile))
	(error "Output PDF file ~A not found" pdffile)
	(progn
	  (when delete-psfile
	    (delete-file psfilens))
	  pdffile))))
    
#+nil ;; the old version that used ps2pdf
(defun ps2pdf (psfile &key (outfile nil) (delete-psfile nil) (timeout 300)
		      (crop t))
  "Convert a PS file to PDF using PS2PDF (searched for in path).
Optionally, delete original ps if DELETE-PSFILE is set. Run with a TIMEOUT
in case something goes wonky.

If CROP is set (the default), then use the -dEPSCrop option to crop whitespace."
  (let* ((psfilens (namestring psfile))
	 (ndot (when (not outfile)
		 (or (position #\. psfilens :from-end t)
		     (error "PostScript file ~A does not have an extension"
			    psfilens))))
	 (basename (when (not outfile)
		     (subseq psfilens 0 ndot)))
	 (pdffile  (or outfile (format nil "~A.pdf" basename)))
	 (ps2pdf-prog 
	   (or (run-program:find-program-in-path "ps2pdf")
	       (error "ps2pdf program not found in PATH"))))

    (run-program:run-program-to-string
     ps2pdf-prog
     (remove nil (list (when crop "-dPDFSETTINGS=/prepress")
		       (when crop "-dEPSCrop")
		       "-dAutoRotatePages=/None"
		       psfilens
		       pdffile)) ;; arg list
     :timeout timeout)

    (if (not (probe-file pdffile))
	(error "Output PDF file ~A not found" pdffile)
	(progn
	  (when delete-psfile
	    (delete-file psfilens))
	  pdffile))))
