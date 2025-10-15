

(in-package run-program)


(defun get-path-list ()
  "Get a list of the current PATH components"
  (loop
    with path-string = (%get-path-variable)
    for kcolon-last = -1 then kcolon
    for kcolon = (position #\: path-string :start (1+ kcolon-last))
    collect (subseq path-string (1+ kcolon-last) kcolon)
    until (not kcolon)))


;; test if a file is a regular file by reading a char
(defun %regular-file-p (file)
  (and (probe-file file)
       (with-open-file (s file :direction :input
			  :element-type '(unsigned-byte 8))
	 (and s ;; directories opened as NIL, it seems
	      (ignore-errors (and (read-byte s nil nil) t))))))
 
    
(defun find-program-in-path (program)
  "Search for PROGRAM in current PATH. Not guaranteed to be a regular
file, or executable."
  (loop for path in (get-path-list)
	for fullprog = (format nil "~A/~A" path program)
	when (%regular-file-p fullprog)
	  do (return fullprog)))
