;;;; main.lisp
;;;;
;;;; Entry point for standalone coma-json-server executable (Docker-specific)
;;;; Not part of Jan's canonical version - added for Docker containerization

(in-package coma-json-server)

(defun main (argv)
  "Main entry point for buildapp executable"
  (declare (ignore argv))

  ;; Parse command line arguments for port and host
  (let ((port (or (parse-integer (or (uiop:getenv "COMA_PORT") "8080")
                                 :junk-allowed t)
                  8080))
        (host (or (uiop:getenv "COMA_HOST") "0.0.0.0")))

    (format t "~%Starting COMA JSON Server...~%")
    (format t "  Host: ~A~%" host)
    (format t "  Port: ~D~%" port)
    (format t "~%")

    ;; Runtime initialization: Load large data files and initialize subsystems
    ;; Most data is now loaded at compile-time via eval-when blocks, but ASTORB
    ;; and lparallel must be initialized at runtime
    (format t "Initializing runtime data...~%")

    ;; 1. Initialize lparallel worker threads (4 workers)
    ;; MUST be at runtime: buildapp cannot save core with multiple threads
    (let ((ncpu (or (ignore-errors (parse-integer (uiop:getenv "NCPU"))) 4)))
      (format t "  - Initializing lparallel kernel (~D workers)...~%" ncpu)
      (when (not lparallel:*kernel*)
        (setf lparallel:*kernel* (lparallel:make-kernel ncpu))))

    ;; 2. Initialize astorb file list and load astorb data
    ;; MUST be at runtime: ASTORB database is too large for Docker image
    (format t "  - Initializing ASTORB file list...~%")
    (force-output)
    (handler-case
        (astorb:initialize-astorb-file-list)
      (error (e)
        (format t "    WARNING: Failed to initialize ASTORB file list: ~A~%" e)))

    ;; Try to download ASTORB if not present (best effort - non-critical)
    ;; The get-the-astorb call below will handle missing ASTORB gracefully
    (handler-case
        (astorb:retrieve-newest-astorb-file :verbose-stream nil)
      (error (e)
        (format t "    Note: ASTORB download skipped (file may already exist or network unavailable)~%")))

    (format t "  - Loading ASTORB asteroid orbit database...~%")
    (format t "    NOTE: First run will compile FASL (~5 minutes), subsequent runs are fast!~%")
    (force-output)
    (handler-case
        (astorb:get-the-astorb)
      (error (e)
        (format t "    WARNING: Failed to load ASTORB data: ~A~%" e)))
    (format t "    ASTORB database ready!~%")
    (force-output)

    ;; 3. Initialize orbit element vector for small body identification
    ;; MUST be at runtime: depends on ASTORB which is loaded at runtime
    (format t "  - Initializing orbit element database for small body identification...~%")
    (handler-case
        (small-body-identify:initialize-orbit-elements)
      (error (e)
        (format t "    WARNING: Failed to initialize orbit elements: ~A~%" e)))

    (format t "Runtime initialization complete!~%~%")

    ;; Launch the web server
    (launch-coma-json-server-web-interface :port port :host host)

    (format t "Server started. Press Ctrl+C to stop.~%~%")

    ;; Keep the server running
    (loop (sleep 60))))
