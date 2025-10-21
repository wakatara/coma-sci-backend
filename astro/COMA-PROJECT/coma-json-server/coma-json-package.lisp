
(defpackage coma-json-server
  (:use #:cl)
  (:export
   #:main  ; Added for buildapp entry point (Docker-specific)
   #:get-command-list
   #:run-coma-json-server
   #:launch-coma-json-server-web-interface
   #:stop-coma-json-server-web-interface
   ))
