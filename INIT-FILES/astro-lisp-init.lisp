
#| 

This file is loaded if ASTROLISP environment variable was set.

The following environment variables are needed - their values
will vary from machine to machine

  TERAPIX_DIRECTORY - location of terapix binaries
  VIZQUERY_PROGRAM  - binary for vizquery
  DS9_PROGRAM       - ds9 binary

|#

;; initialize astronomy lisp

(defparameter *astro-required-environment-variables*
  '("TERAPIX_DIRECTORY" "VIZQUERY_PROGRAM"
    "DS9_PROGRAM" "PSPS_USERNAME" "PSPS_PASSWORD"))

(loop for var in *astro-required-environment-variables*
      when (not (uiop/os:getenv var))
	collect var into bad-vars
      finally
	 (when bad-vars
	   (error "Astro environment variables ~A not defined" bad-vars)))



;; NOTE - the private PSPS is deprecated, now using public STSCI




;; configure ds9 package
(pconfig:set-config "DS9:DS9-PROGRAM" (uiop/os:getenv "DS9_PROGRAM"))

;; configure terapix - don't use /opt/local/bin
;;   because scamp is custom compiled to avoid a known bug
(pconfig:set-config "TERAPIX:TERAPIX-DIRECTORY"
		    (uiop/os:getenv "TERAPIX_DIRECTORY"))
		    
;; configure astro-catalog
(pconfig:set-config "ASTRO-CATALOG:VIZQUERY-PROGRAM"
		    (uiop/os:getenv "VIZQUERY_PROGRAM"))
(pconfig:set-config "ASTRO-CATALOG:VIZQUERY-SITE"
		    (uiop/os:getenv "VIZQUERY_SITE"))
