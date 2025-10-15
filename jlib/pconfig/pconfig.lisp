

#|
  
   A simple way of setting configuration variables for unloaded packages.
  
   at load time, do:
    (pconfig:set-config "FOO:BARVAR" 123)
   and in the package FOO, do
    (defparameter *bar* (pconfig:get-config "FOO:BARVAR"))

|#

(defpackage pconfig
  (:use #:cl)
  (:export
   #:set-config
   #:get-config
   #:config-exists-p
   #:clear-config))


(in-package pconfig)

(defvar *pconfig-hash* (make-hash-table :test 'equalp))

(defvar *pconfig-lock* (bordeaux-threads:make-lock "pconfig-lock"))

(defun set-config (varname value)
  "Set a configuration variable as (SET-CONFIG VARNAME VALUE)."
  (bordeaux-threads:with-lock-held (*pconfig-lock*)
    (setf (gethash varname *pconfig-hash*) value)))

(defun get-config (varname)
  "Get a configuration variable as (GET-CONFIG VARNAME)."
  (bordeaux-threads:with-lock-held (*pconfig-lock*)
    (gethash varname *pconfig-hash*)))

(defun config-exists-p (varname)
  "Does a configure variable named VARNAME exist?"
  (nth-value 1 (get-config varname)))

(defun clear-config (varname)
  "Clear a configuration variable VARNAME."
  (bordeaux-threads:with-lock-held (*pconfig-lock*)
    (remhash varname *pconfig-hash*)))
