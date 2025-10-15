#|

Reporting in MPC AIDAS format. 

This is NOT well documented.

The document here: 
  https://minorplanetcenter.net//iau/info/IAU2015_ADES.pdf
is WRONG.



in particular, example on page 21

For astrometric catalog codes see
  https://minorplanetcenter.net/iau/info/CatalogueCodes.html

sample usage:

(mpc:xml-output-optical-observation-block-to-file
   (mpc:make-xml-observation-block  ;; a block of observations
      :mpc-code "568"... 
      ;; the individual observations
      :observations (mpc:make-xml-optical-observation 
                      :id "C/2018 O1" :ra ... ))
   outfile)

See sample structures at end of this file.


There are some issues with XML handling at MPC - for example,
if fails on <!-- xml comments --> inside an observation.

|#

(in-package mpc)

(defparameter *allowed-mpc-photometric-bands*
  '("Vj" "Rc" "Ic" "Bj" "Uj" ;; Jonhson-Cousins
    "Sg" "Sr" "Si" "Sz"      ;; SDSS
    "Pg" "Pr" "Pi" "Pz" "Pw" "Py"  ;; Pan-STARRS
    "Ao" "Ac" ;; ATLAS
    "U" "B" "V" "R" "I"             ;; the standard photometry bands
    "g" "r" "i" "z" "w" "y"         ;; standard AB mags
    "o" "c"                         ;; ATLAS again
    "G" "Gb" "Gr"                   ;; Gaia
    "J" "H" "K" "Y"                 ;; IR bands
    "C" "L" "u"))                   ;; clear/unknown/unknown - do not use

;; these seem different from the 80 column format, where things like
;; Vj are seemingly not allowed: https://www.minorplanetcenter.net/iau/info/OpticalObs.html
(defparameter *photometric-band-synonyms*
  '((:u . "u") (:g . "g") (:r . "r") (:i . "i") (:z . "z")
    ;;
    (:uj . "Uj") (:bj . "Bj") (:vj . "Vj") (:rc . "Rc") (:ic . "Ic")
    ;;
    (:gps1 . "Pg") (:rps1 . "Pr") (:ips1 . "Pi") (:zps1 . "Pz")
    (:yps1 . "Py")  (:wps1 . "Pw")
    ;;
    (:gsdss . "Sg") (:rsdss . "Sr") (:isdss . "Si") (:zsdss . "Sz")
    (:usdss . "Su")))

(defparameter *allowed-mpc-catalogs*
  '("Gaia2" "Gaia1" "URAT1" "UCAC5" "UCAC4" "PPMXL" "NOMAD" "2MASS"
    "UCAC3" "UCAC2" "UCAC1" "USNOB1" "USNOA2" "USNOSA2" "USNOA1"
    "USNOSA1" "Tyc2" "Tyc1" "Hip2" "Hip1" "ACT" "GSCACT" "GSC2.3"
    "GSC2.2" "GSC1.2" "GSC1.1" "GSC1.0" "GSC" "SDSS8" "SDSS7"
    "CMC15" "CMC14" "SSTRC4" "SSTRC1" "MPOSC3" "PPM" "AC" "SAO1984"
    "SAO" "AGK3" "FK4" "ACRS" "LickGas" "Ida93" "Perth70" "COSMOS"
    "Yale" "UNK"))

(defparameter *astro-catalog-synonyms*
    '(("gaia-dr1-catalog" . "Gaia1")
      ("refcat-catalog" . "Gaia2")
      ("sdss7-catalog" . "SDSS7")
      ("sdss8-catalog" . "SDSS8")
      ("sdss9-catalog" . "SDSS9")
      ("2mass-point-source-catalog" . "2MASS")
      ("usno-b1-catalog" . "USNOB1")
      ("psps-3pi-catalog" . "Gaia2"))) ;; calibrated with Gaia DR2
      
;; given a catalog, find it in 
(defun normalize-mpc-astrometric-catalog-name (catalog)
  (declare (type (or string symbol) catalog))
  (let ((cstring (string catalog)))
    (or (find cstring *allowed-mpc-catalogs* :test 'equalp)
	;; our catalogs in astro-catalog package
	(cdr (assoc cstring *astro-catalog-synonyms* :test 'equalp))
	;; anything like Gaia DR2 
	(cond ((search "Gaia" cstring :test 'equalp)
	       (cond ((search "DR1" cstring :test 'equalp)
		      "Gaia1")
		     ((search "DR2" cstring :test 'equalp)
		      "Gaia2")))
	       ((search "refcat" cstring :test 'equalp)
		"Gaia2")
	       ((or (search "PS1" cstring :test 'equalp)
		    (search "Pan-STARRS" cstring :test 'equalp))
		"Gaia2"))
	(error "Could not identify catalog ~S catalog" catalog))))
	
(defun normalize-mpc-photometric-catalog-name (catalog)
  (declare (type (or string symbol) catalog))
  (let ((cstring (string catalog)))
    (or (find cstring *allowed-mpc-catalogs* :test 'equalp)
	;; our catalogs in astro-catalog package
	(cdr (assoc cstring *astro-catalog-synonyms* :test 'equalp))
	;; anything like Gaia DR2 
	(cond ((search "Gaia" cstring :test 'equalp)
	       (cond ((search "DR1" cstring :test 'equalp)
		      "Gaia1")
		     ((search "DR2" cstring :test 'equalp)
		      "Gaia2")))
	      ;; they won't take PS1, so we go back to grand-daddy 2MASS
	      ((or (search "refcat" cstring :test 'equalp)
		   (search "PS1" cstring :test 'equalp)
		   (search "Pan-STARRS" cstring :test 'equalp))
	       "2MASS"))
	(error "Could not identify catalog ~S catalog" catalog))))

(defun normalize-mpc-photometric-band (band)
  (declare (type (or string symbol) band))
  (or
   ;; handle our standard symbols
   (cdr (assoc band *photometric-band-synonyms*))
   ;; demand case-sensitive equality for others
   (find (string band) *allowed-mpc-photometric-bands*
	   :test 'string=)
   ;;
   (error "Photometric band ~S not known" band)))

;; 1P or 73P-AC
(defparameter *perm-comet-re*  (re:compile-re "^(%d+P)([-]%a+)?$"))
;; eg J13
(defparameter *perm-natural-satellite-re*   (re:compile-re  "^[HVEMJSUN][%d]+$"))
;;
;; full asteroid name like "(123) Smith-Joneß"
(defparameter *full-asteroid-re* (re:compile-re  "^%([%d]+%) [%d%a- ]*$"))
;; we can't accept "123 Smith-Joneß" because that becomes confused with 2008 AB1

;; return (values permid provid) given an id
(defun %compute-permid-and-provid-from-id (id)
  (declare (type string id))
  (let* ((id% (string-trim " " id))
	 (id-minus-parens
	   (string-right-trim ")" (string-left-trim "(" id%)))
	 (astnum nil))
    (cond
      ;; an numbered asteroid like (123) or 123
      ((ignore-errors (parse-integer id-minus-parens :junk-allowed nil))
       (values id-minus-parens nil))
      ;; a full asteroid name like "(123) Smith" 
      ((and (re:match-re *full-asteroid-re* id%)
	    (setf astnum (ignore-errors (parse-integer id-minus-parens :junk-allowed t))))
       (values (format nil "~D" astnum) nil))
      ;; a permanent comet or a natural satellite
      ((or (re:match-re *perm-comet-re* id%)
	   (re:match-re *perm-natural-satellite-re* id%))
       (values id% nil))
      ;; else this must be a provisional ID
      (t
       (values nil id%)))))


;; xml-declaration means whether to write <?XML ..>, and NOSTART means
;; whether this starts a stream (so doing with-xml-output)
(defmacro maybe-with-cxml-sink ((sink &key xml-declaration nostart) &body body)
  `(let ((%the-sink (or ,sink (cxml:make-string-sink 
			       :indentation 2
			       :canonical nil
			       :omit-xml-declaration-p (not ,xml-declaration)))))
     (flet ((%mwxc-body () ,@body))
       (if ,nostart
	   (%mwxc-body) ;; don't do with-xml-output
	   (cxml:with-xml-output %the-sink (%mwxc-body))))))



(defparameter *allowed-optical-modes*
  '("CCD" "Photo"  "Transit" "Occultation"))


;; text without indentation to allow <foo>bar</foo> inline
(defmacro text/noindent (text)
  `(cxml:unescaped ,text))
(defmacro cxml-newline ()
  `(cxml:unescaped #\Newline))
;;
(defmacro cxml-float (x &key (ndecimal 5))
  `(text/noindent (format nil "~,VF" ,ndecimal ,x)))
(defmacro cxml-int (n)
  `(text/noindent (format nil "~D" ,n)))

(defstruct xml-observation-block
  ;; all inside <observationContext>
  ;; <observation>
  obj-types ;; this is "type" field - list of NEO, NEOCP, TNO, COMET, NATSAT
            ;; eventually it will be required.  Maybe just use NEO?
  mpc-code ;; awkwardly, this is STN in an <observation>
  observatory-name ;; optional
  ;;  <contact>
  submitter-name  ;; initials then surname
  ack-message ;; goes into a COMMENT
  ack-email   ;; goes into a COMMET
  ;; <telescope>
  aperture
  (design "Reflector")
  (detector "CCD")
  ;; <observers>
  observers ;; list of observers in <name>, initials then surname
  ;; <measurers>
  measurers  ;; initials then surname
  ;; end <observationContext>
  ;;
  ;; in <observations>
  observations)  ;; list of xml-output-optical-observation

 

;; unfortunately, ORDER MATTERS when writing this out
(defstruct xml-optical-observation
  permid          ;; IAU permanent designation
  provid          ;; IAU provisional ID
  id              ;; a general ID that we parse into either permid/provid
  trksub          ;; tracklet id unique per block, mandatory if no
  stn             ;; observing station
  prg             ;; program code assigned by MPC
  obs-time        ;; if dbl, it is MJDC otherwise YY-MM-DDThh:mm:ssZ
  ra-star dec-star  ;; only for occultations
  astcat          ;; astrometric catalog, mandatory
  ;; 
  (mode "CCD")
  ra
  dec
  rmsra   ;; optional but encouraged
  rmsdec  ;; optional but encouraged
  rmscorr ;; optional
  mag     ;; optional
  rmsmag
  photcat ;; mandatory if mag present
  band    ;; mandatory if mag present
  seeing  ;; optional
  photap  ;; optional (arcsec; encouraged for comets)
  exp     ;; exposure time, optional
  rmsfit  ;; rms fit of reference stars, optional
  nstars  ;; number of reference stars, optional
  uncTime ;; uncertainty in time [s]; optional
  logSNR  ;; mandatory in future
  notes   ;; one character notes, optional, up to 6 may be used
  remarks
  ;;
  (high-precision-mag nil) ;; if NIL, round to 0.1
  ;; optional user data to put into an observation for the benefit of code
  ;; that is processing observations
  user-data
  ) ;; string
  
;; to a string
(defmethod xml-output-optical-observation
    ((xml-obs xml-optical-observation) &key sink)
  (declare (type xml-optical-observation xml-obs))
  (let* ((permid (xml-optical-observation-permid xml-obs))
	 (provid (xml-optical-observation-provid xml-obs))
	 (id     (xml-optical-observation-id xml-obs))
	 (trksub  (xml-optical-observation-trksub xml-obs))
	 (stn     (xml-optical-observation-stn xml-obs))
	 (prg     (xml-optical-observation-prg xml-obs))
	 (obs-time     (xml-optical-observation-obs-time xml-obs))
	 (ra-star    (xml-optical-observation-ra-star xml-obs))
	 (dec-star  (xml-optical-observation-dec-star xml-obs))
	 (astcat  (xml-optical-observation-astcat xml-obs))
	 (mode  (xml-optical-observation-mode xml-obs))
	 (ra  (xml-optical-observation-ra xml-obs))
	 (dec  (xml-optical-observation-dec xml-obs))
	 (rmsra  (xml-optical-observation-rmsra xml-obs))
	 (rmsdec  (xml-optical-observation-rmsdec xml-obs))
	 (rmscorr  (xml-optical-observation-rmscorr xml-obs))
	 (mag  (xml-optical-observation-mag xml-obs))
	 (rmsmag  (xml-optical-observation-rmsmag xml-obs))	
	 (photcat  (xml-optical-observation-photcat xml-obs))
	 (band  (when (xml-optical-observation-band xml-obs)
		  (normalize-mpc-photometric-band
		   (xml-optical-observation-band xml-obs))))
	 (seeing  (xml-optical-observation-seeing xml-obs))
	 (photap  (xml-optical-observation-photap xml-obs))
	 (exp  (xml-optical-observation-exp xml-obs))
	 (rmsfit  (xml-optical-observation-rmsfit xml-obs))
	 (nstars  (xml-optical-observation-nstars xml-obs))
	 (uncTime  (xml-optical-observation-unctime xml-obs))
	 (logsnr  (xml-optical-observation-logsnr xml-obs))
	 (notes  (xml-optical-observation-notes xml-obs))	
	 (remarks  (xml-optical-observation-remarks xml-obs))
	 ;;
	 (high-precision-mag (xml-optical-observation-high-precision-mag xml-obs))
	 (obs-time-string
	   (cond ((stringp obs-time)
		  (concatenate 'string (string-right-trim "Z" obs-time) "Z"))
		 ((floatp obs-time) ;; it's an MJD
		  (concatenate 'string 
			       (astro-time:mjd-to-ut-string obs-time)
			       "Z")))))
  
  (when (or (and id (or permid provid))
	    (and permid provid))
    (error "Either ID or just one of PERMID, PROVID must be given."))
  (when (and (not (or id permid provid)) (not trksub))
    (error "No ID, PERMID, or PROVID, and TRKSUB not given."))
  (when (not (and stn obs-time astcat mode ra dec))
    (error "Each of STN OBS-TIME ASTCAT MODE RA DEC must be given."))
  (when (and mag (not band))
    (error "If MAG is supplied, then BAND must be supplied."))


  (when (not (member mode *allowed-optical-modes* :test 'equalp))
    (error "MODE is not one of ~A" *allowed-optical-modes*))
    

  (when id ;; if only ID given, figure out if it is PERMID or PROVID
    (multiple-value-setq (permid provid)
      (%compute-permid-and-provid-from-id id)))

  
    (maybe-with-cxml-sink (sink :xml-declaration nil :nostart (if sink t))
      ;; print out leading comment if present
      (cxml:with-element "optical"
      ;;
      (when provid (cxml:with-element "provID" (text/noindent provid)))
      (when permid (cxml:with-element "permID" (text/noindent permid)))
      (when trksub (cxml:with-element "trkSub" (text/noindent trksub)))
      (cxml:with-element "mode"   (text/noindent mode))
      (when notes (cxml:with-element "notes"   (text/noindent notes)))
      (cxml:with-element "stn"   (text/noindent stn))
      (when prg (cxml:with-element "prg"   (text/noindent prg))) ;; not sure if in correct order
      
      (cxml:with-element "obsTime"  (text/noindent obs-time-string))
      (when unctime
	(cxml:with-element "uncTime"   (cxml-float unctime :ndecimal 3)))
      (cxml:with-element "ra"   (cxml-float ra :ndecimal 5))
      (cxml:with-element "dec"   (cxml-float dec :ndecimal 5))
      (when rmsra
	(cxml:with-element "rmsRA"   (cxml-float rmsra :ndecimal 3)))
      (when rmsdec
	(cxml:with-element "rmsDec"   (cxml-float rmsdec :ndecimal 3)))
      (when rmscorr
	(cxml:with-element "rmsCorr"   (cxml-float rmscorr :ndecimal 3)))

      (cxml:with-element "astCat"
	(text/noindent (normalize-mpc-astrometric-catalog-name astcat)))

      (when mag
	(cxml:with-element "mag"  (if high-precision-mag
				      (cxml-float mag :ndecimal 3)
				      (cxml-float mag :ndecimal 1))))
      (when rmsmag
	(cxml:with-element "rmsMag"   (cxml-float rmsmag :ndecimal 3)))
      (when band 
	(cxml:with-element "band"   (text/noindent band)))
      (when photcat
	(cxml:with-element "photCat"
	  (text/noindent (normalize-mpc-photometric-catalog-name photcat))))
    

      (when (and ra-star dec-star)
	(cxml:with-element "raStar"   (cxml-float ra-star :ndecimal 5))
	(cxml:with-element "decStar"   (cxml-float dec-star :ndecimal 5)))


     
     

      (when exp
	(cxml:with-element "exp"   (cxml-float exp :ndecimal 3)))
      (when seeing
	(cxml:with-element "seeing"   (cxml-float seeing :ndecimal 3)))
      (when photap
	(cxml:with-element "photAp"   (cxml-float photap :ndecimal 3)))
      (when rmsfit
	(cxml:with-element "rmsFit"   (cxml-float rmsfit :ndecimal 3)))
      (when nstars
	(cxml:with-element "nStars"   (cxml-int nstars)))

      (when logsnr
	(cxml:with-element "logSNR"   (cxml-float logsnr :ndecimal 3)))

      (when remarks
	(cxml:with-element "remarks" (text/noindent remarks)))))))

	 

(defmethod xml-output-optical-observation-block
    ((obs-block xml-observation-block) &key sink stream)
  "Output OBS-BLOCK of type XML-OBSERVATION-BLOCK to CXML SINK, or to STREAM,
or to a string if neither is given."

  (xml-output-optical-observation-block (list obs-block)
					:sink sink
					:stream stream))


(defmethod xml-output-optical-observation-block
    ((obs-block-list list) &key sink stream)
  "Output OBS-BLOCK-LIST, a list of of type XML-OBSERVATION-BLOCK to CXML SINK, or to STREAM,
or to a string if neither is given.

These may be for different observatories and each will have its <obsBlock>

The first <obsBlock> is used for the AckMessage, AckEmail, ObjType."

  (when (not (every (lambda (thing) (typep thing 'xml-observation-block))
		    obs-block-list))
    (error "Every element of OBS-BLOCK-LIST is not a XML-OBSERVATION-BLOCK."))
  
  (when (and stream sink)
    (error "Only one of STREAM and SINK may be given."))
  (when stream
    (setf sink (cxml:make-character-stream-sink stream
						:canonical nil
						:indentation 2
						:omit-xml-declaration-p nil)))
  (dolist (obs-block obs-block-list)
    (when (not (and (xml-observation-block-mpc-code obs-block)
		    (xml-observation-block-submitter-name obs-block)
		    (xml-observation-block-ack-message obs-block)
		    (xml-observation-block-ack-email obs-block)
		    (xml-observation-block-aperture obs-block)
		    (xml-observation-block-design obs-block)
		    (xml-observation-block-detector obs-block)
		    (xml-observation-block-observers obs-block)
		    (xml-observation-block-measurers obs-block)
		    (xml-observation-block-observations obs-block)))
      (error "All fields MPC-CODE, SUBMITTER-NAME, ACK-MESSAGE, ACK-EMAIL, APERTURE,
DESIGN, DETECTOR, OBSERVERS, MEASURERS, OBSERVATIONS must be given in OBS-BLOCK.")))

  
  (maybe-with-cxml-sink (sink :xml-declaration t)
    (let ((obs-block-0 (first obs-block-list)))
      (cxml:with-element "ades"
	(cxml:attribute "version" "2017")  ;; <ades version=2017> must be 2nd line
	(cxml-newline)
	;; write these as comments, using first obs-block given
	(cxml:comment (format nil " AckMessage: ~A "
			      (xml-observation-block-ack-message obs-block-0)))
	(cxml-newline)
	(cxml:comment (format nil " AckEmail: ~A "
			      (xml-observation-block-ack-email obs-block-0)))
	(cxml-newline)
	(cxml:comment (format nil " ObjType: ~A "
			      (format nil  "~{~A~^ ~}"
				      (xml-observation-block-obj-types obs-block-0))))
	;;
	(dolist (obs-block obs-block-list)
	  (cxml:with-element "obsBlock"
	    (cxml:with-element "obsContext"
	      (cxml:with-element "observatory"
		(cxml:with-element "mpcCode"
		  (text/noindent  (xml-observation-block-mpc-code obs-block)))
		(when (xml-observation-block-observatory-name obs-block)
		  (cxml:with-element "name"
		    (text/noindent (xml-observation-block-observatory-name
				    obs-block)))))
	    
	      (cxml:with-element "submitter"
		(cxml:with-element "name"
		  (text/noindent (xml-observation-block-submitter-name obs-block))))
	    
	    
	      (cxml:with-element "telescope"
		(cxml:with-element "aperture"
		  (cxml-float (xml-observation-block-aperture obs-block)))
		(cxml:with-element "design"
		  (text/noindent (xml-observation-block-design obs-block)))
		(cxml:with-element "detector"
		  (text/noindent (xml-observation-block-detector obs-block))))
	    
	      (cxml:with-element "observers"
		(loop
		  for observer in (xml-observation-block-observers obs-block)
		  do
		     (cxml:with-element "name"
		       (text/noindent observer))))
	    
	      (cxml:with-element "measurers"
		(loop
		  for measurer in (xml-observation-block-measurers obs-block)
		  do
		     (cxml:with-element "name"
		       (text/noindent measurer)))))
	    ;; <observationContext over
	  
	    (cxml:with-element "obsData"
	      (loop
		for xml-obs in (xml-observation-block-observations obs-block)
		do (xml-output-optical-observation xml-obs :sink sink)))))))))

(defun xml-output-optical-observation-block-to-file
    (obs-block/s outfile &key (if-exists :supersede))
  "Output an OBS-BLOCK/S of type XML-OBSERVATION-BLOCK or a list of XML-OBSERVATION-BLOCK to OUTFILE.

In the case of a list, they may have different obs-codes, but each block must have its own obs-code."
  (with-open-file (stream outfile :direction :output :if-exists if-exists)
    (xml-output-optical-observation-block obs-block/s :stream stream)))
	  
(defparameter *sample-xml-observation*
  (mpc::make-xml-optical-observation
	   :id "C/2018 O1"
	   :stn "568" 
	   :obs-time "2015-01-01T12:12:12Z" 
	   :ra 1.0
	   :dec 2.0 
	   :rmsra 0.1d0
	   :rmsdec 0.1d0
	   :astcat "Gaia1"
	   :photcat "PS1" 
	   :band "g" 
	   :exp 60
	   :nstars 100
	   :rmsfit 0.01
	   :unctime 0.1 
	   :seeing 1.0))

(defparameter *sample-xml-observation-block* 
  (mpc::make-xml-observation-block 
   :mpc-code "568"
   :obj-types '("NEO")  ;; not mandatory but maybe good for NEOs?
   :observatory-name "MKO"
   :submitter-name "John Smith"
   :ack-message "this is the ACK message"
   :ack-email "johnsmith@foo.com"
   :aperture 10.1
   :observers '("John Doe" "Mrs. Buttersworth")
   :measurers '("Tweedle Dee" "Tweedle Dum")
   :observations (list *sample-xml-observation*)))
	  
  

  
		  
	    
    


  
