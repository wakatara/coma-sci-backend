
#|

mosaic images of 3 frames onto 1 frame using info from IRAF
gemini/gmos/gmosaic.cl

NOTE: we should really have gmos-n-e2v-v1,  gmos-n-e2v-v2, and not
      gmos-n-eev,  gmos-n-e2v   because the chip stayed the same (eev=e2v)
      but the boards changed.  But because of internal coma discussions
      we elected the current way.


|#




(in-package  gmos-proc)

;; we copy the lxshift from gmosaic.cl - we start our arrays at 1
(let ((lxshift (make-array '(3 4 4) :element-type 'single-float))
      (lyshift (make-array '(3 4 4) :element-type 'single-float))
      (lrotation (make-array '(3 4 4) :element-type 'single-float))
      ;; correction for binned pixels
      (lxbshift  (make-array '(3 4 4) :element-type 'single-float)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; GMOS-N old CCDs
  ;;  chip1
  
  (let ((i-ns 1) ;;  telescope (north=1 south=2)
	(iver 1)) ;; version (old=1 new=2)
    (setf (aref lxshift  i-ns iver 1)    -2.50)
    (setf (aref lyshift  i-ns iver 1)    -1.58)
    (setf (aref lrotation  i-ns iver 1)  -0.004)
    ;; chip2
    (setf (aref lxshift  i-ns iver 2)     0.0)
    (setf (aref lyshift  i-ns iver 2)     0.0)
    (setf (aref lrotation  i-ns iver 2)   0.0)
    ;; chip3
    (setf (aref lxshift  i-ns iver 3)     3.8723)
    (setf (aref lyshift  i-ns iver 3)     -1.86)
    (setf (aref lrotation  i-ns iver 3)   -0.046)
    ;;    
    ;; corrections for binned pixels
    (setf (aref lxbshift  i-ns iver 1) -3.5)
    (setf (aref lxbshift  i-ns iver 2) 0.0)
    (setf (aref lxbshift  i-ns iver 3) 4.8723))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; GMOS-NORTH - new e2vDD CCDs
  
  (let ((i-ns 1) ;; north/south
	(iver 2)) ;; version (old/new)
    (setf (aref lxshift  i-ns iver 1)    -2.7000)
    (setf (aref lyshift  i-ns iver 1)    -0.7490)
    (setf (aref lrotation  i-ns iver 1)  -0.0090)
    ;; chip2
    (setf (aref lxshift  i-ns iver 2)     0.0)
    (setf (aref lyshift  i-ns iver 2)     0.0)
    (setf (aref lrotation  i-ns iver 2)   0.0)
    ;; chip3
    (setf (aref lxshift  i-ns iver 3)     2.8014)
    (setf (aref lyshift  i-ns iver 3)     2.0500)
    (setf (aref lrotation  i-ns iver 3)   -0.0030)
    ;;    
    ;; corrections for binned pixels
    (setf (aref lxbshift  i-ns iver 1) -3.700)
    (setf (aref lxbshift  i-ns iver 2) 0.0)
    (setf (aref lxbshift  i-ns iver 3) 3.8014))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; GMOS-NORTH - new Hamamatsu CCDs
  ;; (FIXME - WRONG - NEED TO FILL IN FROM NEW IRAF CODE once installed)

  (let ((i-ns 1) ;; north/south
	(iver 3)) ;; version (old/new)
    (setf (aref lxshift  i-ns iver 1)    -2.5)
    (setf (aref lyshift  i-ns iver 1)    -1.58)
    (setf (aref lrotation  i-ns iver 1)  -0.004)
    ;; chip2
    (setf (aref lxshift  i-ns iver 2)     0.0)
    (setf (aref lyshift  i-ns iver 2)     0.0)
    (setf (aref lrotation  i-ns iver 2)   0.0)
    ;; chip3
    (setf (aref lxshift  i-ns iver 3)     3.8723)
    (setf (aref lyshift  i-ns iver 3)     -1.86)
    (setf (aref lrotation  i-ns iver 3)   -0.046)
    ;;    
    ;; corrections for binned pixels
    (setf (aref lxbshift  i-ns iver 1) -3.5)
    (setf (aref lxbshift  i-ns iver 2) 0.0)
    (setf (aref lxbshift  i-ns iver 3) 4.8723))
  

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; GMOS-SOUTH - old CCDs

  (let ((i-ns 2) ;; north/south
	(iver 1)) ;; version (old/new)
    (setf (aref lxshift  i-ns iver 1)    -1.44)
    (setf (aref lyshift  i-ns iver 1)    -5.46)
    (setf (aref lrotation  i-ns iver 1)  -0.01)
    ;; chip2
    (setf (aref lxshift  i-ns iver 2)     0.0)
    (setf (aref lyshift  i-ns iver 2)     0.0)
    (setf (aref lrotation  i-ns iver 2)   0.0)
    ;; chip3
    (setf (aref lxshift  i-ns iver 3)     7.53)
    (setf (aref lyshift  i-ns iver 3)     9.57)
    (setf (aref lrotation  i-ns iver 3)   0.02)
    ;;    
    ;; corrections for binned pixels
    (setf (aref lxbshift  i-ns iver 1) -2.44)
    (setf (aref lxbshift  i-ns iver 2)  0.0)
    (setf (aref lxbshift  i-ns iver 3)  7.53))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; GMOS-SOUTH - new CCDs
  
  (let ((i-ns 2) ;; north/south
	(iver 2)) ;; version (old/new)
    (setf (aref lxshift  i-ns iver 1)    -1.49)
    (setf (aref lyshift  i-ns iver 1)    -0.22)
    (setf (aref lrotation  i-ns iver 1)   0.011)
    ;; chip2
    (setf (aref lxshift  i-ns iver 2)     0.0)
    (setf (aref lyshift  i-ns iver 2)     0.0)
    (setf (aref lrotation  i-ns iver 2)   0.0)
    ;; chip3
    (setf (aref lxshift  i-ns iver 3)     4.31)
    (setf (aref lyshift  i-ns iver 3)     2.04)
    (setf (aref lrotation  i-ns iver 3)   0.012)
    ;;    
    ;; corrections for binned pixels
    (setf (aref lxbshift  i-ns iver 1) -2.49)
    (setf (aref lxbshift  i-ns iver 2)  0.0)
    (setf (aref lxbshift  i-ns iver 3)  5.31))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; GMOS-SOUTH - newest Hamamatsu CCDs

  (let ((i-ns 2) ;; north/south
	(iver 3)) ;; version (old/new)
    (setf (aref lxshift  i-ns iver 1)    -1.2)
    (setf (aref lyshift  i-ns iver 1)     0.71)
    (setf (aref lrotation  i-ns iver 1)   0.0)
    ;; chip2
    (setf (aref lxshift  i-ns iver 2)     0.0)
    (setf (aref lyshift  i-ns iver 2)     0.0)
    (setf (aref lrotation  i-ns iver 2)   0.0)
    ;; chip3
    (setf (aref lxshift  i-ns iver 3)     0.0)
    (setf (aref lyshift  i-ns iver 3)    -0.73)
    (setf (aref lrotation  i-ns iver 3)   0.0)
    ;;    
    ;; corrections for binned pixels
    (setf (aref lxbshift  i-ns iver 1) -2.4)
    (setf (aref lxbshift  i-ns iver 2)  0.0)
    (setf (aref lxbshift  i-ns iver 3)  0.0))


  (defun compute-gmos-shift-and-rotation
      (chip-number north-or-south chip-version xbin ybin)
    "Return (VALUES ROTATION XROTATION YROTATION GEOXSHIFT GEOYSHIFT IMAGE-GAP)
for  
     CHIP-NUMBER=1,2,3
     NORTH-OR-SOUTH = :NORTH or :SOUTH
     CHIP-VERSION = 1,2,3
     XBIN,YBIN = 1,2 ..

Rotation is counter-clockwise.  XROTATION and YROTATION may be useless;
see routine BUILD-TRANSFORM-FOR-GMOS-CHIP"
    (declare (type (member :north :south) north-or-south)
	     (type (integer 1 3) chip-number)
	     (type (integer 1 3) chip-version)
	     (type (integer 1 2) xbin ybin))
    (let* ((isbinned (not (and (= xbin 1) (- ybin 1))))
	   (ishamamatsu (= chip-version 3)) ;; only latest N/S are hamamatsu
	   (gapvalue (if ishamamatsu 61 37)) ;; DEFAULT value from gmosaic.cl
	   (xbingap (if ishamamatsu 60 36))
	   (inst (if (eq north-or-south :north) 1 2))
	   (iccd chip-version)
	   (k chip-number)
	   (thisimggap (if (> xbin 1)
			   (round (/ (* 1.0 xbingap) xbin))
			   gapvalue))
	   (lxrot 0.0)
	   (lyrot 0.0)
	   (geoxshift 0.0)
	   (rotation (aref lrotation inst iccd k))
	   (geoyshift 0.0))
      
      (if isbinned
	  (progn
	    (setf lxrot (* rotation (/ (float xbin) ybin)))
	    (setf lyrot (* rotation (/ (float ybin) xbin)))
	    (if (= xbin 1)
		(setf geoxshift (aref lxshift inst iccd k))
		(setf geoxshift (/ (aref lxshift inst iccd k) (float xbin))))
	    (setf geoyshift (/ (aref lyshift inst iccd k) (float ybin))))
	  (progn
	    (setf lxrot rotation)
	    (setf lyrot rotation)
	    (setf geoxshift (aref lxshift inst iccd k))
	    (setf geoxshift (aref lyshift inst iccd k))))

      ;; we should probably ignore lxrot,lyrot
      (values rotation lxrot lyrot geoxshift geoyshift thisimggap)))

  ) ;; close definitions of (LET ...


	
		     
	   
#|

now run 

GEOTRAN(xshift,yshift,xrotation,yrotation)

IMTILE(ncoverlap=-thisimggap) (negative, meaning columns of empty space)

we will do this using the linear resampling routine
  (imutils:resample-image-with-linear-xform ...)
place all 3 chips onto a plane.



|#

#|

the use of 2 angles for xrot and yrot seems strange, but we'll guess
at what they mean.  

Splitting into xrotation,yrotation seems be WRONG because we need to consider that
if we define bx,by as the respective binning factors

| nx_new |      | 1/bx  0 |  |  cos(ang)  -sin(ang) | | bx  0  |    | nx |
|        |   =  |         |  |                      | |        |    |    |
| ny_new |      | 1  1/by |  |  sin(ang)   cos(ang) | | 0   by |    | ny |
 
                |  cos(ang)      - by/bx sin(ang) |   | nx |
             =  |                                 |   |    |
                |  bx/by sin(ang)        cos(ang) |   | ny |


But for very small angles ANG, the Taylor expansion of cosine is just
1, and sin(ang)~ang, so that Iraf's xrot=sin(bx/by*ang) is essentially
equal to what we believe is the correct value, xrot=bx/by*sin(ang).

Note that IRAF is wrong (?) only for bx!=by - which never happens for real
observations.

|# 
(defun build-transform-for-gmos-params
    (rotation xbin ybin geoxshift geoyshift
     chip-xshift)
  "Build a linear transform for projecting a chip onto a common plane"
  (let* ((xform (imutils::make-xform-linear
		 :x0 (float (+ geoxshift chip-xshift) 1.0)
		 :y0 geoyshift))
	 (matrix (imutils:xform-linear-matrix xform))
	 (rot/rad (float (* (/ pi 180) rotation) 1.0))
	 (sinrot  (float (sin rot/rad) 1.0))
	 (cosrot  (float (cos rot/rad) 1.0))
	 (stretch (/ (* 1.0 xbin) ybin)))
    (declare (type single-float sinrot cosrot stretch))
		 
    (setf (aref matrix 0 0) cosrot)
    (setf (aref matrix 1 0) (+ (* sinrot stretch)))
    (setf (aref matrix 0 1) (- (/ sinrot stretch)))
    (setf (aref matrix 1 1) cosrot)
    xform))



(defun build-transforms-for-gmos-instrument
    (north-or-south chip-version xbin ybin chip-dims output-dims)
  "Return a vector of three transformations, for chips 1,2,3,
for a GMOS instrument described by:

    NORTH-OR-SOUTH = :NORTH or :SOUTH
    CHIP-VERSION = 1,2,3
    XBIN,YBIN = 1,2 .. 
    CHIP-DIMS are dimensions of one chip (always the same)
    OUTPUT-DIMS are dimensions of output array"
  
  (declare (type (member :north :south) north-or-south)
	   (type (integer 1 3) chip-version)
	   (type (integer 1 2) xbin ybin))

  (let* ((ncx (aref chip-dims 1))
	 (nox (aref output-dims 1))
	 ;; center chip center ncx/2 aligns with output center nox/2
	 (ncx/2 (ash ncx -1))
	 (nox/2 (ash nox -1))
	 ;; the 3 transforms, left center right
	 xfl xfc xfr)

    ;; compute center transform 
    (multiple-value-bind (rotation xrotation yrotation geoxshift geoyshift
			  image-gap)
	(compute-gmos-shift-and-rotation 2 north-or-south chip-version
					 xbin ybin)
      (declare (ignore xrotation yrotation image-gap))
      (let ((chip-xshift (* 1.0 (- nox/2 ncx/2))))
	(setf xfc
	      (build-transform-for-gmos-params
	       rotation xbin ybin
	       geoxshift geoyshift chip-xshift)))

      ;; compute left transform
      (multiple-value-bind (rotation xrotation yrotation geoxshift geoyshift
			    image-gap)
	  (compute-gmos-shift-and-rotation 1 north-or-south chip-version
					   xbin ybin)
	(declare (ignore xrotation yrotation))
	(let ((chip-xshift
		(* 1.0
		   (- (- nox/2 ncx/2) ;; center shift
		      ncx  ;; minus one chip displacement
		      image-gap)))) ;; plus the image gap
	  (setf xfl
		(build-transform-for-gmos-params
		 rotation xbin ybin
		 geoxshift geoyshift chip-xshift))))
      
      ;; compute left transform
      (multiple-value-bind (rotation xrotation yrotation geoxshift geoyshift
			    image-gap)
	  (compute-gmos-shift-and-rotation 3 north-or-south chip-version
					   xbin ybin)
	(declare (ignore xrotation yrotation))
	(let ((chip-xshift
		(* 1.0
		   (+ (- nox/2 ncx/2) ;; center shift
		      ncx ;; plus one chip displacement
		      image-gap)))) ;; plus the image gap
		      
	  (setf xfr
		(build-transform-for-gmos-params
		 rotation xbin ybin
		 geoxshift geoyshift chip-xshift))))
      
      (vector xfl xfc xfr))))
  



(defun %get-binning-for-fits (fits-file)
  (let* ((ccdsum (cf:read-fits-header fits-file "CCDSUM" :extension 2))
	 (nspace (if ccdsum (position #\space ccdsum)))
	 (xbin (if ccdsum
		   (parse-integer ccdsum :junk-allowed t :start 0)))
	 (ybin (if (and ccdsum nspace)
		   (parse-integer ccdsum :junk-allowed t :start nspace))))
    (values xbin ybin)))


;; fill corners with NaNs, where corner-clip is fractional image size
(defun %gmos-mosaic-corner-clip (im corner-clip)
  (declare (type imutils:image im)
	   (type (single-float 0.0 1.0) corner-clip))
  (let* ((nx (array-dimension im 1))
	 (ny (array-dimension im 0))
	 (cx (round (* corner-clip nx)))
	 (cy (round (* corner-clip ny)))
	 (a (/ (* 1.0 cy) cx))) ;; a=dy/dx = slope of corners

    ;; sweep out a corner rectangle of cx*cy, and NaN out the points
    ;; below the diagonal line; sign is the slope of the corner line,
    ;; and csign is whether the NaNs go above or below the corner line
    ;; xstart,ystart is the start of the line, and y has to be incremented
    ;; according to yinc

    (flet ((nan-corner (slope-sign direction  xstart ystart corner)
	     ;(format t "~%~%~%Doing corner ~A in image of size ~A cx=~A cy=~A~%" corner (array-dimensions im) cx cy)
	     (loop
	       for kx from 0 to cx  ;; from 0 to cx, not 1 to cx, because first index is at ix=xstart+0
	       for ix = xstart then (+ ix 1) ;; x always increases
					     ;; ly is the y that is on corner line, for this x
	       for ly = (+ ystart (* a slope-sign (- ix xstart)))
	       for ily = (round ly)
	       do
		  ;;(format t "  ix=~A kx=~A ly=~,5F  ily=~A~%" ix kx ly ily)
		  ;; now loop until we hit the edge
		  (loop for iy = ily then (+ iy direction) ;; y grows up or down
			until (or (minusp iy) (= iy ny)) ;; stop at edge
			do (setf (aref im iy ix) float-utils:*single-float-nan*)))))
      ;;
      (nan-corner -1 -1    0            cy '#:bottom-left) ;; bottom left
      (nan-corner +1 -1    (- nx cx 1)  0  '#:bottom-right)  ;; bottom right
      (nan-corner +1 +1    0            (- ny cy 1) '#:top-left) ;; top left
      (nan-corner -1 +1    (- nx cx 1)  (- ny  1) '#:top-right) ;; top right
      )))

(defun mosaic-gmos-chips (fits-file fits-mosaic-file
			  &key (overwrite nil) (side-trim 0.5)
			  ;; how much of corners of side-trimmed
			  ;; corners (as frac of image) to fill with
			  ;; NaN
			  (corner-clip 0.12))
  "Mosaic a 3-chip combined fits file into a full mosaic, trimming 
fraction SIDE-TRIM of the 2 side chips"
  (let ((id (instrument-id:identify-instrument fits-file))
	chip-version
	north-or-south
	xbin ybin
	naxis1 naxis2
	naxis1-out naxis2-out
	xforms
	output-array)
    (cond ((eq (type-of id) 'instrument-id:gmos-n-e2v-full-chip-array)
	   (setf chip-version 2)
	   (setf north-or-south :north))
	  ;;
	  ((eq (type-of id) 'instrument-id:gmos-n-hamamatsu-full-chip-array)
	   (setf chip-version 3)
	   (setf north-or-south :north))
	  ;;
	  ((eq (type-of id) 'instrument-id:gmos-s-eev-full-chip-array)
	   (setf chip-version 2)
	   (setf north-or-south :south))
	  ;;
	  ((eq (type-of id) 'instrument-id:gmos-s-hamamatsu-full-chip-array)
	   (setf chip-version 3)
	   (setf north-or-south :south))
	  ;;
	  (t
	   (error "Cannot mosaic-gmos-chips for fits ~A of type ~A"
		  fits-file id)))


    (setf naxis1 (cf:read-fits-header fits-file "NAXIS1" :extension 2)) ;; x dim
    (setf naxis2 (cf:read-fits-header fits-file "NAXIS2" :extension 2)) ;; y dim
    (setf naxis1-out (+  naxis1 (* 2 (round (* (- 1.0 side-trim) naxis1)))))
    (setf naxis2-out naxis2)


    ;; check that all extension the same size
    (loop for ext in '(2 3 4)
	  for n1 =  (cf:read-fits-header fits-file "NAXIS1"
					 :extension ext)
	  for n2 = (cf:read-fits-header fits-file "NAXIS2"
						:extension ext)
	  when (not (and n1 n2 (eql naxis1 n1) (eql naxis2 n2)))
	    do (error "Extension ~A of ~A has different dimensions [~A,~A] from first image extension [~A,~A]" ext fits-file n1 n2 naxis1 naxis2))
	    
		   
    
    ;;
    (multiple-value-setq (xbin ybin)
      (%get-binning-for-fits fits-file))

    (when (not (and xbin ybin))
      (error "Cannot get binning for fits  ~A of type ~A"
	     fits-file id))



    (setf output-array
	  (make-array (list naxis2-out naxis1-out)
		      :element-type 'single-float
		      ;; fill with NaN for chip gaps
		      :initial-element float-utils:*single-float-nan*
		      ))
    

    
    (setf xforms (build-transforms-for-gmos-instrument
		  north-or-south chip-version xbin ybin
		  (vector naxis2 naxis1)
		  (vector naxis2-out naxis1-out)))
    
    (cf:with-open-fits-file (fits-file ff-in)
      (cf:with-new-fits-file (fits-mosaic-file ff-out :overwrite overwrite)


	(loop for ext in '(2 3 4)
	      for xf across (the vector xforms)
	      for imsec = (progn
			    (cf:move-to-extension ff-in ext)
			    (cf:read-image-section ff-in))
	      for input-array = (cf:image-section-data imsec)
	      ;; center array is just nearest pixel interpolation because it doesn't rotate
	      ;; but others are linear
	      for interp-method = (if (= ext 3)
				      :nearest
				      :linear)
	      do (imutils:resample-image-with-linear-xform
		  input-array output-array xf
		  :interp-method :linear))
	;;
	(when corner-clip
	  (%gmos-mosaic-corner-clip output-array corner-clip))
	;;
	(cf:add-image-to-fits-file ff-out :float (vector naxis1-out naxis2-out) 
					  :create-data output-array)
	;; %copy-headers is defined in gmos-proc-assemble-chips.lisp
	(%copy-headers ff-in ff-out  3)
	
	;; fix the wcs using the offset of the 2nd chip
	(let ((xshift (imutils:xform-linear-x0 (aref xforms 1)))
	      (crpix1 (cf:read-fits-header ff-out "CRPIX1")))
	  (when crpix1
	    (cf:write-fits-header ff-out "CRPIX1" (+ crpix1 xshift))))
	(cf:write-fits-header ff-out "IMRED.GMOS-MOSAIC" "yes"
			      :comment  "This is a GMOS file mosaic'ed by IMRED package")))))


	      
	
