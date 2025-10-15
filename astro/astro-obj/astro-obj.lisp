
;; define some structures used for storing astronomical objects



(in-package astro-obj)


;; a minimal object with just x and y
(defstruct xyobj
  (x           0d0    :type double-float)
  (y           0d0    :type double-float))


;; xyobj with an ID tag
(defstruct (ixyobj (:include xyobj)) 
  (id          nil    :type t))

;; the basic astronomical object structure - can create
;; 'classes' derived from this that contain additional fields
(defstruct (obj (:include ixyobj))
  ;; the meaning of x,y is not fixed - it can refer to pix
  ;; coords or projected ra,dec
  (alpha       0d0    :type double-float)
  (delta       0d0    :type double-float)
  (epoch       :j2000 :type symbol)
  (mag         0.0    :type single-float)
  )



;; a structure for binning objects in x and y dimensions
(defstruct objgrid
  (nobj 0   :type (unsigned-byte 26))
  ;; all x,y are in pseudo-pixel coords, so that wcs (if present)
  ;; translates these to normal decimal degrees
  (xmin 0.0d0 :type double-float)
  (xmax 0.0d0 :type double-float)
  (ymin 0.0d0 :type double-float)
  (ymax 0.0d0 :type double-float)
  (dx   0.0d0 :type double-float) ;; xmax-xmin
  (dy   0.0d0 :type double-float) ;; ymax-ymin
  (dxbin 0.0d0 :type double-float);; size of a bin = dx/nx
  (dybin 0.0d0 :type double-float);; size of a bin = dy/ny 
  ;;
  (nx 0 :type (unsigned-byte 22))
  (ny 0 :type (unsigned-byte 22))
  ;; 
  ;; bins containing lists of obj arranged in an x,y grid indexed
  ;; as iy,ix; iy=0...ny-1,   ix=0...nx-1
  (bins nil :type (or null (simple-array t (* *))))
  ;;
  ;; list of obj sorted by increasing magnitude
  (list nil :type (or null list))
  ;;
  ;; pixel scale of x,y - size of a pixel in ARCSEC
  (arcsec/pix 0.0d0 :type double-float)
  ;; wcs coordinate system for obj, representing translation of
  ;; x,y into ra,dec
  (wcs nil))


 
