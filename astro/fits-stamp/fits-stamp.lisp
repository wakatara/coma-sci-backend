#|

Stamp generator using pgplot to create and annotate stamps, and output
PDF, PNG, GIF files.


Example usage

(fits-stamp:make-stamp
    "./test.fits"
    :pdf
    1000 1000 400 ;; x0,y0, size in pix
    :output-file   "./test.pdf"
    :num-labels t
    :toplabel "Halley's comet"

    ;; specify a list of markers
    ;;   the x,y of most markers can be specified in :COORDINATE-UNITS
    ;;   of :PIXEL, UNIT ([0,1]), or :EQUATORIAL (J2000, using WCS)
    :markers
      (list (make-instance 'fits-stamp:compass-rose  ;; will use WCS
                           :axis-length 0.15)
            (make-instance 'fits-stamp:pixel-scale-marker)  ;; will use WCS
	    (make-instance 'fits-stamp::arrow 
		           :xa 900 :xb 1000
		           :ya 900 :yb 1000)
            (make-instance 'fits-stamp:crosshairs 
                           :coordinate-units :equatorial  ;; will use WCS
                           :fail-silently nil ;; will throw error if, eg, no WCS
                           :x 231.1947  :y 1.68218 )
            (make-instance 'fits-stamp:circle 
                            :x 900 :y 1100 :color :green)
	    (make-instance 'fits-stamp:points 
			    :xvec #(850 860 870)
                            :yvec #(850 860 870) 
			    :ptype :circle-2
			    :color :blue)
            (make-instance 'fits-stamp:text-marker 
                            :x 0.1 :y 0.9
                            :coordinate-units :unit  ;; [0,1] coordinate system
			    :character-height 1.5
			    :text "Halley's comet")))


or as a shortcut to make a PDF stamp

(fits-stamp:make-pdf-stamp "./test.fits" "./test.pdf" 1000 1000 400 ....)

|#


(defpackage fits-stamp
  (:use #:cl)
  (:export
   #:make-stamp
   #:make-pdf-stamp
   ;; markers - don't export accessors because they aren't used
   ;; outside this package
   #:crosshairs
   #:arrow
   #:circle
   #:points
   #:compass-rose
   #:orbit-direction-axes
   #:pixel-scale-marker
   #:text-marker
   ;;
   #:fill-orbit-direction-axes-using-comet-elem
   ))

(in-package fits-stamp)

;; define some markers that can go on the stamp
(defclass fits-stamp-marker () ;; parent class
  ((color :initform :red :initarg :color :accessor fsm-color)
   (weight :initform 2 :initarg :weight :accessor fsm-weight)
   ;; if can't work (eg, missing WCS), just fail silently instead of doing an error
   (fail-silently :initform t :initarg :fail-silently :accessor fail-silently)))

;; markers that have an x,y position
(defclass %xy-mixin ()
  ((x :initform 0 :initarg :x :accessor fsm-x)
   (y :initform 0 :initarg :y :accessor fsm-y)
   ;; :pixel, :unit ([0,1]), or :equatorial
   (coordinate-units :initform :pixel :initarg :coordinate-units :accessor coordinate-units))) 

(defclass crosshairs (fits-stamp-marker  %xy-mixin)
  ((r-inner :initform 0.03 :initarg :r-inner :accessor r-inner) ;; outer radius of crosshairs, in UNIT [0,1] coords
   (r-outer :initform 0.10 :initarg :r-outer :accessor r-outer))) ;; inner ..
   
(defclass circle (fits-stamp-marker  %xy-mixin)
  ((r :initform 20 :initarg :r :accessor r)))

;; use pixel scale, then wcs, then wcs in image
(defclass pixel-scale-marker (fits-stamp-marker)
  ((pixel-scale :initform nil :initarg :pixel-scale :accessor pixel-scale)
   (weight      :initform 5)
   (bar-length  :initform nil :initarg :bar-length  :accessor bar-length))) ;; in arcsec

(defclass text-marker (fits-stamp-marker  %xy-mixin)
  ((text :initform "" :initarg :text :accessor text)
   (character-height :initform 0.7 :initarg :character-height :accessor character-height)
   ;; offsets are fraction of image
   (xoffset :initform 0 :initarg :xoffset :accessor xoffset)
   (yoffset :initform 0 :initarg :yoffset :accessor yoffset)
   (angle   :initform  0.0 :initarg :angle :accessor angle)
   (font    :initform :default :initarg :font :accessor font)
   (blanked :initform nil  :initarg :blanked :accessor blanked)
   (blanking-color :initform nil  :initarg :blanking-color :accessor blanking-color)
   ;; centered text, else start at xoffset, yoffset
   (centered :initform t :initarg :centered :accessor centered)))

;; compass rose takes a wcs, or vectors of north and east
(defclass compass-rose (fits-stamp-marker)
  ((wcs :initform nil :initarg :wcs :accessor wcs)
   ;; two element vectors #(xpix ypix)
   (north-vector :initform nil  :initarg :north-vector :accessor north-vector) 
   (east-vector  :initform nil  :initarg :east-vector  :accessor east-vector)
   ;; axis length in [0,1] plot coordinates
   (axis-length  :initform 0.15  :initarg :axis-length  :accessor axis-length) 
   (weight      :initform 5)
   (color :initform :red)))

;; orbit direction axes are draw in in a circle to show their length (projection)
(defclass orbit-direction-axes (fits-stamp-marker)
  ((wcs :initform nil :initarg :wcs :accessor wcs)
   ;; the PA of the various orbital vectors (E from N)
   (pa-solar :initform -90.0 :initarg :pa-solar :accessor pa-solar) ;; toward sun
   (pa-orbit :initform +90.0 :initarg :pa-orbit :accessor pa-orbit) ;; in dir of orbit
   (pa-plane :initform 0.0   :initarg :pa-plane :accessor pa-plane) ;; out of plane (orbit x sun)
   ;; the lengths of the various orbital vectors (dot product with L.O.S.)
   (dot-solar :initform 1d0  :initarg :dot-solar :accessor dot-solar)
   (dot-orbit :initform 1d0  :initarg :dot-orbit :accessor dot-orbit)
   (dot-plane :initform 1d0  :initarg :dot-plane :accessor dot-plane)
   ;;
   (axis-length  :initform 0.07  :initarg :axis-length  :accessor axis-length)
   (xc           :initform 0.13   :initarg :xc    :accessor xc) ;; center of object, UNIT coords
   (yc           :initform 0.87   :initarg :yc    :accessor yc) ;; center of object, UNIT coords
   (arrow-size   :initform 0.4  :initarg :arrow-size :accessor arrow-size)
   (weight      :initform 5)
   (color :initform :red)))

(defclass arrow (fits-stamp-marker)
  ((xa  :initform 0.0 :initarg :xa :accessor arrow-xa)
   (xb  :initform 1.0 :initarg :xb :accessor arrow-xb)
   (ya  :initform 0.0 :initarg :ya :accessor arrow-ya)
   (yb  :initform 1.0 :initarg :yb :accessor arrow-yb)
   (arrow-size    :initform 2.0  :initarg :arrow-size :accessor arrow-size)
   (weight :initform 5)
   ;; :pixel, :unit ([0,1]), or :equatorial
   (coordinate-units :initform :pixel :initarg :coordinate-units :accessor coordinate-units)))

(defclass points (fits-stamp-marker)
  ((xvec :initform (make-array 0) :initarg :xvec :accessor xvec)
   (yvec :initform (make-array 0) :initarg :yvec :accessor yvec)
   ;; ptype is a keyword, or a vector of keywords
   (ptype :initform :point :initarg :ptype :accessor ptype)
   ;; point-size is a number or a vector of numbers
   (point-size :initform 2 :initarg :point-size :accessor point-size)
    ;; :pixel, :unit ([0,1]), or :equatorial
   (coordinate-units :initform :pixel :initarg :coordinate-units :accessor coordinate-units)))



;; convert pixel coordinates to [0,1] coordinates
(defun convert-pixel-to-unit (x y x0 y0 size)
  (values
   (/ (+ x (- x0) (/ size 2.0)) size)
   (/ (+ y (- y0) (/ size 2.0)) size)))

;; convert [0,1] coordinates to pixel coordinates
(defun convert-unit-to-pixel (xu yu x0 y0 size)
  (values
   (+ (* xu size)  (- x0 (/ size 2.0)))
   (+ (* yu size)  (- y0 (/ size 2.0)))))

;; general conversion to pixel units, depending on UNIT={:pixel, :unit, :equatorial}
(defun convert-xy-to-pixels (x y units x0 y0 size wcs)
  (cond ((eq units :pixel)
	 (values x y))
	((eq units :unit) ;; [0,1] coords
	 (convert-unit-to-pixel x y x0 y0 size))
	((eq units :equatorial)
	 (when (not (wcs:wcs-2d-p wcs)) (error "UNITS = :equatorial but WCS undefined"))
	 (wcs:wcs-convert-ra-dec-to-pix-xy wcs (* 1d0 x) (* 1d0 y)))
	(t
	 (error "Unknown UNITS ~S - allowed are :PIXEL, :UNIT, and :EQUATORIAL"
		units))))
	 

;; perform the transform to pixel coordinates on an entire vector at once
(defun convert-xy-vectors-to-pixels (xvec yvec units x0 y0 size wcs)
  (declare (type vector xvec yvec))
  (cond ((eq units :pixel)
	 (values xvec yvec))
	(t
	 (let ((xv (make-array (length xvec)))
	       (yv (make-array (length yvec))))
	   (loop for x across xvec and y across yvec
		 for i from 0
		 do (multiple-value-bind (xx yy)
			(convert-xy-to-pixels x y units x0 y0 size wcs)
		      (setf (aref xv i) xx)
		      (setf (aref yv i) yy)))
	   (values xv yv)))))

     

(defgeneric render-marker (marker pgplot
			   &key x0 y0 size wcs))




(defmethod render-marker ((the-arrow arrow) (p pgplot:pgplot)
			   &key  x0 y0 size wcs)
  (declare (ignorable x0 y0 size))
  (multiple-value-bind (xa ya)
      (convert-xy-to-pixels (arrow-xa the-arrow) (arrow-ya the-arrow)
			    (coordinate-units the-arrow)
			    x0 y0 size wcs)
    (multiple-value-bind (xb yb)
	(convert-xy-to-pixels (arrow-xb the-arrow) (arrow-yb the-arrow)
			      (coordinate-units the-arrow)
			      x0 y0 size wcs)
      (pgplot:draw-arrow p xa ya xb yb
			 :color (fsm-color the-arrow)
			 :size (arrow-size the-arrow)))))


    
(defmethod render-marker ((the-points points) (p pgplot:pgplot)
			  &key x0 y0 size wcs)
  (multiple-value-bind (xv yv) ;; convert to pixel units
      (convert-xy-vectors-to-pixels 
       (xvec the-points) (yvec the-points)
       (coordinate-units the-points)
       x0 y0 size wcs)
    (loop with the-ptype = (ptype the-points)
	  with the-psize = (point-size the-points)
	  for x across xv
	  for y across yv
	  for i from 0
	  for ptype-symbol = (if (vectorp the-ptype) (aref the-ptype i) the-ptype)
	  for psize = (if (vectorp the-psize) (aref the-psize i) the-psize)
	  do
	     (pgplot:points p x y ptype-symbol
			    :color (fsm-color the-points)
			    :size psize))))

(defmethod fill-orbit-direction-axes-using-comet-elem
    ((the-daxes orbit-direction-axes)  (comet-elem orbital-elements:comet-elem)
     (mjd real) &key (observatory "geocenter"))
  "Populate ORBIT-DIRECTION-AXES with position angles and dot products"
  (let ((orbit-dirs (slalib-ephem::generate-orbit-dirs-for-comet-elem-for-observatory
		     comet-elem mjd observatory)))
    ;;
    (setf (pa-solar the-daxes) (slalib-ephem:orbit-dirs-solar-pa orbit-dirs))
    (setf (pa-orbit the-daxes) (slalib-ephem:orbit-dirs-orbit-pa orbit-dirs))
    (setf (pa-plane the-daxes) (slalib-ephem:orbit-dirs-plane-pa orbit-dirs))
    ;;
    (setf (dot-solar the-daxes) (slalib-ephem:orbit-dirs-solardot orbit-dirs))
    (setf (dot-orbit the-daxes) (slalib-ephem:orbit-dirs-orbitdot orbit-dirs))
    (setf (dot-plane the-daxes) (slalib-ephem:orbit-dirs-planedot orbit-dirs))
    ;;
    the-daxes))
    
		     

(defmethod render-marker ((the-daxes orbit-direction-axes) (p pgplot:pgplot)
		      &key x0 y0 size wcs)
  (declare (ignorable x0 y0 size))
  (let ((pa-solar (pa-solar the-daxes))   (dot-solar (dot-solar the-daxes))
	(pa-orbit (pa-orbit the-daxes))   (dot-orbit (dot-orbit the-daxes))
	(pa-plane (pa-plane the-daxes))   (dot-plane (dot-plane the-daxes))
	(axis-length (axis-length the-daxes)) ;; unit coords
	(xc (xc the-daxes))  	(yc (yc the-daxes)) ;; center, in unit coords
	(color (fsm-color the-daxes))
	(weight (fsm-weight the-daxes))
	(wcs  (or (wcs the-daxes) wcs (error "No WCS when rendering ORBIT-DIRECTION-AXES")))
	solar-vec orbit-vec plane-vec  ;; unit vectors
	solar-len orbit-len plane-len) ;; their lengths

    (when (not (and pa-solar pa-orbit pa-plane dot-solar dot-orbit dot-plane color weight xc yc))
      (error "Required fields for rendering ORBIT-DIRECTION-AXES not present"))
    ;;
    (flet
	;; make an axis vector that is normalized to length DOT, in directions aligned
	;; with pixel (and thus UNIT) coordinates - no coordinate transforms needed
	;; because both UNIT and PIXEL coords increase to upper-right
	((make-axis-vec (pa dot)
	     (multiple-value-bind (xp yp)
		 (wcs:wcs-convert-world-xy-to-pix-xy
		  wcs ;; PA=0 --> (0,1); PA=90 --> (-1,0)
		  (sin (* (/ pi -180) pa))
		  (cos (* (/ pi 180) pa)))
	       (let* ((norm (/ 1d0 (+ 1d-10 (sqrt (+ (expt xp 2) (expt yp 2))))))
		      (idot (sqrt (- 1.0001d0 (* dot dot)))) ;; dot is cosine; we want sin
		      (len (* (max 0.1 idot) ;; impose a minimum length
			      axis-length)) ;; scale factor, including tilt of axis to LOS
		      (vnorm (vector (* norm xp) (* norm yp))))
		 (values vnorm len)))))
      (multiple-value-setq (solar-vec solar-len) (make-axis-vec pa-solar dot-solar))
      (multiple-value-setq (orbit-vec orbit-len) (make-axis-vec pa-orbit dot-orbit))
      (multiple-value-setq (plane-vec plane-len) (make-axis-vec pa-plane dot-plane))

      (pgplot:with-window (p 0 1 0 1) ;; in unit coords
	(pgplot:with-pgplot-settings (p :line-width weight)
	  (flet ((draw-vector (vector len label col)
		  
		   ;; draw a tick mark on the circle
		   (pgplot:connect
		    p
		    (vector (+ xc (* 0.9 axis-length (aref vector 0)))
			    (+ xc (* 1.10 axis-length (aref vector 0))))
		    (vector (+ yc (* 0.9 axis-length (aref vector 1)))
			    (+ yc (* 1.10 axis-length (aref vector 1)) ))
		    :color :dark-gray
		    :line-width 4)
		   
		   (pgplot:draw-arrow p xc yc
				      (+ xc  (* len (aref vector 0)))
				      (+ yc  (* len (aref vector 1)) )
				      :color col :size (arrow-size the-daxes))
		   ;;
		   (when label ;; put the label just outside the circle
		     (flet ((do-text (text-color weight)
			      (pgplot:with-pgplot-settings (p :line-width weight)
				(pgplot:write-text p label ;; write text beyond the arrow
						   (+ xc (* 1.5 axis-length (aref vector 0)))
						   (+ yc (* 1.5 axis-length (aref vector 1)))
						   :color text-color
						   :center t :character-height 1.2))))
		       (do-text :background 12)
		       (do-text col 3)))))
	    ;; black circle on gray circle for distinct-ness
	    (pgplot:circle p xc yc axis-length :color :background :line-width 12)
	    (pgplot:circle p xc yc axis-length :color :dark-gray :line-width 5)
	    ;; the orthogonal vector isn't really useful to plot.  What does it mean?
	    ;;(draw-vector plane-vec plane-len "n"         :blue+cyan)
	    (draw-vector solar-vec solar-len "\\(2281)"  :default)
	    (draw-vector orbit-vec orbit-len "v"         :red)
	    ))))))
					 

      
    


(defmethod render-marker ((the-cr compass-rose)  (p pgplot:pgplot)
			  &key x0 y0 size wcs)
  (let* ((o-vector (make-array 2 :initial-element 0.0)) ;; origin
	 (n-vector (copy-seq (north-vector the-cr))) ;; vector pointing N
	 (e-vector (copy-seq (east-vector the-cr)))  ;; vector pointing E
	 (nl-vector nil) ;; vector the letter N position
	 (el-vector nil) ;; vector the letter E position
	 (axis-length/pix (* size (axis-length the-cr)))
	 (color (fsm-color the-cr))
	 (weight (fsm-weight the-cr))
	 (wcs (if (not (and n-vector e-vector))
		  (or (wcs the-cr)
		      wcs))))
    (when (not (or (and n-vector e-vector)
		   wcs))
      (error "Cannot render a COMPASS-ROSE without either N-VECTOR,E-VECTOR or given WCS or WCS in the fits file."))
    ;;
    ;; compute N-VECTOR and E-VECTOR
    (when (not n-vector) ;; we know wcs is defined
      ;; do north vector
      (multiple-value-bind (xpix ypix)
	  (wcs:wcs-convert-world-xy-to-pix-xy wcs 0.0d0 0.01d0)
	(decf xpix (wcs:wcs-2d-crpix1 wcs)) ;; deviation from center pix
	(decf ypix (wcs:wcs-2d-crpix2 wcs))
	(setf n-vector (vector xpix ypix)))
       ;; do east vector
      (multiple-value-bind (xpix ypix)
	  (wcs:wcs-convert-world-xy-to-pix-xy wcs 0.01d0 0.0d0)
	(decf xpix (wcs:wcs-2d-crpix1 wcs)) ;; deviation from center pix
	(decf ypix (wcs:wcs-2d-crpix2 wcs))
	(setf e-vector (vector xpix ypix))))
    ;; make the vectors representing the locations of letters N,E on label
    (setf nl-vector (copy-seq n-vector))
    (setf el-vector (copy-seq e-vector))
    
    ;; now normalize the vectors, giving them the right length length and starting point
    (flet ((norm-vector (v factor)
	     (let ((norm (sqrt (+ (expt (aref v 0) 2)
				  (expt (aref v 1) 2)))))
	       (when (zerop norm)
		 (error "Direction vector has zero norm when drawing compass rose."))
	       (setf (aref v 0) (* factor axis-length/pix (/ (aref v 0) norm)))
	       (setf (aref v 1) (* factor axis-length/pix (/ (aref v 1) norm))))))
      (norm-vector n-vector 1.0)
      (norm-vector e-vector 1.0)
      (norm-vector nl-vector 1.20) ;; the letter vectors for N,E extend farther
      (norm-vector el-vector 1.20))

    ;; shift the vectors to the upper right corner, as tightly as possible
    (let* ((ximright (+ x0 (* 0.5 size))) ;; top right bounds of image
	   (yimtop   (+ y0 (* 0.5 size)))
	   (xright (max 0.0 (aref nl-vector 0) (aref el-vector 0))) ;; top right bound of rose
	   (ytop   (max 0.0 (aref nl-vector 1) (aref el-vector 1)))
	   (edge-room   (* 0.05 size)) ;; room to leave by edge
	   (dx     (- ximright xright edge-room)) ;; the shift we want
	   (dy     (- yimtop   ytop   edge-room)))
      (flet ((shift-vec (v)
	       (incf (aref v 0) dx)
	       (incf (aref v 1) dy)))
	(loop for v in (list o-vector e-vector el-vector n-vector nl-vector)
	      do (shift-vec v))))
      
    ;;
    (pgplot:with-pgplot-settings (p  :line-width weight)
      (pgplot:draw-arrow p
			 (aref o-vector 0) (aref o-vector 1)
			 (aref n-vector 0) (aref n-vector 1)
			 :color color :size 0.8)
      (pgplot:draw-arrow p
			 (aref o-vector 0) (aref o-vector 1)
			 (aref e-vector 0) (aref e-vector 1)
			 :color color :size 0.8)
      
      (pgplot:write-text p "N" (aref nl-vector 0) (aref nl-vector 1)
			 :center t :character-height 1.4
			 :color color)
      (pgplot:write-text p "E" (aref el-vector 0) (aref el-vector 1)
			 :center t :character-height 1.4
			 :color color))
    

    ))
    
      
    


(defmethod render-marker ((the-crosshairs crosshairs) (p pgplot:pgplot)
			   &key  x0 y0 size wcs)
  (let ((xx (fsm-x the-crosshairs))
	(yy (fsm-y the-crosshairs))
	(color (fsm-color the-crosshairs))
	(weight (fsm-weight the-crosshairs))
	(ri (* size (r-inner the-crosshairs))) ;; convert to pixel units
	(ro (* size (r-outer the-crosshairs))))
    (multiple-value-bind (x y) ;; convert to pixel units
	(convert-xy-to-pixels xx yy (coordinate-units the-crosshairs)
			      x0 y0 size wcs)
      (loop for xvec in (list
			 (vector (+ x ri) (+ x ro))
			 (vector (- x ri) (- x ro))
			 (vector x x)
			 (vector x x))
	    for yvec in (list
			 (vector y y)
			 (vector y y)
			 (vector (+ y ri) (+ y ro))
			 (vector (- y ri) (- y ro)))
	    do
	       (pgplot:connect p xvec yvec
			       :color color :line-width weight)))))
    
(defmethod render-marker ((the-circle circle) (p pgplot:pgplot)
			  &key x0 y0 size wcs)
  (multiple-value-bind (x y) ;; convert to pixel units
      (convert-xy-to-pixels
       (fsm-x the-circle) (fsm-y the-circle)
       (coordinate-units the-circle)
       x0 y0 size wcs)
    (pgplot:circle p
		   x y
		   (r the-circle)
		   :color (fsm-color the-circle)
		   :line-width (fsm-weight the-circle))))

(defmethod render-marker ((the-text-marker text-marker)  (p pgplot:pgplot)
			  &key x0 y0 size wcs)
  (multiple-value-bind (xx yy) ;; convert to pixel units
      (convert-xy-to-pixels
       (fsm-x the-text-marker) (fsm-y the-text-marker)
       (coordinate-units the-text-marker)
       x0 y0 size wcs)
    (let ((x (+ xx
		(* size (xoffset the-text-marker))))
	  (y (+ yy
		(* size (yoffset the-text-marker)))))
      (if (blanked the-text-marker)
	  ;;
	  (pgplot:write-blanked-text
	   p (text the-text-marker) x y 
	   :center (centered the-text-marker)
	   :character-height (character-height the-text-marker)
	   :color (fsm-color the-text-marker)
	   :angle (angle the-text-marker) 
	   :font  (font the-text-marker)
	   :blanking-color (blanking-color the-text-marker))
	  ;;
	  (pgplot:write-text 
	   p (text the-text-marker) x y
	   :center (centered the-text-marker)
	   :character-height (character-height the-text-marker)
	   :color (fsm-color the-text-marker)
	   :angle (angle the-text-marker) 
	   :font  (font the-text-marker))))))




(defmethod render-marker ((the-scale pixel-scale-marker) (p pgplot:pgplot)
			  &key x0 y0 size wcs)
  (declare (ignorable wcs))
  (let* ((pix-scale
	   (or (pixel-scale the-scale) ;; first the pix scale, if given
	       (if wcs    ;; or the wcs given
		   (wcs:get-pixel-scale-for-wcs wcs))
	       (error "Cannot compute pixel scale; scale not given in PIXEL-SCALE-MARKER and no WCS found.")))
	 ;; compute the bar length (arcsec) to fill up at least 0.15 of image
	 (blen (or (bar-length the-scale)
		   (loop with blen = 0.0001  ;; start with ridiculously small scale
			 for i below 20  ;; don't loop forever
			 for jump = (aref #(2 2.5 2) (mod i 3))
			 for pixlen = (/ blen pix-scale)
			 do
			    (if (> pixlen (* 0.15 size))
				(return blen)
				(setf blen (* blen jump)))
			 finally (error "Could not find pixel scale in render-marker"))))
	 ;; the length in pixels
	 (plen (/ blen pix-scale))
	 ;; textual representation of scale
	 (stext (if (< blen 0.0)
		    (format nil "~,4F\"" blen)
		    (format nil "~D\"" (round blen))))
	 ;; start and end of line
	 (xa (+ (* 0.05 size) (- x0 (* size 0.5))))
	 (xb (+ xa plen))
	 (y  (+ (* 0.04 size) (- y0 (* size 0.5)))))
    ;;
    (pgplot:connect p (vector xa xb) (vector y y)
		    :color (fsm-color the-scale)
		    :line-width (fsm-weight the-scale))
    (loop for x in (list xa xb) ;; vertical bars
	  do  (pgplot:connect p (vector x x)
			      (vector (+ y (* size 0.01)) (- y (* size 0.01)))
			      :color (fsm-color the-scale)
			      :line-width (fsm-weight the-scale)))
    ;;
    (pgplot:with-pgplot-settings (p :line-width (fsm-weight the-scale))
      (pgplot:write-text p stext (* 0.5 (+ xa xb)) (+ y (* 0.03 size))
			 :character-height 1.5
			 :center t :color (fsm-color the-scale)))))




(defun make-stamp (fits-file plot-type x0 y0 size
		   &key
		     (output-file nil)
		     extension
		     wcs
		     toplabel
		     ;;
		     (scaling :linear)
		     (sigma-stretch nil) ;; defaults set below
		     (sigma-offset  nil)
		     (num-labels nil) (markers nil)) ;; markers of type fits-stamp-marker
			 
			 
  "Extract a stamp from EXTENSION (default first image extension)
from FITS_FILE at pixel x0,y0 of SIZE x SIZE in pixels.  The stamp
has PLOT-TYPE, defaulting to PDF.

Without EXTENSION specified, the extensions 0 and 1 are attempted.

WCS is an override for any WCS in the fits image, used for the scale,
the compass rose, and for placing markers at world coordinate positions

SIGMA-STRETCH sets the min/max range of the image colormap, multiplying
the sigma obtained from the 16%-84% span of the image.

SIGMA-OFFSET is how many sigma below image median the center point of the 
color scale is set.

SCALING is :LINEAR by default
  if :LOG, then the image is remapped to 
     x=Image-backd; NewImage=sign(x)*ln(1+abs(x))
  if :SQRT, then the image is remapped to 
     x=Image-backd; NewImage=sign(x)*sqrt(abs(x))

NUM-LABELS is whether pixel numbers should be on the axes.

Returns T/NIL on success/failure.
"

  (when (and (not (eq plot-type :x11))
	     (not output-file))
    (error "OUTPUT-FILE is not specified, but PLOT-TYPE is not :X11"))
  
  ;; set some reasonable scalings if not given
  (when (not sigma-stretch)
    (setf sigma-stretch 
	  (cond ((eq scaling :linear) 5)
		((eq scaling :log)    3)
		((eq scaling :sqrt)   3))))
  (when (not sigma-offset)
    (setf sigma-offset
	  (cond ((eq scaling :linear) -3)
		((eq scaling :log)    -0.5)
		((eq scaling :sqrt)   -1.0))))
  
		
  
  
  (let* ((p nil) ;; plot device
         (nx0 (round x0))
         (ny0 (round y0))
         (hs (ash size -1)) ;; halfsize
         (nxmin (- nx0 hs))
         (nxmax (+ nx0 hs))
         (nymin (- ny0 hs))
         (nymax (+ ny0 hs))
         medval sigma
	 (the-wcs (or wcs (ignore-errors (cf:read-wcs fits-file :extension extension))))
	 (null-value -1e-30) ;; what NULL pixels will be set to by cfitsio
	 (out-of-bounds-value -2e30)
	 img bitarray)
	 

    (declare (ignorable bitarray) )

    (cf:with-open-fits-file (fits-file ff) 
      (cond ;; use extension given
	(extension (cf:move-to-extension ff extension))
	;; or try to find an image extension in HDU 1 or 2
	((not extension)
	 (cond
	   ;; first extension has an image
	   ((= 2 (cf:fits-file-current-image-ndims ff))
	    nil)
	   ;; else try 2nd extension
	   ((>= (cf:fits-file-num-hdus ff) 1)
	    (cf:move-to-extension ff 2)
	    (when (not (= 2 (cf:fits-file-current-image-ndims ff)))
	      (error
	       "make-pdf-stamp: failed to find an image extension in HDU 1 or 2 in fits file ~A"
	       fits-file))))))

      ;;
      (multiple-value-setq (img bitarray)
	(cf:read-fits-image-subsection-into-array     
	 ff nxmin nymin nxmax nymax
	 :null-value  null-value
	 :out-of-bounds-value out-of-bounds-value)))

	     ;;
    (when img
      ;; compute median and sigma
      (float-utils:with-float-traps-masked
	  (:inexact t :overflow t :underflow t :invalid t)
	(loop for k below (array-total-size img)
	      for pixval of-type single-float = (row-major-aref img k)
	      when (not (or (= pixval null-value)
			    (= pixval out-of-bounds-value)
			    (float-utils:float-nan-p pixval)
			    (float-utils:float-infinity-p pixval)))
		collect pixval into pixlist
	      finally
		 (let ((v (make-array (length pixlist)
				      :element-type 'single-float
				      :initial-contents pixlist)))
		   (setf medval
			 (fastmedian:fast-single-float-1d-array-median v))
		   (setf
		    sigma
		    (* 0.5
		       (- (fastmedian:fast-single-float-1d-array-fraction
			   v 0.84)
			  (fastmedian:fast-single-float-1d-array-fraction
			   v 0.16))))))

	;; if log, then remap it
	(when (eq scaling :log)
	  (loop
	    for k below (array-total-size img)
	    for pixval of-type single-float = (row-major-aref img k)
	    when (not (or (= pixval null-value)
			  (= pixval out-of-bounds-value)
			  (float-utils:float-nan-p pixval)
			  (float-utils:float-infinity-p pixval)))
	      do
		 (let* ((x (- pixval medval))
			(sx (if (minusp x) -1 1)))
		   (setf (row-major-aref img k)
			 (float (* sx (log (+ 1d0 (abs x))))
				1.0))))
	  ;; and readjust sigma
	  (setf sigma (log sigma))
	  ;; and median is now at zero
	  (setf medval 0d0))

	;; if sqrt, then remap it
	(when (eq scaling :sqrt)
	  (loop
	    for k below (array-total-size img)
	    for pixval of-type single-float = (row-major-aref img k)
	    when (not (or (= pixval null-value)
			  (= pixval out-of-bounds-value)
			  (float-utils:float-nan-p pixval)
			  (float-utils:float-infinity-p pixval)))
	      do
		 (let* ((x (- pixval medval))
			(sx (if (minusp x) -1 1)))
		   (setf (row-major-aref img k)
			 (float (* sx (sqrt (abs x)))
				1.0))))
	  ;; and readjust sigma
	  (setf sigma (sqrt sigma))
	  ;; and median is now at zero
	  (setf medval 0.0))


	(let* ((centval
		 (- medval (* sigma-offset  sigma)))
	       (val1 (- centval (* sigma-stretch sigma)))
	       (val2 (+ centval (* sigma-stretch sigma))))
	  ;; now wipe out the bad values to be val1
	  (loop
	    for k below (array-total-size img)
	    for pixval of-type single-float = (row-major-aref img k)
	    when  (or (= pixval null-value)
		      (= pixval out-of-bounds-value)
		      (float-utils:float-nan-p pixval)
		      (float-utils:float-infinity-p pixval))
	      do (setf (row-major-aref img k) val1))			  
		


      (unwind-protect ;; robust closing of device
	   (progn
	     (setf p (pgplot:open-device plot-type :filename output-file :square t))
	     (pgplot:set-character-font p :default)
	     (if num-labels ;; extra padding for axis labels
		 (pgplot:set-viewport p 0.10 0.95 0.05 0.90)
		 (pgplot:set-viewport p 0.10 1.0 0.05 0.95))
	     (pgplot:set-window p nxmin nxmax nymin nymax)
	     (pgplot:insert-colormap p :gray :imin 16 :imax 99) ;; max gray map

	     (pgplot:image p img :type :gray :reverse t
				 :val1 val1
				 :val2 val2)))
	  
	     
	     (pgplot:box p :size 1.2 :x-num-labels-bottom num-labels
			   :y-num-labels-left num-labels)
		      
	     (when toplabel
	       (pgplot:toplabel p toplabel :character-height 1.4
					   :y-offset -0.02))
		      
	     (loop for marker in markers
		   when marker ;; NIL markers do nothing
		     do
			(flet ((do-marker ()
				 (render-marker
				  marker p
				  :x0 x0 :y0 y0 :size size
				  :wcs the-wcs)))
			  (if (fail-silently marker)
			      (ignore-errors (do-marker))
			      (do-marker))))
		      
	     (when output-file (pgplot:close-device p))
	     t))) ;; return T that we made a stamp
    ;;
    (progn ;; unwind protected form to ensure device is really
      ;; closed if there was an error
      (ignore-errors
       (when (and (pgplot:pgplot-p p)
		  output-file
		  (pgplot:is-open p))
	 (pgplot:close-device p)))))
  t) ;; return T for success if entire call to make-stamp is wrapped in ignore-errors


(defun make-pdf-stamp (fits-file pdf-file
		       x0 y0 size
		       &key
			 extension
			 wcs
			 toplabel
			 (scaling :linear)
			 (sigma-stretch nil) ;; defaults set below
			 (sigma-offset  nil)
			 (num-labels nil)
			 (markers nil)) ;; markers of type fits-stamp-marker
  "A trivial wrapper for MAKE-STAMP that restricts type to PDF"
  (make-stamp fits-file :pdf x0 y0 size  
	      :output-file pdf-file
	      :extension extension :wcs wcs :toplabel toplabel
	      :scaling scaling :sigma-stretch sigma-stretch
	      :sigma-offset sigma-offset :num-labels num-labels
	      :markers markers))
