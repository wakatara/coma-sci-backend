#|

Routines to generate a background map efficiently using the observations that

- A ring median gets the desired result, but is computationally expensive. The central exclusion
seems desirable to suppress point sources.

- If N points are used to compute the background map, then the relative noise contribution of
the background map is 1/sqrt(N).  So the total noise in a background
subtracted image is sqrt(sigma^2+ [sigma/sqrt(N)]^2)=1.01 sigma for N=50 and 1.005 for N=100

- It isn't necessary to compute the background at every point, just some representative spacing
of points, and then smooth. So a 4x4 spacing would gain a factor of 16 in execution speed.







|#



(in-package imutils)




;; perform a simple 1,n/2,2,n/2+1,3 card shuffle.  The last element
;; stays in place if N is odd
(defun %cardshuffle-intarr (vec &optional vscratch)
  (declare (type (simple-array simindex (*)) vec)
	   (type (or null (simple-array single-float (*))) vscratch))
  (let* ((n (length vec))
	 (vscr (or vscratch (make-array n :element-type 'simindex))))
    (loop for i below n do (setf (aref vscr i) (aref vec i)))
    (loop with n/2 = (ash n -1)
	  with k of-type simindex = 0
	  for i below n/2
	  for j = (+ i n/2)
	  do
	     (setf (aref vec k) (aref vscr i))
	     (incf k)
	     (setf (aref vec k) (aref vscr j))
	     (incf k))
    vec))

;; Create vectors (values nxvec nyvec) of relative indices for
;; scattered points covering a ring pattern centered on zero.  These
;; representing a sparse, reasonably uniform sampling pattern.  They
;; are shuffled at the end, so even a subsequence should be reasonably
;; uniform.
(defun %generate-ring-median-template (nrinner nrouter nvals)
  (declare (type (integer 1 #.(expt 2 20)) nrinner nrouter nvals))
  (let* ((ndxvec (make-array nvals :element-type 'simindex))
	 (ndyvec (make-array nvals :element-type 'simindex))
	 ;; for shuffle
	 (nr (1+ (- nrouter nrinner)))
	 (ntheta (ceiling nvals nr))
	 (fpi (float pi 1.0))
	 (phi (* 0.5 (/ fpi ntheta)))) ;; u
    (loop with itot = 0 for ir from nrinner to nrouter
	  until (= itot nvals)
	  for dtheta = 0.0 then (+ dtheta phi) ;; offset angle of rings in r
	  do
	     (loop for itheta below ntheta
		   for theta = (+ dtheta (* itheta (/ (* 2 fpi) ntheta)))
		   for cost = (cos theta)
		   for sint = (sin theta)
		   until (= itot nvals)
		   do (setf (aref ndxvec itot) (round (* cost ir))
			    (aref ndyvec itot) (round (* sint ir)))
		      (incf itot)))
    ;; now shuffle the points, so that the points are more evenly
    ;; distributed in index (ie don't just spiral outwards, so a sequential
    ;; subsequence is more likely to give an even sample of the annulus)
    (loop for i below 4 ;; 4 shuffles
	  with tmpvec = (make-array nvals :element-type 'simindex)
	  do
	     (%cardshuffle-intarr ndxvec)
	     (%cardshuffle-intarr ndyvec))
	 ;;
    (values ndxvec ndyvec)))


;; using the sparse template of NDXVEC,NDYVEC compute the median
;; around NX0,NY0 in IM, using at most NSAMP points from the template.
;; IMFLAG is an optional flag array that should be 0 where the image is good.
;; If COMPUTE-SIGMA is true, then compute the sigma using quartiles.
;; VSCRATCH is an array of at least length NSAMP for computing the median and quartiles.
(defun %sample-median-using-template (im nx0 ny0 ndxvec ndyvec nsamp vscratch
				      bitimflag compute-sigma
				      ;; the default values if there are no data points
				      default-median default-sigma)
  (declare (type image im)
	   (type (or null bit-image) bitimflag)
	   (type (simple-array simindex (*)) ndxvec ndyvec)
	   (type (integer 1 #.(expt 2 20)) nsamp)
	   (type (or null (simple-array single-float (*))) vscratch)
	   (optimize speed))
  (let ((ngood 0)
	(nxmax (array-dimension im 1))	
	(nymax (array-dimension im 0))
	(vmed (or vscratch (make-array nsamp :element-type 'single-float))))
    (declare (type simindex ngood)
	     (type (simple-array single-float (*)) vmed))
    (loop with imax = (length ndxvec) ;; max in sampling template
	  with k of-type simindex = 0 ;; index of successful insertions into vmed
	  for i of-type simindex from 0 ;; index of position in ndxvec,ndyec
	  while (and (< i imax)  ;; can't exceed sampling template
		     (< k nsamp)) ;; get no more than this many samples
	  for jx = (+ nx0 (aref ndxvec i))
	  for jy = (+ ny0 (aref ndyvec i))
	  when (and
		;; is this sample point on the image?
		(< -1 jx nxmax)
		(< -1 jy nymax)
		;; if flagging, is this flag location zero (good)?
		(or (not bitimflag)
		    (not (plusp (aref bitimflag jy jx)))))
	    do (setf (aref vmed k) (aref im jy jx))
	       (incf k)
	  finally (setf ngood k))
    (let ((med (if (plusp ngood)
		   (fastmedian:fast-single-float-1d-array-median vmed ngood)
		   0.0))
	  (sigma 0.0))
      (when (and compute-sigma (plusp ngood))
	(let ((q25 (fastmedian:fast-single-float-1d-array-fraction vmed 0.25 ngood))
	      (q75 (fastmedian:fast-single-float-1d-array-fraction vmed 0.75 ngood)))
	  (setf sigma (/ (- q75 q25) 1.34897950039216))))
      (values med sigma))))


;; Generate an backd image and optional sigma image, but down-binned
;; from IM by NBIN. 
;; 
;; Arrays IMBACKD and IMSIGMA (not not NIL) are filled, so no return value
(defun %fill-downsampled-median-array
    (im ndxvec ndyvec nsamp nbin
     bitimflag  ;; flag image, 0 if good
     imbackd    ;; output backd image
     imsigma    ;; output sigma image - if not present, not filled
     default-median default-sigma)
  ;;
  (declare (type image im)
	   (type (simple-array simindex (*)) ndxvec ndyvec)
	   (type (integer 1 1024) nbin)
	   (type (integer 1 #.(expt 2 20)) nsamp)
	   (type (or null bit-image) bitimflag)
	   (type image imbackd)
	   (type single-float default-median default-sigma)
	   (type (or null image) imsigma)
	   (optimize (speed 3)))
  ;;
  (let* ((nx (array-dimension im 1))
	 (ny (array-dimension im 0))
	 ;; size of output array; mx*nbin >= nx (> if nx/nbin isn't an integer)
	 (mx (ceiling nx nbin)) 
	 (my (ceiling ny nbin)))

    (%check-dims-n imbackd mx my "IMBACKD")
    (when imsigma (%check-dims-n imsigma mx my "IMSIGMA"))
    ;;
    (loop with vscratch = (make-array nsamp :element-type 'single-float)
	  with nx-1 = (- nx 1)
	  with ny-1 = (- ny 1)
	  for iy of-type simindex below my  ;; index in downsampled/binned images
	  for jy of-type simindex = (* iy nbin) ;; location on IM to sample
	  do (loop for ix below mx ;; index in downsampled/binned images
	  	   for jx of-type simindex = (* ix nbin) ;; location on IM to sample 
		   do (multiple-value-bind (med sigma)
			  (%sample-median-using-template
			   im
			   ;; don't sample off IM because the template might not
			   ;; overlap the entire extension zone.  So if the downsampled
			   ;; image is bigger than IM, extend the edges.
			   (min jx nx-1) (min jy ny-1)
			   ndxvec ndyvec nsamp
			   vscratch bitimflag
			   (if imsigma t nil);; if imsigna not NIL, sigma is computed
			   default-median default-sigma)
			(declare (type single-float med sigma))
			(setf (aref imbackd iy ix) med)
			(when imsigma
			  (setf (aref imsigma iy ix) sigma)))))))



;; given a binned image IMB[MY,MX], a binning factor BIN,
;; and an IX,IY position in the parent unbinned image,
;; interpolate in IMB,IMS (if IMS is not NIL)
;; Return (values interpolated-backd interpolated-simga-or-zero)
(defun %interpolate-binned-backd-and-sigma (iy ix nbin mx my imb ims)
  (declare (type imindex ix iy)
	   (type (integer 1 1024) nbin)
	   (type image imb)
	   (type (or null image) ims)
	   (optimize (speed 3) (safety 0)))
  (let* ((xnbin (float nbin 1.0))
	 (x (/ ix xnbin))
	 (y (/ iy xnbin)))
    
    (multiple-value-bind (jx fx)
	(floor x)
      (multiple-value-bind (jy fy)
	  (floor y)
	(let ((1-fx (- 1.0 fx))
	      (1-fy (- 1.0 fy)))
	  (flet ((interp-array (arr)
		   (let* ((jx+1 (min (1+ jx) (1- mx))) ;; prevent going over edge of array
			  (jy+1 (min (1+ jy) (1- my)))
			  (a00 (aref arr jy jx))
			  (a01 (aref arr jy jx+1))
			  (a10 (aref arr jy+1 jx))
			  (a11 (aref arr jy+1 jx+1)))
		     ;;
		     (+ (* 1-fy 1-fx  a00)
			(* 1-fy fx    a01)
			(* fy   1-fx  a10)
			(* fx   fy    a11)))))
	    ;;
	    (values (interp-array imb)
		    (if ims (interp-array ims) 0.0))))))))

(declaim (inline %interpolate-binned-backd-and-sigma))
	      
      
(deftype backd-image-function ()
  `(function (simindex simindex)
	    (values single-float
		    (or null single-float)
		    (member t nil))))


(defun build-background-image-func (im
				    &key
				    (nbin 16)
				    (rinner 30)
				    (router 70)
				    (nsamp  512)
				    (compute-sigma t) ;; compute sigma 
				    (remove-sources nil)
				    (sigma-sources 2.5)
				    (im-flag nil)
				    (default-median 0.0)
				    (default-sigma 1e20))
  
  "Compute a background image for image IM using sparse ring median method,
using an intermediate binned image to reduce computational load.  Optionally
remove sources.

 Return a functional representation
 of the background
    FUNC = (lambda (iy ix) ..) ==> (VALUES BACKD-VALUE SIGMA-VALUE-OR-NIL FLAGGED-P)
 where FLAGGED-P is T if the point was flagged either originally or by star removal,
 and where SIGMA is NIL if COMPUTE-SIGMA-IMAGE was NIL.



Keyword arguments:

  NBIN - the binning level at which to perform the calculaton - eg '16' makes
         the intermediate image 256x smaller in memory and operations.
  RINNER,ROUTER - inner and outer radii of sparse sampling annulus
  NSAMP - number of sampling points inside the sampling annulus
  COMPUTE-SIGMA -  compute the sigma image of background as well
  REMOVE-SOURCES - perform first pass to remove sources above SOURCE-SIGMA
  SIMGA-REMOVE-SOURCES  - sigma level of pixels above background to remove as sources
  IM-FLAG - optional flag or bit image, zero where image has valid pixels
  IM-BACKD-OUT, IM-SIGMA-OUT, IM-FLAG-OUT - optional output images to overwrite.
  DEFAULT-MEDIAN, DEFAULT-SIGMA - The values of median and sigma to use, if
                                  sampling yields nothing. 
"


  (declare (type image im)
	   (type (integer 1 1024) nbin)
	   (type (integer 1 #.(expt 2 20)) nsamp)
	   (type (or (float 1.0 10000) (integer 1 10000)) rinner router)
	   (type single-float sigma-sources)
	   (type (or null bit-image flag-image) im-flag)
	   ;;
	   (optimize speed))
	   
  
  (when im-flag      (%check-dims2 im im-flag      "IM vs IM-FLAG-OUT"))
  (let* ((nrinner (round   rinner))
	 (nrouter (ceiling router))
	 (nx (array-dimension im 1))
	 (ny (array-dimension im 0))
	 (im-bit-out-or-null
	   (if (or remove-sources im-flag)
	       (make-bit-image ny nx :initial-value 0)))
	 ;; binned images
	 (mx (ceiling nx nbin)) 
	 (my (ceiling ny nbin))
	 (imb/binned (make-image my mx))
	 ;; will need a sigma image for computing sources
	 (ims/binned-or-null (if (or compute-sigma remove-sources)
				 (make-image my mx))))

    (declare (type (or null bit-image) im-bit-out-or-null)
	     (type image imb/binned)
	     (type (or image null) ims/binned-or-null))

    ;; if im-flag exists, then im-bit-out-r-null
    ;; must contain it to start with,
    (when im-flag
      (if (typep im-flag 'bit-image)
	  (loop for i below (array-total-size im-flag)
		do (setf (row-major-aref im-bit-out-or-null i)
			 (row-major-aref (the bit-image im-flag) i)))
	  (loop for i below (array-total-size im-flag)
		do (setf
		    (row-major-aref im-bit-out-or-null i)
		    (if (zerop (row-major-aref (the flag-image im-flag) i))
			0
			1)))))

    
    ;; make the sampling template twice as big as it needs to be
    ;; go get batter sampling at endges, corners, and heavily masked regions
    (multiple-value-bind (ndxvec ndyvec)
	(%generate-ring-median-template nrinner nrouter (* 2 nsamp))
      (declare (type (simple-array simindex (*)) ndxvec ndyvec))

      ;; function to create (lambda (iy ix) ...) => (values backd median flagged-p)
      (flet ((make-med-sigma-func (compute-sigma-this-iteration)
	       (%fill-downsampled-median-array im  
					       ndxvec ndyvec nsamp nbin
					       ;; im-flag-out has been copied from im-flag,
					       ;; or just zeros
					       im-bit-out-or-null 
					       imb/binned
					       ims/binned-or-null
					       default-median default-sigma) 
	       
	       ;; return function of original image indices that returns
	       ;; (VALUES BACKD SIGMA-OR-NULL PIXEL-FLAGGED-P)
	       ;;
	       ;; this let is not necessary, but makes it clear
	       ;; what variables we're closing over
	       (let ((%nbin nbin)
		     (%mx mx)
		     (%my my)
		     (%imb/binned imb/binned)
		     (%ims/binned-or-null
		       ;; to handle special case of not returning sigma
		       ;; but removing sources
		       (if compute-sigma-this-iteration ims/binned-or-null)))
		     
		 (lambda (iy ix)
		   (declare (type simindex iy ix))
		   (multiple-value-bind (backd sigma)
		       (%interpolate-binned-backd-and-sigma
			iy ix %nbin %mx %my
			%imb/binned
			;; we may need to compute sigma on first pass
			;; when rem
			%ims/binned-or-null)
		     (values backd
			     sigma
			     (if  im-bit-out-or-null
				  (= 1 (aref im-bit-out-or-null iy ix))
				  nil)))))))

	(the
	 backd-image-function
	 (if (not remove-sources)
	     (make-med-sigma-func compute-sigma)
	     ;;
	     ;; if removing sources, fix im-bit-out-or-null image wherever there's a source,
	     ;; and make make-med-sigma-func again
	     ;;
	     (progn
	       ;; use med-sigma-func-v1 to detect sources, then rebuild the
	       ;; IM-BIT-OUT-OR-NULL array to add these sources
	       (let ((med-sigma-func-v1 (make-med-sigma-func t))) ;; T=compute-sigma
		 (image-iterate (im iy ix)
		   ;; when IM exceeds BACKD by (SIGMA-REMOVE-SOURCES)*SIGMA,
		   ;; flag this pix with 1
		   (when (zerop (aref im-bit-out-or-null iy ix)) ;; a valid pixel
		     (multiple-value-bind (backd sigma)
			 (funcall med-sigma-func-v1 iy ix)
		       (when (> (abs (- (aref im iy ix) backd))
				(* sigma-sources sigma))
			 (setf (aref im-bit-out-or-null iy ix) 1)))))
		 (make-med-sigma-func compute-sigma)))))))))
	      
		    



	
		  
							 
(defun build-background-image (im &key
				    (nbin 16)
				    (rinner 30)
				    (router 70)
				    (nsamp  512)
				    (compute-sigma t) ;; compute sigma 
				    (remove-sources nil)
				    (sigma-sources 2.5)
				    (im-flag nil))
  
  "Using BUILD-BACKGROUND-IMAGE-FUNCTION, Compute a background image for
image IM using sparse ring median method, using an intermediate binned image to
reduce computational load.  Optionally
remove sources.

Return (BACKD-IMAGE SIGMA-IMAGE-OR-NULL BIT-FLAG-IMAGE BACKD-IMAGE-FUNCTION)
where the sigma image is computed only if COMPUTE-SIGMA
is true, and BIT-FLAG-IMAGE is a bit image that is 1
if a pixel was flagged.

Keyword arguments:

  NBIN - the binning level at which to perform the calculaton - eg '16' makes
         the intermediate image 256x smaller in memory and operations.
  RINNER,ROUTER - inner and outer radii of sparse sampling annulus
  NSAMP - number of sampling points inside the sampling annulus
  COMPUTE-SIGMA -  compute the sigma image of background as well
  REMOVE-SOURCES - perform first pass to remove sources above SOURCE-SIGMA
  SIMGA-REMOVE-SOURCES  - sigma level of pixels above background to remove as sources
  IM-FLAG - optional flag or bit image, zero where image has valid pixels
  IM-BACKD-OUT, IM-SIGMA-OUT, IM-FLAG-OUT - optional output images to overwrite."
		    
      
  (let* ((backd-sigma-func
	   (build-background-image-func
	    im
	    :nbin nbin :rinner rinner :router router
	    :nsamp nsamp :compute-sigma compute-sigma
	    :remove-sources remove-sources
	    :sigma-sources sigma-sources
	    :im-flag im-flag))
	 (ny (array-dimension im 0))
	 (nx (array-dimension im 1))
	 (imb (make-image ny nx))
	 (ims (if compute-sigma (make-image ny nx)))
	 (imbitflag (make-bit-image ny nx :initial-value 0)))
    (declare (type image imb)
	     (type (or null image) ims)
	     (type bit-image imbitflag))
    (image-iterate (im iy ix)
      (multiple-value-bind (backd sigma badpix)
	  (funcall (the function backd-sigma-func) iy ix)
	(setf (aref imb iy ix) backd)
	(when ims (setf (aref ims iy ix) sigma))
	(if badpix (setf (aref imbitflag iy ix) 1))))
    ;;
    (values imb ims imbitflag backd-sigma-func)))
	      
		   
	  
					   
      
  
	 

  




  
  

		   
	
				     
				     
				   


    
  


