
#+(or sbcl ccl) (pushnew :have-nr-wavelets cl:*features*)
(asdf:defsystem imutils
  :depends-on (fastmedian
	       llsq
	       fftw3lib
	       cffi ;; for erf - assume libmath is loaded
	       #+have-nr-wavelets nrwavelets 
	       ;; matlisp is just a pain
	       ;#+sbcl matlisp 
	       matrix
	       powell
	       golden-section
	       linterp
	       bootstrap
	       kdtree-jk)
    ;;
    :components
  ((:file "imutils-package" )
   (:file "image-utils" :depends-on ("imutils-package"))
   (:file "functional-images" :depends-on ("image-utils"))
   (:file "func-utils"  :depends-on ("imutils-package"))
   (:file "imarith" :depends-on ("image-utils"))
   (:file "graphics-algorithms" :depends-on ("imutils-package"))
   (:file "filters"  :depends-on ("imutils-package"))
   (:file "image-interp" :depends-on ("image-utils"))
   (:file "image-resample" :depends-on ("image-utils" "image-interp"))
   (:file "image-upsample" :depends-on ("image-utils" "image-interp"))
   (:file "imclean" :depends-on ("image-utils"))
   (:file "ring-stats" :depends-on ("image-interp" "imfit-quadratic"))
   (:file "image-stack"  :depends-on ("image-utils"))
   (:file "imstats" :depends-on ("image-utils"))
   (:file "convolve" :depends-on ("image-utils"))
   (:file "imfit-quadratic" :depends-on ("image-utils"))
   (:file "detections" :depends-on ("image-utils" "imfit-quadratic"))
   (:file "photometry-utils" :depends-on ("imutils-package"))
   (:file "ap-phot" :depends-on ("imstats" "photometry-utils"))
   (:file "backd-image" :depends-on ("image-utils" "photometry-utils"))
   (:file "ring-phot" :depends-on ("ring-stats" "photometry-utils"))
   (:file "test-images"  :depends-on ("image-utils"))
   #+have-nr-wavelets (:file "imwavelet" :depends-on ("image-utils" "test-images"))
   (:file "fft-image" :depends-on ("image-utils"))
   (:file "one-dim-project"  :depends-on ("image-interp" "image-interp"))
   (:file "image-smoothing" :depends-on ("image-utils"))
   (:file "subpixel-quadrature" :depends-on ("image-utils"))
   (:file "fit-gaussian-psf" :depends-on ("image-utils" "subpixel-quadrature"))
   (:file "fit-moffat-psf" :depends-on ("image-utils" "subpixel-quadrature"))
   (:file "fit-trailed-gaussian-psf"
    :depends-on ("image-utils" "subpixel-quadrature"))
   (:file "fit-1d-profile" :depends-on ())
   (:file "add-sources" :depends-on ("image-utils"
				     "func-utils"
				     "subpixel-quadrature"))
   (:file "moments" :depends-on ("image-utils" "subpixel-quadrature"))
   (:file "trail-image" :depends-on ("image-utils"))
   (:file "histogram-equalize" :depends-on ("image-utils"))
   (:file "simple-source-detector" 
    :depends-on ("image-utils"  "image-smoothing" "convolve" "backd-image"))
   (:file "psf-extract"
	  :depends-on ("image-utils" "image-resample"))
   (:file "static-sky-subtract" 
    :depends-on ("image-utils" "imarith" "imstats"))
   ;; perform cosmic ray rejection at detection location
   (:file "cosmic-ray-rejection" :depends-on ("image-utils" ))
   (:file "bad-rowcol-clean" :depends-on ("image-utils" "filters" "cosmic-ray-rejection"))
   (:file "imfill"     :depends-on ("image-utils" ))
   (:file "larson-sekanina-filter"     :depends-on ("image-utils" "image-interp"))
   ;; the following is in progress
   (:file "match-amplifier-boundaries" :depends-on ("image-utils" ))
   ))
