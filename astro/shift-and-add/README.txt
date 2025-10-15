

Purpose: shift and add a set of fits files, with optional masking.  Uses WCS.


Main function

(shift-and-add-fits-list fits-list :saaplan saaplan)


in particular

(shift-and-add:shift-and-add-fits-list 
  fits-list 
  :saaplan (make-instance
    'shift-and-add:saaplan
    :object-name object
    :imageout-base outbase
    :observatory nil
    :locator (shift-and-add:build-orbit-locator comet-elem)
    :image-preproc
     (when static-sky-subtract
	     (make-instance 'shift-and-add:static-sky-subtract-preproc
	                    ;; optional first estimate of object to remove
			    ;; before creating the sky
	                    :pre-subtract-fits  "OBJ_ESTIMATE_1.fits"
			    :pre-subtract-radius 80
		     ))
    :image-weighter
	   (when mask
	     (make-instance 'shift-and-add:stacked-masker
	                     :fwhm-factor 2.5)))


The LOCATOR is an object that can locate the object in each frame,
and is here constructed from an instance of slalib:comet-elem.
Another possibility is a RATE-LOCATOR (see object-locators.lisp).

The IMAGE-PREPROC is an optional preprocessor, here a sky subtractor.
(see preprocess.lisp and static-sky-subtract.lisp)

The IMAGE-WEIGHTER is an optional image-weigher, here a masker
that masks out stars in the stack to 2.5 x their FWHM.



================================================================

Some intermediate files  assuming inputs are FOOx.fits  and object is OBJ



* FOOx_presub.fits - if static sky subtract is used with pre-subtract-fits, this is
   FOO_x.fits minus the initial object estimate

* FOOx_skysub.fits - FOOx.fits minus the sky (if sky subtraction used)


* FOOx_sky.fits    - the sky frame for FOOx.fits, if no pre-subtract
  FOOx_skyPS.fits  - the sky frame for FOOx.fits, if using pre-subtract

* OBJ_stack.fits   - the desired output

* OBJ_stack_backdsky.fits - the background sky estimate that is to be subtracted
  from individual frames.


