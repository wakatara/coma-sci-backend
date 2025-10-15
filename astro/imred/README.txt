
Routines for providing IRAF imred like reduction of ccd images.

Provided:

     automatic description and matching of images, by extension
     trimming
     biases (zeros)
     flats, by filter keyword
     normalizing by gain (ie, make gain=1)
     ccdproc = trim+bias+flatten+normalize (all optional)

     combined processing of a fits list using process-set.lisp

     

A reduction is defined by a REDUCTION-PLAN object, which
may be subclassed for various instruments.

For example, one might call
 
    (imred:process-fits-list 
     *fits-list* 
      (make-instance 'imred:reduction-plan-tek88
                      :target-dir "./TargetDir"))

to do a full reduction of images in *fits-list*, placing
results in "./TargetDir"

================================================================

Headers:

Input:
(set in the REDUCTION-PLAN structure; the following are examples)
IMAGTYP   BIAS/FLAT/OBJECT
GAIN      1.0
FILTER    B



Created: 

DEBIAS    UTDATE   // Bias Subtracted
FLATTEN   UTDATE   // Flattened
MJD       xxxxx    // MJD start
MJDSTART  xxxxx    // MJD start
MJDMID    xxxxx    // MJD midpoint


================================================================
