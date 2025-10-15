
;; return normalization imb using imb, using the boundary
;; im1 can be the same as im1

;; typical use would be to tweak amplifier gains in a CCD that
;; splits on amplifiers, or between chips in a close mosaic
;; that needs to be rebinned


(in-package imutils)

#|

The assumption is that 
  
   ixa0,iya0   ixa1,iya0    [is next to]  ixb0,iyb0   ixb1,iyb0 
   ixa0,iya0   ixa1,iya1                  ixb0,iyb0   ixb1,iyb1    

ie, that index ix1 of image a is adjacent index ix0 of image b.
It is allowed for ix1<ix0.

If :DIRECTION is :VERTICAL, then 
   
   ixa0,iya0   ixa1,iya0  
   ixa0,iya0   ixa1,iya1
         [is above]
   ixb0,iyb0   ixb1,iyb0
   ixb0,iyb0   ixb1,iyb1 

In each case, the distance is 'GAP'

|#

#+nil
(defun find-relative-amp-normalization
  (ima imb
       ;; block in ima to use
       ixa0 ixa1 iya0 iya1
       ;; block in imb to use
       ixb0 ixb1 ixb0 ixb1
       ;; direction to move from ima to imb
       &key
       (direction :horizontal) ;; or :vertical
       (gap 1.0) ;; the gap from last to first pixel
       (inverse nil) ;; return scaling to apply to a to match b
       (use-gradient nil) ;; fit a gradient across the gap
       (n-chunks 1)) ;; how many chunks to compute (and take their median)

  )
