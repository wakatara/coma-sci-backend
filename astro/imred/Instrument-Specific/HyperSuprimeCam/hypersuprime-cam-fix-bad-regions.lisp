

(in-package hypersuprime-cam-fix)

;; do not show up in data
(defparameter *focus-chips* '(110 111 108 109 106 104 107 105))

;; should never show up
(defparameter *autoguide-chips* '("AG114" "AG115" "AG112" "AG113"))

;; the vignetted chips
(defparameter *edge-chips*
  '( 90 95 102 103 70 77 62 69 54 61 46 
    53 38 45 30 37 22 29 100 101 4 9
    0 3 96 99))



(defun mask-bad-hsc-region (image det-id)
  (declare (type imutils:image))
  (cond

    ((eql det-id 90)
     (imutils:imfill-corner image 340 1920 :bottom-right float-utils:*single-float-nan*))
    ;;
    ((eql det-id 95)
     (imutils:imfill-above/below-line image 0 3800 2048 1685 :above float-utils:*single-float-nan*))
    ;;
    ((eql det-id 102)
     (imutils:imfill-corner image 2040 2750 :bottom-left float-utils:*single-float-nan*))
    ;;
    ((eql det-id 103)
     (imutils:imfill-above/below-line image 0 600 2048 3460 :below float-utils:*single-float-nan*))
    ;;
    ((eql det-id 70)
     (imutils:imfill-above/below-line image 0 650 2048 1440 :below float-utils:*single-float-nan*))
    ;;
    ((eql det-id 77)
     (imutils:imfill-above/below-line image 0 1770 2048 1000 :below float-utils:*single-float-nan*))
    ;;
    ((eql det-id 62)
     (imutils:imfill-above/below-line image 0 0 2048 565 :below float-utils:*single-float-nan*))
    ;;
    ((eql det-id 69)
     (imutils:imfill-above/below-line image 0 950 2048 640 :below float-utils:*single-float-nan*))
    ;;
    ((eql det-id 54)
     T) ;; OK, but hot columns
    ;;
    ((eql det-id 61)
     (imutils:imfill-above/below-line image  0 400 2048 0 :below float-utils:*single-float-nan*))
    ;;
    ((eql det-id 46)
     T) ;; OK
    ;;
    ((eql det-id 53)
     (imutils:imfill-above/below-line image 0 200 2048 200 :below float-utils:*single-float-nan*))
    ;;
    ((eql det-id 38) ;; dead rows at top
     (imutils:imfill-edge image 3 :top float-utils:*single-float-nan*))
    ;;
    ((eql det-id 45)
     (imutils:imfill-above/below-line image 0 150 2048 420 :below float-utils:*single-float-nan*))
    ;;
    ((eql det-id 30)
     (imutils:imfill-above/below-line image 0 750 2048 120 :below float-utils:*single-float-nan*))
    ;;
    ((eql det-id 37)
     (imutils:imfill-above/below-line image 0 400 2048 800 :below float-utils:*single-float-nan*))
    ;;
    ((eql det-id 22)
     (imutils:imfill-above/below-line image 0 1500 2048 800 :below float-utils:*single-float-nan*))
    ;;
    ((eql det-id 29)
     (imutils:imfill-above/below-line image 0 850 2048 1600 :below float-utils:*single-float-nan*))
    ;;
    ((eql det-id 100)
     (imutils:imfill-above/below-line image 120 0  2048 2800 :below float-utils:*single-float-nan*))
    ;;
    ((eql det-id 101)
     (imutils:imfill-above/below-line image 0 3200 2048 0 :below float-utils:*single-float-nan*))
    ;;
    ((eql det-id 4)
     (imutils:imfill-above/below-line image 200 4175 2048 2300 :above float-utils:*single-float-nan*))
    ;;
    ((eql det-id 9) ;; check me - this chip was non-functional when code was written
     (imutils:imfill-above/below-line image 0 2275 1400 4175 :below float-utils:*single-float-nan*))
    ;;
    ((eql det-id 0) ;; this one has dead amp
     (imutils:imfill-corner image 1500 3500 :top-right float-utils:*single-float-nan*))
    ;;
    ((eql det-id 3)
     (imutils:imfill-corner image 1040 2000 :bottom-right float-utils:*single-float-nan*))
    ;;
    ((eql det-id 96)
     (imutils:imfill-corner image 1500 900 :bottom-right float-utils:*single-float-nan*))
    ;;
    ((eql det-id 99)
     (imutils:imfill-corner image 1360 3000 :top-right float-utils:*single-float-nan*)
     (imutils:imfill-edge image 5 :top float-utils:*single-float-nan*))
    ;;
    ;;
    (t ;; other chips are good
     t)))
    
    
  
