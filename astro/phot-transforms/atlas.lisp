
(in-package phot-transforms)

;; From Tonry et al (2018), ATLAS: A High-cadence All-sky Survey System,
;; Pub. Astron. Soc. Of Pacific 130:064505.

;; we use AB bands instead of SDSS becauae ATLAS is a bit vague, but can probably
;; use SDSS or PS1 because I doubt these are exact, and vary by telescope
(defun catlas-from-gab+rab (gab rab)  ;; aargh we're changing nomenclature
  (+ (* 0.49 gab) (* 0.51 rab)))

(defun oatlas-from-rab+iab (rab iab)
  (+ (* 0.55 rab) (* 0.45 iab)))

(defun gab-from-catlas+oatlas (catlas oatlas)
  (+ (* 1.67 catlas) (* -0.67 oatlas)))

(defun rab-from-catlas+oatlas (catlas oatlas)
  (+ (* 0.35 catlas) (* 0.65 oatlas )))

(defun iab-from-catlas+oatlas (catlas oatlas)
  (+ (* -0.39 catlas) (* 1.39 oatlas )))


;; these are all a bit shaky, depending on two-stage transforms
(defun BJ-from-catlas+oatlas (catlas oatlas)
  (let ((gab (gab-from-catlas+oatlas catlas oatlas))
	(rab (rab-from-catlas+oatlas catlas oatlas)))
    (bj-from-grps1 gab rab))) ;; pretend that PS1 is AB (closer to AB than SDSS)

(defun VJ-from-catlas+oatlas (catlas oatlas)
  (let ((gab (gab-from-catlas+oatlas catlas oatlas))
	(rab (rab-from-catlas+oatlas catlas oatlas)))
    (vj-from-grps1/g gab rab))) ;; pretend that PS1 is AB (closer to AB than SDSS)

(defun Rc-from-catlas+oatlas (catlas oatlas)
  (let ((gab (gab-from-catlas+oatlas catlas oatlas))
	(rab (rab-from-catlas+oatlas catlas oatlas)))
    (rc-from-grps1 gab rab))) ;; pretend that PS1 is AB (closer to AB than SDSS)

(defun IC-from-catlas+oatlas (catlas oatlas)
  (let ((gab (gab-from-catlas+oatlas catlas oatlas))
	(rab (rab-from-catlas+oatlas catlas oatlas))
	(iab (iab-from-catlas+oatlas catlas oatlas)))
    (ic-from-grips1 gab rab iab))) ;; pretend that PS1 is AB (closer to AB than SDSS)
	

