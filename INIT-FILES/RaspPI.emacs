;; minimal .emacs for raspberry pi                                                                     

(add-to-list 'load-path (concat (getenv "LISP_LIB") "/slime"))

(package-initialize)
(require 'slime-autoloads)

(require 'vc-git)  ;; otherwise seems to barf when used in GIT trees
(require 'cl) ;; cl-style stuff, like list*
                                                                                       
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))


;; run as superuser to allow access to hardware
;;(setf inferior-lisp-program "sudo -E /usr/local/bin/armcl"
;;      slime-contribs '(slime-fancy))

;; non-superuser
;;(setf inferior-lisp-program "/usr/local/bin/armcl"
;;      slime-contribs '(slime-fancy))


(slime-setup '(
	       slime-repl
	       slime-asdf
	       slime-autodoc
	       slime-editing-commands
	       slime-fancy-inspector
	       slime-fontifying-fu
	       slime-fuzzy
	       slime-indentation
	       ;slime-mdot-fu ; this 'does not work at the moment'
	       slime-package-fu
	       slime-references
	       slime-repl
	       slime-sbcl-exts
	       slime-scratch
	       slime-xref-browser
	       ))

(setq slime-lisp-implementations
      `((ecl   ("/usr/bin/ecl" ))
      	(sbcl   ("/usr/bin/sbcl"))
      	(sbcl-suid   ("/usr/bin/sbcl.suid"))
        ;; ccl doesn't work
		   
	))

;; sbcl with its own inferior-lisp buffer
(defun slime-sbcl ()
  (interactive)
  (let ((slime-net-coding-system 'utf-8-unix))
    (apply #'slime-start
         (list* :buffer "*inferior-lisp-sbcl*" 
                (slime-lookup-lisp-implementation slime-lisp-implementations
                                                  'sbcl)))))

(defun slime-sbcl-suid ()
  (interactive)
  (let ((slime-net-coding-system 'utf-8-unix))
    (apply #'slime-start
         (list* :buffer "*inferior-lisp-sbcl*" 
                (slime-lookup-lisp-implementation slime-lisp-implementations
                                                  'sbcl-suid)))))



(defun slime-ecl ()
  (interactive)
  (let ((slime-net-coding-system 'utf-8-unix))
    (apply #'slime-start
         (list* :buffer "*inferior-lisp-ecl*" 
                (slime-lookup-lisp-implementation slime-lisp-implementations
                                                  'ecl)))))


(progn ;; window system stuff                                                                          
  ;; load library for doing better closure flashing                                                    
  (load-library "paren")
  (make-face 'paren-match) ;define our face for paren matching                                         
  (set-face-background 'paren-match "darkblue")
  (set-face-foreground 'paren-match "red")
  (show-paren-mode)
  ;;put some buffers in frames of their own                                                            
  (setq special-display-buffer-names '("*Help*" "*compilation*" ) )

  (add-hook 'c-mode-hook 'turn-on-font-lock)
  (add-hook 'scheme-mode-hook 'turn-on-font-lock)
  (add-hook 'tex-mode-hook 'turn-on-font-lock)
  (add-hook 'latex-mode-hook 'turn-on-font-lock)
  (add-hook 'lisp-mode-hook 'turn-on-font-lock)
  (add-hook 'ilisp-mode-hook 'turn-on-font-lock)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; on RPI emacs starts with weird tiny height
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 80) ; chars
              (height . 60) ; lines
              (background-color . "honeydew")
              ;(left . 50)
              ;(top . 50)
	      ))
      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 106)
              (height . 60)
              (background-color . "honeydew")
              (left . 50)
              (top . 50))))
  (progn
    (setq initial-frame-alist '( (tool-bar-lines . 0)))
    (setq default-frame-alist '( (tool-bar-lines . 0)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
