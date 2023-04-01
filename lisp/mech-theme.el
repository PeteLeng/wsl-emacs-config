(defvar mech-theme 'box)

;; VS code theme
;; (straight-use-package 'vscode-dark-plus-theme)
;; (require 'vscode-dark-plus-theme)
;; (load-theme 'vscode-dark-plus t)

;; Tomorrow theme
(when (equal mech-theme 'tmr)
  ;; (straight-use-package '(tomorrow-theme :type git :host github :repo "ChrisKempson/Tomorrow-Theme"))
  ;; (require 'color-theme-tomorrow)
  
  (straight-use-package 'color-theme-sanityinc-tomorrow)
  (require 'color-theme-sanityinc-tomorrow)
  (load-theme 'sanityinc-tomorrow-night t t)
  (load-theme 'sanityinc-tomorrow-eighties t t)
  (enable-theme 'sanityinc-tomorrow-night)
  )

;; Gruvbox
(when (equal mech-theme 'box)
  (straight-use-package 'gruvbox-theme)
  (require 'gruvbox-theme)
  (load-theme 'gruvbox-dark-hard t t)
  (load-theme 'gruvbox-light-hard t t)
  (enable-theme 'gruvbox-dark-hard)
  )

;; Lambda
(when (equal mech-theme 'lambda)
  (straight-use-package '(lamda-themes :type git :host github :repo "lambda-emacs/lambda-themes"))
  (require 'lambda-themes)
  (setq
   lambda-themes-set-italic-comments t
   lambda-themes-set-italic-keywords t
   lambda-themes-set-variable-pitch t
   )
  (load-theme 'lambda-dark)
  )

;; Customization
;; (deftheme mechanicus "I love to sail forbidden seas.")

;; (custom-theme-set-variables
;;  'mechanicus
;;  '(header-color "#d8bfd8")
;;  '(header-height 1.2)
;;  )

(unless (fboundp 'color-darken-name)
    (autoload #'color-darken-name "color" nil nil))

(defun mech-darken-worker ()
  (let ((bg (face-attribute 'default :background)))
    (message (format "orig bg: %s\n" bg))
    (setq bg (color-darken-name bg 40))
    (message (format "new bg: %s\n" bg))
    (custom-set-faces
     `(default ((t (:background ,bg)))))))

(defun mech-darken (&optional frame)
  (if frame
      (with-selected-frame frame (mech-darken-worker))
    (mech-darken-worker))
  (remove-hook 'after-make-frame-functions #'mech-darken))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'mech-darken)
  (mech-darken))

(defun endswithn (n s)
  (booleanp (compare-strings n nil nil s -1 nil))
  )

(defun mech-theme ()
  (require 'color)
  ;; Note let* form
  ;; https://www.gnu.org/software/emacs/manual/html_node/eintr/fwd_002dpara-let.html
  (let* ((default-fg-color
	  (face-attribute 'default :foreground))
	 (default-bg-color
	  (face-attribute 'default :background))
	 ;; (header-color "#ffdead")
	 (header-color
	  (face-attribute 'org-level-1 :foreground))
	 (header-height-l1 1.1)
	 (header-height-l2 1.05)
	 (header-height-l3 1.05)
	 ;; (link-fg-color "#8751be")
	 (link-fg-color "#7851a9")
	 (inline-code-fg-color "#c2261f")
	 ;; gtk colors are less contrasting
	 (inline-code-bg-color
	  (cond
	   ((equal system-type 'windows-nt) (color-lighten-name default-bg-color 10))
	   ((equal system-type 'gnu/linux) (color-lighten-name default-bg-color 30))))
	 (code-block-bg-color
	  (cond
	   ((equal system-type 'windows-nt) (color-darken-name default-bg-color 10))
	   ((equal system-type 'gnu/linux) (color-darken-name default-bg-color 30))))
	 )
    (custom-set-faces
     `(org-level-1 ((t (:height ,header-height-l1))))
     `(org-level-2 ((t (:foreground ,header-color :height ,header-height-l2))))
     )
    ;; Note that latter custom-set-faces with unspecified attributes will overide the previous custom-set-faces
    (dolist (header-face org-level-faces)
      (unless (or (endswithn "1" (symbol-name header-face))
		  (endswithn "2" (symbol-name header-face)))
       (custom-set-faces
	`(,header-face ((t (:foreground ,header-color :height ,header-height-l3))))
       )))
    (custom-set-faces
     `(org-link ((t (:foreground ,link-fg-color :underline t))))
     `(org-code ((t (:foreground ,inline-code-fg-color :background ,inline-code-bg-color))))
     `(org-block-begin-line ((t (:background ,code-block-bg-color :extend t))))
     `(org-block-end-line ((t (:background ,code-block-bg-color :extend t))))
     `(org-block ((t (:background ,code-block-bg-color))))
     `(org-document-title ((t (:height 1.2 :weight bold :underline t))))
     )
    )
  )

(when (equal mech-theme 'box)
  (add-hook 'org-mode-hook #'mech-theme)
  )

;; Less clustered modeline
;; http://emacs-fu.blogspot.com/2010/05/cleaning-up-mode-line.html
(straight-use-package 'diminish)
(require 'diminish)
(with-eval-after-load 'eldoc
  (diminish 'eldoc-mode ""))
(with-eval-after-load 'yasnippet
  (diminish 'yas-minor-mode ""))
(with-eval-after-load 'company
  (diminish 'company-mode ""))
(with-eval-after-load 'projectile
  (diminish 'projectile-mode " 📁"))
(with-eval-after-load "hideshow"
  (diminish 'hs-minor-mode " 📜"))
(with-eval-after-load 'company-box
  (diminish 'company-box-mode " 🤖"))
(with-eval-after-load 'apheleia
  (diminish 'apheleia-mode " 🏺"))
(with-eval-after-load 'lsp
  (diminish 'lsp-mode " 𝞢"))
;; (with-eval-after-load 'flycheck
;;   (diminish 'flycheck-mode " Δ"))
(add-hook 'emacs-lisp-mode-hook
	  (lambda()
	    (setq mode-name "El")))
(add-hook 'python-mode-hook
	  (lambda()
	    (setq mode-name "Py")))
(add-hook 'pdf-view-mode-hook
	  (lambda()
	    (setq mode-name "PDF")))

