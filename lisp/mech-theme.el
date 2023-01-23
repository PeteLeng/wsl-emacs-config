
;; VS code theme
;; (straight-use-package 'vscode-dark-plus-theme)
;; (require 'vscode-dark-plus-theme)
;; (load-theme 'vscode-dark-plus t)

;; Gruvbox
(straight-use-package 'gruvbox-theme)
(require 'gruvbox-theme)
(load-theme 'gruvbox-dark-hard t t)
(load-theme 'gruvbox-light-hard t t)
(enable-theme 'gruvbox-dark-hard)

;; Customization
;; (deftheme mechanicus "I love to sail forbidden seas.")

;; (custom-theme-set-variables
;;  'mechanicus
;;  '(header-color "#d8bfd8")
;;  '(header-height 1.2)
;;  )

;; (custom-set-faces
;;    '(default ((t (:foreground "#e1d7bd")))))

(defun endswith1 (s)
  (booleanp (compare-strings "1" nil nil s -1 nil))
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
	  (face-attribute 'org-level-2 :foreground))
	 (header-height 1.1)
	 (link-fg-color "#8751be")
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
     `(org-level-1 ((t :height ,header-height))))
    (dolist (header-face org-level-faces)
      (unless (endswith1 (symbol-name header-face))
       (custom-set-faces
       `(,header-face ((t (:foreground ,header-color))))
       )))
    (custom-set-faces
     `(org-link ((t (:foreground ,link-fg-color :underline t))))
     `(org-code ((t (:foreground ,inline-code-fg-color :background ,inline-code-bg-color))))
     `(org-block-begin-line ((t (:background ,code-block-bg-color :extend t))))
     `(org-block-end-line ((t (:background ,code-block-bg-color :extend t))))
     `(org-block ((t (:background ,code-block-bg-color))))
     )
    )
  )

(add-hook 'org-mode-hook #'mech-theme)

