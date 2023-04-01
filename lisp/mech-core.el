;; The core Lisp file contains the settings for a minimum functional Emacs.
;; It shall be loaded first in the init file before any packages.
;; The list of configurations includes:
;; - keybindings
;; - UI
;; - fonts
;; - encoding
;; - editing
;; - performance
;;   - gc

(defvar debug nil)

(defun mech-core-keybindings ()
  ;; windows-key to super-key on Windows
  (if (string-equal system-type "windows-nt")
      ;; or (equal system-type 'windows-nt)
      (progn
	(setq w32-lwindow-modifier 'super)
	(w32-register-hot-key [s-])))

  ;; Navigation
  (global-set-key (kbd "M-p") 'backward-sentence)
  (global-set-key (kbd "M-n") 'forward-sentence)
  ;; (global-set-key (kbd "M-a") 'backward-sexp)
  ;; (global-set-key (kbd "M-e") 'forward-sexp)
  (global-set-key (kbd "C-<") 'backward-paragraph)
  (global-set-key (kbd "C->") 'forward-paragraph)
  (if (equal system-type 'windows-nt)
      (progn
	(global-set-key (kbd "s-p") 'previous-buffer)
	(global-set-key (kbd "s-n") 'next-buffer)))

  ;; Editing
  (global-set-key (kbd "C-;") 'comment-line)	;; Comment
  (global-set-key (kbd "C-z") 'undo)		;; Undo
  (global-set-key (kbd "M-#") 'mark-sexp)	;; Mark symbolic expression

  ;; Isearch, see more using "M-s C-h"

  ;; Imenu
  (global-set-key (kbd "C-c m") 'imenu)

  ;; Ibuffer
  (global-set-key (kbd "C-x C-b") 'ibuffer)

  ;; Emacs PGTK build bug, missing C-g keystrokes
  ;; Possible related to gnome bug: https://www.reddit.com/r/emacs/comments/bnly4o/unable_to_exit_minibuffer_with_just_cg/
  ;; (global-set-key (kbd "C-g") 'abort-recursive-edit)

  ;; Visible mode, show marker texts when org-hide-emphasis-markers is t
  ;; (global-set-key (kbd "C-c v") 'visible-mode)
  )

(defun mech-core-ui ()
  (setq inhibit-startup-message t)
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (set-scroll-bar-mode nil)
  (setq-default cursor-type 'hbar)
  )

(defun mech-core-dired ()
  (setq dired-create-destination-dirs t))

(defun mech-core ()
  ;; Keybindings
  (mech-core-keybindings)
  
  ;; UI
  (mech-core-ui)

  ;; Dired
  (mech-core-dired)

  ;; Encoding
  ;; source: https://www.masteringemacs.org/article/working-coding-systems-unicode-emacs
  (set-default-coding-systems 'utf-8)

  ;; Fontset
  ;; Replace default font with Symbola for symbol chars in default fontset
  ;; (list-character-sets 0) for the list of charsets
  ;; (setq use-default-font-for-symbols nil)
  (set-fontset-font
   t
   'symbol
   (cond
    ((equal system-type 'windows-nt)
     (cond ((member "MesloLGM Nerd Font Mono" (font-family-list)) "MesloLGM Nerd Font Mono")))
    ((equal system-type 'gnu/linux)
     (cond ((member "Symbola" (font-family-list)) "Symbola")
	   ((member "Symbols Nerd Font Mono" (font-family-list)) "Symbols Nerd Font Mono"))))
   )
    
  ;; Font
  (add-to-list 'default-frame-alist '(alpha-background . 90))
  (cond
   ((string-equal (getenv "MON") "on")
    (message "Monitor on")
    (add-to-list 'default-frame-alist '(font . "FantasqueSansMono Nerd Font-13")))
   (t
    (message "Monitor off")
    (add-to-list 'default-frame-alist '(font . "FantasqueSansMono Nerd Font Mono-18"))
    ))

  ;; Editing
  (electric-pair-mode)
  ;; (setq split-height-threshold nil)
  (global-display-line-numbers-mode)
  
  ;; Completion
  (savehist-mode)
  (setq read-file-name-completion-ignore-case t
	read-buffer-completion-ignore-case t
	completion-ignore-case t)
  (setq tab-always-indent 'complete)

  ;; Minibuffer
  ;; (setq minibuffer-prompt-properties
  ;;       '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t)

  ;; Performance
  (setq gc-cons-threshold (* 1024 1024 2))
  (setq read-process-output-max (* 1024 1024))
  
  ;; Use windows browser in wsl2 for default sound support
  ;; adapted from: https://hungyi.net/posts/browse-emacs-urls-wsl/
  (if (and (equal system-type 'gnu/linux)
	   (string-match "microsoft" (shell-command-to-string "uname -a")))
      (setq
       browse-url-generic-program "/mnt/c/Windows/System32/cmd.exe"
       browse-url-generic-args '("/c" "start")
       browse-url-browser-function #'browse-url-generic
       ))

  ;; Other
  (setq use-package-expand-minimally t)
  (setq debug-on-error debug) ;; set to t for debugging
  ;; (setq visible-bell t)
  (setq ring-bell-function 'ignore)
  )

(provide 'mech-core)
