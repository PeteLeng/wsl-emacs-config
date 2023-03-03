;; Navigation and editing
;; Set zap-up-to-char as default
(global-set-key (kbd "M-z") 'zap-up-to-char)
;; repeat the most recent command
(global-set-key (kbd "C-.") 'repeat)

;; Buffer Management
(straight-use-package 'ace-window)
;; (require 'ace-window)
(with-eval-after-load 'ace-window
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil)
  (setq aw-dispatch-always t)
  ;; Source: https://oremacs.com/2015/02/27/ace-window-leading-char/
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 1.1))))))
(unless (fboundp 'ace-window)
  (autoload #'ace-window "ace-window" nil t))
(global-set-key (kbd "M-o") 'ace-window)

;; Text Selection
(straight-use-package 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Template
;; source: https://github.com/joaotavora/yasnippet
(straight-use-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode)

;; Snippets
(straight-use-package 'yasnippet-snippets)

;; Spell Check
(add-hook 'text-mode-hook #'flyspell-mode)
(with-eval-after-load "flyspell"
  (global-set-key (kbd "M-/") 'ispell-word)
  ;; (define-key flyspell-mode-map (kbd "C-~") (keymap-lookup flyspell-mode-map "C-;"))
  ;; (define-key flyspell-mode-map (kbd "C-;") nil)
  )

;; Hide-show mode
;; Source: https://emacs.stackexchange.com/a/33686
(with-eval-after-load "hideshow"
  ;; replace @ with h
  (define-key hs-minor-mode-map (kbd "C-c h")
	      (lookup-key hs-minor-mode-map (kbd "C-c @")))
  ;; disable @
  (define-key hs-minor-mode-map (kbd "C-c @") nil)
  (define-key hs-minor-mode-map (kbd "C-c h h") 'hs-hide-block)
  (define-key hs-minor-mode-map (kbd "C-c h s") 'hs-show-block)
  (define-key hs-minor-mode-map (kbd "C-c h H") 'hs-hide-all)
  (define-key hs-minor-mode-map (kbd "C-c h S") 'hs-show-all))

;; God mode
(straight-use-package 'god-mode)
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)
(require 'god-mode)
(global-set-key (kbd "<escape>") #'god-mode-all)
(define-key god-local-mode-map (kbd ".") #'repeat)
;; god mode cursor
(defun god-mode-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'hollow 'hbar)))
(add-hook 'god-mode-enabled-hook #'god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook #'god-mode-update-cursor)

;; Holy motion
(unless (fboundp 'holymotion-make-motion)
  (autoload #'holymotion-make-motion "holymotion" nil t))
(straight-use-package
 '(holymotion :type git :host github :repo "Overdr0ne/holymotion" :branch "main"))
;; (require 'holymotion)
(holymotion-make-motion
 holymotion-forward-sexp #'forward-sexp)
(holymotion-make-motion
 holymotion-backward-sexp #'backward-sexp)

(with-eval-after-load 'holymotion
  ;; C-j originally bind to electric-newline-and-maybe-indent
  (define-key global-map (kbd "C-j") holymotion-map)
  (define-key holymotion-map (kbd "n") 'holymotion-next-line)
  (define-key holymotion-map (kbd "p") 'holymotion-previous-line)
  (define-key holymotion-map (kbd "e") 'holymotion-forward-to-word)
  (define-key holymotion-map (kbd "a") 'holymotion-backward-to-word)
  (define-key holymotion-map (kbd "f") 'holymotion-forward-sentence)
  (define-key holymotion-map (kbd "b") 'holymotion-backward-sentence)
  (define-key holymotion-map (kbd "F") 'holymotion-forward-sexp)
  (define-key holymotion-map (kbd "B") 'holymotion-backward-sexp)
  )
