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
     ((t (:inherit ace-jump-face-foreground :height 1.0))))))
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
  (global-set-key (kbd "M-\\") 'ispell-word)
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

