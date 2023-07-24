;; Navigation and editing

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
;; (require 'yasnippet-snippets)

;; Spell Check
(add-hook 'text-mode-hook #'flyspell-mode)
(unless (fboundp 'ispell-word)
  (autoload #'ispell-word "ispell" nil t))
(global-set-key (kbd "M-/") 'ispell-word)
;; (with-eval-after-load "flyspell"
;;   (define-key flyspell-mode-map (kbd "C-~") (keymap-lookup flyspell-mode-map "C-;"))
;;   (define-key flyspell-mode-map (kbd "C-;") nil)
;;   )

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
;; (straight-use-package 'god-mode)
;; (setq god-exempt-major-modes nil)
;; (setq god-exempt-predicates nil)
;; (require 'god-mode)
;; (global-set-key (kbd "M-<f1>") #'god-mode-all)
;; (define-key god-local-mode-map (kbd ".") #'repeat)
;; ;; god mode cursor
;; (defun god-mode-update-cursor ()
;;   (setq cursor-type (cond (god-local-mode 'hollow)
;; 			  (buffer-read-only 'box)
;; 			  (t 'hbar))))
;; (add-hook 'god-mode-enabled-hook #'god-mode-update-cursor)
;; (add-hook 'god-mode-disabled-hook #'god-mode-update-cursor)

;; Holy motion
(unless (fboundp 'holymotion-make-motion)
  (autoload #'holymotion-make-motion "holymotion" nil t))
(straight-use-package
 '(holymotion :type git :host github :repo "Overdr0ne/holymotion" :branch "main"))
;; (holymotion-make-motion
;;  holymotion-forward-sexp #'forward-sexp)
;; (holymotion-make-motion
;;  holymotion-backward-sexp #'backward-sexp)

;; (with-eval-after-load 'holymotion
;;   ;; C-j originally bind to electric-newline-and-maybe-indent
;;   (define-key global-map (kbd "M-m") holymotion-map)
;;   (define-key holymotion-map (kbd ".") #'back-to-indentation)
;;   (define-key holymotion-map (kbd "n") 'holymotion-next-line)
;;   (define-key holymotion-map (kbd "p") 'holymotion-previous-line)
;;   (define-key holymotion-map (kbd "e") 'holymotion-forward-to-word)
;;   (define-key holymotion-map (kbd "a") 'holymotion-backward-to-word)
;;   (define-key holymotion-map (kbd "f") 'holymotion-forward-sentence)
;;   (define-key holymotion-map (kbd "b") 'holymotion-backward-sentence)
;;   (define-key holymotion-map (kbd "F") 'holymotion-forward-sexp)
;;   (define-key holymotion-map (kbd "B") 'holymotion-backward-sexp)
;;   )

;; Avy motions
;; https://github.com/abo-abo/avy
(straight-use-package '(avy :type git :host github :repo "abo-abo/avy"))
(defvar avy-map (make-sparse-keymap) "Avy motion")
;; (global-set-key (kbd "C-j") avy-map)
;; (keymap-set avy-map "j" #'avy-goto-word-1)
;; (keymap-set avy-map "k" #'avy-goto-char)
;; (keymap-set avy-map "l" #'avy-goto-line)

;; Unbind C-m from RET
(defun mech-avy (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (progn
      (define-key input-decode-map [?\C-m] [C-m]) 
      (global-set-key (kbd "<C-m>") avy-map))
    (message "new frame %S" (selected-frame))))
;; (if (daemonp) (add-hook 'after-make-frame-functions #'mech-avy) (mech-avy))

;; Navigator minor mode
;; Combine movements in avy and other modes
(defvar navigator-mode-map (make-sparse-keymap) "Navis Nobilite")
(keymap-set navigator-mode-map "C-j j" #'avy-goto-word-1)
(keymap-set navigator-mode-map "C-j k" #'avy-goto-char)
(keymap-set navigator-mode-map "C-j l" #'avy-goto-line)
;; (keymap-set navigator-mode-map "C-j ." #'org-noter-sync-current-page-or-chapter)
;; (keymap-set navigator-mode-map "C-j n" #'org-noter-sync-next-page-or-chapter)
;; (keymap-set navigator-mode-map "C-j p" #'org-noter-sync-prev-page-or-chapter)
(define-minor-mode navigator-mode "Navis Nobilite" :global t :keymap navigator-mode-map)
(navigator-mode 1)

