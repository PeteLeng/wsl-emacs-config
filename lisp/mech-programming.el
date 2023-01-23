;; Configuration
(defvar lang-server-framework "lsp-mode")

;; flycheck
(straight-use-package 'flycheck)
;; (add-hook 'prog-mode-hook #'flycheck-mode)
;; (with-eval-after-load "flycheck"
;;   ;; source: https://stackoverflow.com/a/32239523/17006775
;;   (define-key flycheck-mode-map (kbd "C-c ! !") 'org-time-stamp-inactive)
;;   )

;; tramp

;; gdb

;; tree-sitter

;; copilot

;; python
(straight-use-package 'pyvenv)

(defun mech-python-mode-hook ()
  ;; Rebind key
  (when (boundp 'python-mode-map)
    ;; (define-key map [remap backward-sentence] #'python-nav-backward-block) ...
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Remapping-Commands.html
    (define-key python-mode-map [remap backward-sentence] nil)
    (define-key python-mode-map [remap forward-sentence] nil)
    (define-key python-mode-map (kbd "C-<") #'python-nav-backward-block)
    (define-key python-mode-map (kbd "C->") #'python-nav-forward-block))
  )

(add-hook 'python-mode-hook #'mech-python-mode-hook)

;; C/CPP
(with-eval-after-load "cc-mode"
  (c-set-offset 'case-label '+)
  )

;; Rust
(straight-use-package 'rust-mode)
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; JS

;; LSP
(when (and (not (equal completion-framework "bridge"))
	   (equal lang-server-framework "lsp-mode"))
  (straight-use-package 'lsp-mode)
  (setq lsp-keymap-prefix "C-c l")
  (with-eval-after-load "lsp-mode"
    ;; (define-key lsp-mode-map (kbd "M-p") nil)
    ;; (define-key lsp-mode-map (kbd "M-n") nil)
    (setq
     ;; lsp-log-io t ;; For debugging only, big performance hit
     lsp-signature-render-documentation nil
     )
    )

  (defun mech-lsp-mode-hook ()
    ;; Configure company backends
    (when (equal completion-framework "company")
      (setq-local company-backends '(company-capf company-dabbrev company-dabbrev-code)))

    ;; Configure completion styles
    ;; In hopes of increasing completion speed, to no avail
    ;; (setq-local completion-styles '(basic partial-completion emacs22))

    )
  
  (add-hook 'lsp-mode-hook #'mech-lsp-mode-hook)
  
  ;; Configure modes that automatically turn on lsp mode
  (unless (fboundp 'lsp-deferred)
    (autoload #'lsp-deferred "lsp-mode" nil nil))
  (add-hook 'c-mode-hook #'lsp-deferred)

  (straight-use-package 'lsp-pyright)
  (add-hook 'python-mode-hook #'(lambda ()
				    (require 'lsp-pyright)
				    ))
  (add-hook 'python-mode-hook #'lsp-deferred)
  (add-hook 'rust-mode-hook #'lsp-deferred)
  )

;; mech-programming.el ends here.
