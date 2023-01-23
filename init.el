(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(require 'mech-core)
(mech-core)

;; package management, straight
;; (require 'use-package-core) ;; https://github.com/radian-software/straight.el/issues/1035
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Buffer-selection, Expand-region, Template, Spell-check
(load "mech-editing")

;; Theme
;; lsp-bridge depends on correct theme colors
(load "mech-theme")

;; Vertico, Orderless, Company
(load "mech-completion")

;; Projectile, Magit
(load "mech-project")

;; PDF, EPUB
(load "mech-reading")

;; Programming
;; Flycheck, Tramp, LSP, Tree-sitter, Copilot,
;; C, Python, Rust,
;; Leetcode, Exercism,
(load "mech-programming")

;; Org, Org-roam
(load "mech-org")

;; Utilities
(load "mech-util")

;; init.el ends here.
 
