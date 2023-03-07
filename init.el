(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(require 'mech-core)
(mech-core)

;; package management, straight
;; (require 'use-package-core) ;; https://github.com/radian-software/straight.el/issues/1035
(defvar native-comp-deferred-compilation-deny-list nil) ;; https://www.reddit.com/r/emacs/comments/nyis3p/problem_running_native_comp/

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
(load "mech-roam")

;; Utilities
(load "mech-util")

;; init.el ends here.
 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
