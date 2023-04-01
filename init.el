(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(require 'mech-core)
(mech-core)

;; package management, straight
;; (require 'use-package-core) ;; https://github.com/radian-software/straight.el/issues/1035
(setq native-comp-deferred-compilation-deny-list nil) ;; https://www.reddit.com/r/emacs/comments/nyis3p/problem_running_native_comp/

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

;; fix unwanted warning in Projectile
(defun straight-files-exl-gignore ()
  (let ((files '()))
    (dolist (p straight-default-files-directive)
      (if (stringp p)
	  (add-to-list 'files p t)
	(let ((exl p))
	  (setq exl (add-to-list 'p ".gitignore" t))
	  (add-to-list 'files exl t))))
    files))

(defun mech-expand-files-log (fapp &rest args)
  (let* ((res (apply fapp args)))
    (message "Sym links:")
    ;; (message "%s" res)
    (dolist (edge res)
      (let ((src (car edge))
	    (dst (cdr edge)))
	(message "%s -> %s" src dst)))
    res))
(advice-add 'straight-expand-files-directive :around #'mech-expand-files-log)
(advice-remove 'straight-expand-files-directive #'mech-expand-files-log)

(defun mech-symlink-advice (fapp &rest args)
  (let ((src (nth 0 args))
	(dst (nth 1 args)))
    (if (string-equal (file-name-base src) ".gitignore")
	(message "ignore .gitignore as symlink")
      (apply fapp args))))
(advice-add 'straight--symlink-recursively :around #'mech-symlink-advice)

;; Theme
;; lsp-bridge depends on correct theme colors
(load "mech-theme")

;; Buffer-selection, Expand-region, Template, Spell-check
(load "mech-editing")

;; Vertico, Orderless, Company
(load "mech-completion")

;; Projectile, Magit
(load "mech-project")

;; PDF, EPUB
(load "mech-reading")

;; Programming
;; Flycheck, Tramp, LSP, Tree-sitter, Copilot,
;; C, Python, Rust,
(load "mech-programming")

;; Org, Org-roam
(load "mech-org")
(load "mech-roam")

;; Utilities
(load "mech-util")

;; init.el ends here.

