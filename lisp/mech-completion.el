;; Configuration
(defvar completion-framework "company")
(when (equal system-type 'gnu/linux)
    ;; Toggle completion framework on linux
    (setq completion-framework "company"))

;; Orderless
(straight-use-package 'orderless)
(require 'orderless)
;; (setq-local completion-styles '(basic orderless))
(setq completion-styles '(basic orderless))

;; Vertico
(straight-use-package 'vertico)
(require 'vertico)
(vertico-mode)
(setq vertico-count 7)
(setq vertico-resize nil)

;; Company
(when (equal completion-framework "company")
  (straight-use-package 'company)
  (require 'company)
  ;; source: https://www.reddit.com/r/emacs/comments/m52nky/comment/gr2ap03/?utm_source=share&utm_medium=web2x&context=3
  (setq
   company-minimum-prefix-length 1
   company-idle-delay 0.1
   company-tooltip-limit 10
   company-tooltip-align-annotations t		;; Align annotations to the right
   company-require-match nil			;; Allow free typing
   company-dabbrev-ignore-case 'keep-prefix	;; Keep prefix at completion
   company-dabbrev-downcase nil			;; Don't automatically downcase completions
   )
  ;; Source: https://stackoverflow.com/a/11573802/17006775
  ;; (setq company-backends (remove 'company-clang company-backends))
  (setq company-dabbrev-other-buffers t)
  ;; Make dabbrev ignore pdf buffers
  (setq company-dabbrev-ignore-buffers ".*\\.pdf")
  (setq company-global-modes '(not shell-mode eshell-mode))
  (delete 'company-clang company-backends)
  (global-company-mode)

  ;; Company box
  (straight-use-package 'company-box)
  (setq company-box-doc-enable nil)
  (add-hook 'company-mode-hook #'company-box-mode)
  )

;; corfu
;; lsp with corfu is so slow that it's unusable
(when (eq completion-framework "corfu")
  (straight-use-package 'corfu)
  (global-corfu-mode)
  (setq
   corfu-auto t
   corfu-auto-delay 0.4 ;; Lower delay hangs while higher delay just won't complete anything
   corfu-quit-no-match 'separator
   ))

;; Completion in shell-mode is too slow
;; Update, configure wsl to not include host path.
(defun mech-completion-shell-hook ()
  (setq-local completion-styles '(basic))
  (setq-local corfu-auto nil)
  )
;; (add-hook 'shell-mode-hook #'mech-completion-shell-hook)
;; (add-hook 'eshell-mode-hook #'mech-completion-shell-hook)

(defun sh-completion-table-exclude-path (orig &rest args)
  (let ((exec-path exec-path))
    (dolist (path exec-path)
      (if (booleanp (compare-strings "/mnt/c" nil nil path nil 6))
          (setq exec-path (remove path exec-path))))
    ;; (dolist (path exec-path)
    ;;   (insert (concat path "\n")))
    (apply orig args)
    ))

(unless (fboundp 'sh--cmd-completion-table-gen)
  (autoload #'sh--cmd-completion-table-gen "sh-script" nil nil))

(advice-add 'sh--cmd-completion-table-gen :around #'sh-completion-table-exclude-path)

;;; mech-completion.el ends here
