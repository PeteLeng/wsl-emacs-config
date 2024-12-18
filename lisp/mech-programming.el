;; Configuration
(defvar lang-server-framework "lsp-mode")
(unless completion-framework
  (setq lang-server-framework "bridge"))

(add-hook 'prog-mode-hook #'hs-minor-mode)

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
(straight-use-package
 `(copilot :type git
	   :host github
	   :repo "zerolfx/copilot.el"
	   :files ,(append '("dist") straight-default-files-directive)))
;; (require 'copilot)
;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;; (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
;; (with-eval-after-load 'company
;;   (delq 'company-preview-if-just-one-frontend company-frontends))

;; Python
(straight-use-package 'pyvenv)
(add-hook 'python-mode-hook #'pyvenv-mode)

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
;; (add-hook 'python-mode-hook #'mech-python-mode-hook)

;; C/CPP
;; (with-eval-after-load "cc-mode"
;;   (c-set-offset 'case-label '+)
;;   (when (boundp 'c-mode-map)
;;     (define-key c-mode-map (kbd "M-n") #'forward-sentence)
;;     (define-key c-mode-map (kbd "M-p") #'backward-sentence)
;;     )
;;   (when (boundp 'c++-mode-map)
;;     (define-key c++-mode-map  (kbd "M-n") #'forward-sentence)
;;     (define-key c++-mode-map (kbd "M-p") #'backward-sentence)
;;     )
;;   )

;; CUDA
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

;; Rust
(straight-use-package 'rust-mode)
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; Racket
(straight-use-package 'racket-mode)

;; Ocaml
(straight-use-package 'tuareg)

;; Ada lovelace
;; cannot compile, frustrating
;; (straight-use-package 'ada-mode)

;; JS

;; Java


;; Lsp-Bridge (aims to be the fastest lsp client out there)
;; Dependencies
(straight-use-package 'posframe)
(straight-use-package '(markdown-mode :type git :host github :repo "jrblevin/markdown-mode"))

(when (equal lang-server-framework "bridge")
  (cond
   ((equal system-type 'gnu/linux)
    (straight-use-package
     `(lsp-bridge :type git
		  :host github
		  :repo "manateelazycat/lsp-bridge"
		  :files ,(append '("*.py" "acm" "core" "langserver" "multiserver" "resources") straight-default-files-directive))))
   
   ;; Cannot create symlink for .dz files on Windows
   ((equal system-type 'windows-nt)
    (straight-use-package
     `(lsp-bridge :type git
		  :host github
		  :repo "manateelazycat/lsp-bridge"
		  :files ,(append '("*.py" "acm" "core" "langserver" "multiserver" "resources") straight-default-files-directive)))))
  (require 'lsp-bridge)
  (global-lsp-bridge-mode)
  ;; (setq lsp-bridge-enable-log t)

  ;; lsp-bridge and pdf-view-mode somehow does not play nice together.
  ;; Turning on and off lsp-bridge mode munally in pdf buffer solves the problem.
  ;; Use hook to avoid turning on lsp-bridge mode in pdf-view-mode, did not work.

  ;; Update: the issue is not pdf-view-mode, it's pdf file.
  ;; Opening a pdf file while connecting to the pyright server has the same issue without turing on pdf-view-mode
  ;; See temporary fix: [[id:c5e31f43-4947-4866-8531-817637e1dff0][lsp-bridge pdf bug report]]
  )

;; LSP
(when (equal lang-server-framework "lsp-mode")
  (straight-use-package 'lsp-mode)
  (setq lsp-keymap-prefix "C-c l")
  (with-eval-after-load "lsp-mode"
    (setq
     ;; lsp-log-io t ;; For debugging only, big performance hit
     lsp-signature-render-documentation nil)
    
    ;; Remote client for Rust
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection "~/.local/bin/rust-analyzer")
                      :major-modes '(rust-mode)
                      :remote? t
                      :server-id 'rust-analyzer-remote)))
  
  ;; Python mode
  (straight-use-package 'lsp-pyright)
  ;; https://github.com/emacs-lsp/lsp-pyright/issues/6
  (setq lsp-pyright-multi-root nil)
  (add-hook 'python-mode-hook #'(lambda ()
				  (require 'lsp-pyright)
				  ))
  
  ;; Configure modes that automatically turn on lsp mode
  (unless (fboundp 'lsp-deferred)
    (autoload #'lsp-deferred "lsp-mode" nil nil))
  (add-hook 'c-mode-hook #'lsp-deferred)
  (add-hook 'python-mode-hook #'lsp-deferred)
  (add-hook 'rust-mode-hook #'lsp-deferred)
  (add-hook 'sh-mode-hook #'lsp-deferred)
  (add-hook 'c++-mode-hook #'lsp-deferred)
  (setq lsp-csharp-server-path "~/.local/omnisharp-roslyn/OmniSharp")
  (add-hook 'csharp-mode-hook #'lsp-deferred)
  (add-hook 'racket-mode-hook #'lsp-deferred)

  ;; Java
  (straight-use-package 'lsp-java)
  (setq lsp-java-server-install-dir "/home/pete/opt/java-lsp/")
  (setq lsp-java-format-settings-url "https://github.com/redhat-developer/vscode-java/wiki/Formatter-settings")
  (add-hook 'java-mode-hook #'(lambda ()
				(require 'lsp-java)
				(lsp-deferred)))

  ;; Ruby
  (add-hook 'ruby-mode-hook #'(lambda ()
				(lsp-deferred)
				;; (push 'rubocop-ls lsp-disabled-clients)
				))
  ;; (setq lsp-rubocop-use-bundler t)

  (defun mech-lsp-mode-hook ()
    ;; Configure company backends
    (when (equal completion-framework "company")
      (setq-local company-backends '(company-capf company-dabbrev company-dabbrev-code)))
    )
  ;; (add-hook 'lsp-mode-hook #'mech-lsp-mode-hook)
  )

;; Autoformatting
(straight-use-package 'apheleia)
(apheleia-global-mode +1)
(with-eval-after-load 'apheleia
  (setf (cdr (assoc 'google-java-format apheleia-formatters))
	'("java" "-jar" "/home/pete/opt/google-java-format-1.18.1-all-deps.jar" filepath)))

;; mech-programming.el ends here.
