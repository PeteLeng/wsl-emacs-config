;; PDF
(defun straight-files-exl-gignore ()
  (let ((files '()))
    (dolist (p straight-default-files-directive)
      (if (stringp p)
	  (add-to-list 'files p t)
	(let ((exl p))
	  (setq exl (add-to-list 'p ".gitignore" t))
	  (add-to-list 'files exl t))))
    files))

;; (straight-use-package `(pdf-tools :type git :host github :repo "vedang/pdf-tools"))
(straight-use-package 'pdf-tools)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(unless (fboundp 'pdf-view-mode)
  (autoload #'pdf-view-mode "pdf-view" nil t))
(with-eval-after-load "pdf-tools"
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  ;; (setq pdf-annot-activate-created-annotations t)

  ;; Source: https://pragmaticemacs.wordpress.com/2017/11/08/more-pdf-tools-tweaks/
  ;; Fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  ;; Shorter keybindings
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "u") 'pdf-annot-add-underline-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
  ;; Integration with God mode, which adds Ctrl modifier by default.
  ;; (define-key pdf-view-mode-map (kbd "C-SPC") 'pdf-view-scroll-up-or-next-page)
  ;; (define-key pdf-view-mode-map (kbd "C-j") 'pdf-view-next-page-command)
  ;; (define-key pdf-view-mode-map (kbd "C-k") 'pdf-view-previous-page-command)
  )

(add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes)
(defun mech-pdfview-mode-hook ()
  (display-line-numbers-mode -1)
  ;; (yas-minor-mode -1)
  )
(add-hook 'pdf-view-mode-hook #'mech-pdfview-mode-hook)

;; EPUB
(straight-use-package 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
