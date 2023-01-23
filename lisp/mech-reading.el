;; PDF
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
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
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
