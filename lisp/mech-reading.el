;; PDF
(straight-use-package 'pdf-tools)
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
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete))

;; EPUB
(straight-use-package 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
