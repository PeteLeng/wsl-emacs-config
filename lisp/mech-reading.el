;; Dictionaries
(setq display-buffer-alist
      '(("^\\*Dictionary\\*"
	 display-buffer-in-side-window
	 (side . right)
	 (window-width . 60)
	 )))

(keymap-global-set "C-c d" 'dictionary-lookup-definition)

(defun mech-dict ()
  ;; (toggle-truncate-lines 1)
  )

(defun trace-dict-store-pos (orig &rest args)
  (message (format "%s" (list (point) (window-start) (frame-selected-window))))
  (apply orig args))

(defun dict-restore-buffer ()
  (select-window dictionary-selected-window)
  ;; (goto-char (car dictionary-positions))
  )

(with-eval-after-load 'dictionary
  (setq dictionary-use-single-buffer t)
  (add-hook 'dictionary-mode-hook 'mech-dict)
  ;; (advice-add 'dictionary-store-positions :around #'trace-dict-store-pos)
  (advice-add 'dictionary-lookup-definition :after #'dict-restore-buffer)
  )


;; PDF
(straight-use-package 'pdf-tools)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(require 'pdf-tools)

;; (unless (fboundp 'pdf-view-mode)
;;   ;; (autoload #'pdf-view-mode "pdf-view" nil t)
;;   ;; loads "pdf-view.el", however requires variables
;;   ;; from the not yet loaded "pdf-tools.el"
;;   (require 'pdf-tools)
;;   )

;; incompatible with pdf-view-mode
(defun mech-pdfview-mode-hook ()
  (message "---\nrun mech pdf hook\n---")
  (display-line-numbers-mode -1)
  )

(with-eval-after-load "pdf-tools"
  ;; (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  ;; (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  ;; (setq pdf-annot-activate-created-annotations t)

  ;; Source: https://pragmaticemacs.wordpress.com/2017/11/08/more-pdf-tools-tweaks/
  ;; Fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  ;; Faster keys
  ;; More on official doc: https://www.gnu.org/software/emacs/manual/html_node/elisp/Edebug.html
  ;; (define-key pdf-view-mode-map (kbd "m") 'pdf-annot-add-markup-annotation)
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "u") 'pdf-annot-add-underline-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "L") 'pdf-annot-list-annotations)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
  ;; Integration with God mode, which adds Ctrl modifier by default.
  ;; (define-key pdf-view-mode-map (kbd "C-SPC") 'pdf-view-scroll-up-or-next-page)
  ;; (define-key pdf-view-mode-map (kbd "C-j") 'pdf-view-next-page-command)
  ;; (define-key pdf-view-mode-map (kbd "C-k") 'pdf-view-previous-page-command)

  (add-hook 'pdf-view-mode-hook #'mech-pdfview-mode-hook)
  (add-hook 'pdf-view-mode-hook #'pdf-tools-enable-minor-modes)
  )


;; EPUB
(straight-use-package 'nov)
;; (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; (load-file "~/proj/epub/exml-query.el")
;; (load-file "~/proj/epub/epub.el")
(add-to-list 'load-path "~/proj/epub/")
(autoload #'epub-mode "epub" nil t)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . epub-mode))

(defun mech-epub ()
  (toggle-truncate-lines 1)
  (face-spec-set 'shr-text '((t . (:family "Iosevka Nerd Font")))))

(add-hook 'epub-mode-hook 'mech-epub)

;; Web feed
(straight-use-package 'elfeed)
(setq elfeed-feeds
      '("http://nullprogram.com/feed/"
	"https://project-mage.org/rss.xml"
	"https://karthinks.com/index.xml"
	"https://blog.trailofbits.com/feed/"
	"https://andreyor.st/feed.xml"))
(keymap-global-set "C-c w" 'elfeed)
