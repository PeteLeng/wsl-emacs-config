;; Org
(straight-use-package 'org)

(setq
 org-hide-leading-stars t
 org-hide-emphasis-markers t
 org-pretty-entities t				;; replace \name with utf-8 characters
 org-cycle-include-plain-lists 'integrate	;; tab hides bullet points (plain lists)
 org-cycle-separator-lines -1			;; show empty lines when folding subtree
 org-use-speed-commands t
 org-image-actual-width nil			;; allows inline image resizing

 ;; Quote block bg eye candies
 ;; Source: https://emacs.stackexchange.com/a/41260
 org-fontify-quote-and-verse-blocks t
 )

(defun mech-org-prettify-symbols ()
  (setq prettify-symbols-unprettify-at-point 'right-edge)
  (setq org-ellipsis "⤵")
  (let ((symbols
	 '(("TODO" . "")
	   ("WAIT" . "")
   	   ("NOPE" . "")
	   ("DONE" . "")
	   ("[#A]" . "")
	   ("[#B]" . "")
 	   ("[#C]" . "")
	   ("[ ]" . "")
	   ("[X]" . "")
	   ("[-]" . "")
	   ("#+BEGIN_SRC" . "")
	   ("#+END_SRC" . "―")
	   ("#+begin_src" . "")
	   ("#+end_src" . "―")
	   (":PROPERTIES:" . "")
	   (":END:" . "―")
	   ("#+STARTUP:" . "")
	   ("#+startup:" . "")
	   ("#+TITLE: " . "")
	   ("#+RESULTS:" . "")
	   ("#+NAME:" . "")
	   ("#+ROAM_TAGS:" . "")
	   ("#+FILETAGS:" . "")
	   ("#+HTML_HEAD:" . "")
	   ("#+SUBTITLE:" . "")
	   ("#+AUTHOR:" . "")
	   (":Effort:" . "")
	   ("SCHEDULED:" . "")
	   ("DEADLINE:" . "")
	   ("lambda" . "λ")
	   ("|>" . "▷")
	   ("<|" . "◁")
	   ("->>" . "↠")
	   ("->" . "→")
	   ("<-" . "←")
	   ("=>" . "⇒")
	   ("<=" . "≤")
	   (">=" . "≥")
           ("!=" . "?"))
	 ))
    (dolist (symbol symbols)
      (push symbol prettify-symbols-alist)))
  (prettify-symbols-mode)
  )

(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-mode-hook #'mech-org-prettify-symbols) 

;; Show hidden markers when editing
(straight-use-package 'org-appear)
(with-eval-after-load 'org-appear
  (setq
   org-appear-autosubmarkers t
   org-appear-autoentities t
   org-appear-inside-latex t
   ))
(add-hook 'org-mode-hook #'org-appear-mode)

;; Eye Candies
(straight-use-package 'org-bullets)
(with-eval-after-load 'org-bullets
  (setq org-bullets-bullet-list '("◉" "•" "•" "•" "•" "•" "•" "•"))
  )
(add-hook 'org-mode-hook #'org-bullets-mode)

;; Org links for pdf
(straight-use-package 'org-pdftools)
(add-hook 'org-mode-hook #'org-pdftools-setup-link)

;; Org keybindings
(unless (fboundp 'org-store-link)
  (autoload #'org-store-link "org" nil t))
(unless (fboundp 'org-agenda)
  (autoload #'org-agenda "org" nil t))
(unless (fboundp 'org-capture)
  (autoload #'org-capture "org" nil t))
(unless (fboundp 'org-insert-todo-heading)
  (autoload #'org-insert-todo-heading "org" nil t))

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
;; (define-key global-map (kbd "") 'org-insert-todo-heading)

;; Note Management
(straight-use-package 'org-roam)
(defvar org-roam-directory nil)
(setq org-roam-directory (file-truename "~/org/vault"))
(with-eval-after-load 'org-roam
  ;; (setq org-roam-node-display-template
  ;; 	(concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  )

(unless (fboundp 'org-roam-node-find)
  (autoload #'org-roam-node-find "org-roam" nil t))
(unless (fboundp 'org-roam-node-insert)
  (autoload #'org-roam-node-insert "org-roam" nil t))
(unless (fboundp 'org-roam-node-random)
  (autoload #'org-roam-node-random "org-roam" nil t))

(define-key global-map (kbd "C-c n f") 'org-roam-node-find)
(define-key global-map (kbd "C-c n i") 'org-roam-node-insert)
(define-key global-map (kbd "C-c n r") 'org-roam-node-random)

(with-eval-after-load 'org-roam
  (define-key org-roam-mode-map (kbd "C-c n k") 'org-id-get-create)
  (define-key org-roam-mode-map (kbd "C-c n t") 'org-roam-tag-add)
  (define-key org-roam-mode-map (kbd "C-c n a") 'org-roam-alias-add)
  (define-key org-roam-mode-map (kbd "C-c n b") 'org-roam-buffer-toggle)
  )

