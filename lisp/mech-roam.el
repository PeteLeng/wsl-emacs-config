;; Note Management
(straight-use-package 'org-roam)
(defvar org-roam-directory nil)
(setq org-roam-directory (file-truename "~/org/vault/"))

;; Global Keybindings
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
  (org-roam-db-autosync-mode)
  
  ;; Roam Keybindings
  (define-key org-roam-mode-map (kbd "C-c n k") 'org-id-get-create)
  (define-key org-roam-mode-map (kbd "C-c n t") 'org-roam-tag-add)
  (define-key org-roam-mode-map (kbd "C-c n a") 'org-roam-alias-add)
  (define-key org-roam-mode-map (kbd "C-c n b") 'org-roam-buffer-toggle)

  (cl-defmethod org-roam-node-dir-name ((node org-roam-node))
    (let* ((dir
	    ;; returns the directory name that ends with "/"
	    (file-name-directory (org-roam-node-file node)))
	   (rel-dir
	    (file-relative-name dir org-roam-directory))
	   )
      (unless (string-equal "./" rel-dir)
	(concat "#/" rel-dir " "))))
    
  (setq org-roam-node-display-template
	(concat "${title:60} "
		(propertize "${dir-name:10}" 'face 'org-tag)
		(propertize "${tags:10}" 'face 'org-tag)))

  ;; Subdirectories
  (let ((templates
	 '(("a" "archives" plain nil
	    :target (file+head "archs/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}") :unnarrowed t)
	   ("l" "lectures" plain nil
	    :target (file+head "lects/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}") :unnarrowed t))
		   ))
    (dolist (tmpl templates)
      (push tmpl org-roam-capture-templates)))
  
  )

;;; mech-roam ends here.