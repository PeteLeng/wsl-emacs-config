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
  
  ;; Roam Keybindings, bind to org mode instead of org-roam-mode
  (define-key org-mode-map (kbd "C-c n k") 'org-id-get-create)
  (define-key org-mode-map (kbd "C-c n t") 'org-roam-tag-add)
  (define-key org-mode-map (kbd "C-c n a") 'org-roam-alias-add)
  (define-key org-mode-map (kbd "C-c n b") 'org-roam-buffer-toggle)

  (cl-defmethod org-roam-node-dir-name ((node org-roam-node))
    (let* ((dir
	    ;; returns the directory name that ends with "/"
	    (file-name-directory (org-roam-node-file node)))
	   (relative-dir
	    (file-relative-name dir org-roam-directory))
	   )
      (unless (string-equal "./" relative-dir)
	(concat "- " relative-dir " "))))
    
  (setq org-roam-node-display-template
	(concat "${title:60} "
		(propertize "${dir-name:20}" 'face 'org-tag)
		(propertize "${tags:10}" 'face 'org-tag)))

  ;; Integration with org mode
  (let ((proj-dirs
	 (file-expand-wildcards (file-name-concat org-roam-directory "/projs/*.org") t)))
    (setq org-refile-targets (list (cons proj-dirs (cons :level 1))))
    )

  ;; capture templates with targets in different directories
  (defun get-blog-tmpl ()
    (let ((f (expand-file-name "../tmpls/blog.org" org-roam-directory)))
      (with-temp-buffer
	(insert-file-contents f)
	(buffer-string))))

  (defun get-latex-tmpl ()
    (let ((f (expand-file-name "../tmpls/latex.org" org-roam-directory)))
      (with-temp-buffer
	(insert-file-contents f)
	(buffer-string))))
  
  (defvar roam-templates
    '(("d" "default" plain "%?"
       :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
       :unnarrowed t)
      ("a" "archives" plain
       nil
       :target (file+head "archs/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
       :unnarrowed t)
      ("p" "projects" plain
       nil
       :target (file+head "projs/%<%Y%m%d%H%M%S>-${slug}.org"
			  ":PROPERTIES:\n:DATE_CREATED: %U\n:END:\n#+title: ${title}")
       :unnarrowed t)
      ("b" "blogs" plain (function get-blog-tmpl)
       :target (file+head "blogs/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}"))
      ("l" "latex" plain (function get-latex-tmpl)
       :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}"))
      
      ;; ("c" "Courses")
      ;; ("cl" "lectures" plain
      ;;  nil
      ;;  :target (file+head "courses/lects/%<%Y%m%d%H%M%S>-${slug}.org"
      ;; 			  ":PROPERTIES:\n:DATA_CREATED: %U\n:END:\n#+title: ${title}\n") 
      ;;  :unnarrowed t)
      
      ))
  
  ;; (dolist (tmpl templates)
  ;;   (push tmpl org-roam-capture-templates))
  ;; (setq org-roam-capture-templates (nconc org-roam-capture-templates roam-templates))
  (setq org-roam-capture-templates roam-templates)
  )

;;; mech-roam ends here.
