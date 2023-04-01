;; Asynchronous IO library
(straight-use-package 'aio)

;; Graphql
(straight-use-package 'graphql-mode)

;; List library
(straight-use-package 's)

;; String library
(straight-use-package 'dash)

;; Leetcode
;; (straight-use-package 'leetcode)
;; Not working and havn't got time to dig into it.

;; Playground
(defun mech-capture ()
  (interactive)
  (let* ((title
	  (read-string "Title:"))
	 (target
	  (completing-read "Target" (list "a" "b" "c"))))
    (message "%s" title)
    (message "%s" target)
    )
  )

(defun org-ast (fpath)
  (let ((tree
	 (with-temp-buffer
	   (insert-file-contents fpath)
	   (org-mode)
	   (org-element-parse-buffer))))
    tree))

(defun org-headline-path (tree)
  (org-element-map tree 'headline
    #'(lambda (hl)
	(let ((path ())
	      (hl hl))
	  (while (not (equal (org-element-property :raw-value hl) nil))
	    (push (org-element-property :raw-value hl) path)
	    (setq hl (org-element-property :parent hl)))
	  (string-join path "> "))))
  )

(defun org-title-name (tree)
  (org-element-map tree 'keyword
    #'(lambda (kw)
	(if (equal (org-element-property :key kw) "TITLE")
	    (org-element-property :value kw)))
    nil t)) ;; Stop at first match

(defun roam-proj-files (proj-dir)
  (file-expand-wildcards (file-truename (file-name-concat proj-dir "*.org")))
  )

(defun roam-proj-headlines (proj-dir)
  (let* ((files
	  (roam-proj-files proj-dir))
	 (headlines ()))
    (dolist (file files)
      (let* ((tree
	      (org-ast file))
	     (title
	      (org-title-name tree))
	     (hls
	      (org-headline-path tree)))
	;; (setq headlines (append headlines hls))
	(dolist (hl hls)
	  (push (concat title "> " hl) headlines))))
    headlines))

(defun mech-get-fonts (regex)
  (dolist (f (font-family-list))
    (when (string-match regex f)
      (insert (format "%s\n" f)))))
