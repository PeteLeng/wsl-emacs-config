;; Projectile
;; source: https://github.com/bbatsov/projectile
(straight-use-package 'projectile)
(unless (fboundp 'projectile-mode)
  (autoload #'projectile-mode "projectile" nil t))
;; "git ls-files" give warnings to ".gitignore" files that are symlinks
;; https://www.reddit.com/r/git/comments/nvglu2/new_git_version_doesnt_like_my_symbolic_link/
;; invoked by (projectile-files-via-ext-command)
;; fix by confining straight.el
(projectile-mode)
;; (setq projectile-indexing-method 'hybrid)
(setq projectile-project-search-path '(("~/prog" . 1) ("~/proj" . 1)))
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Version Control
(straight-use-package 'magit) ;; Magit is autoloaded at startup
