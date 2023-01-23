;; Projectile
;; source: https://github.com/bbatsov/projectile
(straight-use-package 'projectile)
(unless (fboundp 'projectile-mode)
  (autoload #'projectile-mode "projectile" nil t))
(global-set-key (kbd "C-c p") #'projectile-mode)
(with-eval-after-load "projectile"
  (setq projectile-project-search-path '(("~/prog" . 1)
					 ("~/proj" . 1)))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

;; Version Control
(straight-use-package 'magit) ;; Magit is autoloaded at startup
