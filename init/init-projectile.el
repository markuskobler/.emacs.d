(use-package projectile
  :ensure
  :config
  (progn
    (use-package helm-projectile :ensure)
    (autoload
      'projectile-mode
      "projectile-mode"
      "Project Management"
      t)
    (helm-projectile-on)))

(provide 'init-projectile)
