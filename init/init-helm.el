(use-package helm
  :ensure helm
  :commands (helm-get-sources helm-marked-candidates)
  :init
  (setq helm-autoresize-max-height 40
        helm-autoresize-min-height 20
        projectile-completion-system 'helm)

  :config
  (use-package helm-config)
  (helm-autoresize-mode 1)
  (helm-mode 1))

(use-package helm-projectile
  :ensure t
  :config
  (projectile-global-mode)
  (helm-projectile-on))

(provide 'init-helm)
