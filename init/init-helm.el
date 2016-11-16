(use-package helm
  :ensure helm
  :commands (helm-get-sources helm-marked-candidates)
  :init
  (setq helm-autoresize-max-height 40)
  (setq helm-autoresize-min-height 20)
  
  :config
  (use-package helm-config)
  (helm-autoresize-mode 1)
  (helm-mode 1))

(provide 'init-helm)
