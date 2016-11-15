(use-package helm-config
  :ensure helm
  :commands
  (helm-get-sources helm-marked-candidates)
  :config
  (progn
    (helm-mode 1)))

(provide 'init-helm)
