(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode))
  :config
  (use-package yaml-tomato))

(provide 'init-yaml)
