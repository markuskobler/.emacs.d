(use-package yaml-mode
  :ensure
  :mode (("\\.yml\\'" . yaml-mode))
  :config
  (progn
    (use-package yaml-tomato :ensure)))

(provide 'init-yaml)
