(use-package go-mode
  :mode ("\\.go$" . go-mode)
  :config
  (progn
    (use-package go-flymake)))

(provide 'init-go)
