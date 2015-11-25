(use-package css-mode
  :mode (("\\.css\\'" . css-mode))
  :config
  (progn
    (setq-default css-basic-offset 2)
    (setq-default css-indent-offset 2)
    (add-hook 'css-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'delete-trailing-whitespace)))))

(use-package scss-mode
  :mode (("\\.scss\\'" . scss-mode))
  :config
  (progn
    (setq-default css-basic-offset 2)
    (setq-default css-indent-offset 2)
    (setq-default scss-compile-at-save nil)
    (add-hook 'scss-mode-hook
              (lambda ()
                (rainbow-mode t)
                (flycheck-mode t)
                (add-hook 'before-save-hook 'delete-trailing-whitespace)))))

(provide 'init-css)
