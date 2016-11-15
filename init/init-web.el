(use-package web-mode
  :ensure t
  :mode "\\.html\\'"
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (add-hook 'web-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'delete-trailing-whitespace)))))

(provide 'init-web)
