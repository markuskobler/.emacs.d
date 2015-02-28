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
    (setq-default sass-indent-level 2)
    (setq-default css-basic-offset 2)
    (setq-default css-indent-offset 2)
    (add-hook 'scss-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'delete-trailing-whitespace)))))

(provide 'init-css)
;;; init-css.el ends here
