(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :init
  (progn
    (use-package flycheck)
    (use-package flycheck-rust)
    (add-hook 'rust-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'delete-trailing-whitespace)
                (flycheck-rust-setup)
                (setq rust-ident-offset 4)
                (setq tab-width 4)))))

(provide 'init-rust)
