(use-package nix-mode
  :mode (("\\.nix\\'" . nix-mode))
  :config
  (progn
    (add-hook 'nix-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'delete-trailing-whitespace)))))

(provide 'init-nix)
