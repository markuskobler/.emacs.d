(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :init
  (progn
    (setq racer-rust-src-path "~/code/vendor/rust/src/")
    (setq company-tooltip-align-annotations t)

    (use-package racer)
    (use-package flycheck)
    (use-package flycheck-rust)

    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

    (add-hook 'rust-mode-hook
              (lambda ()

                (flycheck-mode t)

                (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
                (local-set-key (kbd "M-.") #'racer-find-definition)

                (add-hook 'before-save-hook 'delete-trailing-whitespace)
                (setq rust-ident-offset 4)
                (setq tab-width 4)))))

(provide 'init-rust)
