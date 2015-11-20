(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :init
  (progn
    (use-package racer)
    (use-package flycheck)
    (use-package flycheck-rust)

    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

    (add-hook 'racer-mode-hook
              (lambda ()
                (eldoc-mode t)
                (company-mode t)))

    (add-hook 'rust-mode-hook
              (lambda ()
                (setq racer-rust-src-path "~/code/vendor/rust/src/")
                (setq company-tooltip-align-annotations t)
                (setq company-minimum-prefix-length 1)
                (setq company-idle-delay 0.2)

                (racer-mode t)
                (flycheck-mode t)
                (flycheck-disable-checker 'rust)
;;                (flycheck-list-errors)

                (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
                (local-set-key (kbd "M-.") #'racer-find-definition)

                (add-hook 'before-save-hook 'delete-trailing-whitespace)
                (setq rust-ident-offset 4)
                (setq tab-width 4)))))

(provide 'init-rust)
