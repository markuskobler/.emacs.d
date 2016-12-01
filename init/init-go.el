(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :init
  (progn
    (use-package auto-complete   :ensure t)
    (use-package flycheck        :ensure t)
    (use-package go-autocomplete :ensure t)
    (use-package go-eldoc        :ensure t)
    (use-package go-errcheck     :ensure t)
    (use-package gotest          :ensure t))

  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq gofmt-command "goimports")

              (go-set-project)

              (flycheck-mode)
              (auto-complete-mode)
              (go-eldoc-setup)
              (add-hook 'before-save-hook 'gofmt-before-save)
              (setq tab-width 4))))

(provide 'init-go)
