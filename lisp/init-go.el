(use-package go-mode
  :mode ("\\.go$" . go-mode)
  :init
  (progn
    (use-package auto-complete)
    (use-package go-eldoc)
    (use-package go-autocomplete)
    (use-package go-flymake)
    (use-package go-flycheck)
    (add-hook 'go-mode-hook
              (lambda ()
                (setq truncate-lines nil)
                (auto-complete-mode)
                (go-eldoc-setup)
                (add-hook 'before-save-hook 'gofmt-before-save)
                (setq tab-width 4)))))

(setq gofmt-command "goimports")

(provide 'init-go)
