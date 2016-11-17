(use-package flycheck
  :init (progn
          (add-hook 'after-init-hook 'global-flycheck-mode))
  :config (progn
            (setq flycheck-display-errors-delay 0.3)
            (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc html-tidy))
            (set-face-foreground 'flycheck-error "red"))
  :bind (("C-c C-l" . flycheck-list-errors)))

(provide 'init-flycheck)
