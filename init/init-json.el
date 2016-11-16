(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("\\.jshintrc\\'" . json-mode)
         ("\\.eslintrc\\'" . json-mode)
         ("\\.jscsrc\\'" . json-mode))
  :init
  (setq show-trailing-whitespace t)
  (setq js-indent-level 2)
  :config
  (add-hook 'json-mode-hook
            (progn
              (flycheck-mode t)))

  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(provide 'init-json)
