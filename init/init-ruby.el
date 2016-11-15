(use-package ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :init
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(provide 'init-ruby)
