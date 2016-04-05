(use-package ruby-mode
  :mode (("\\.rb$" . ruby-mode))
  :config
  (progn
    (add-hook 'ruby-mode
              (lambda ()
                (add-hook 'before-save-hook 'delete-trailing-whitespace)))))

(provide 'init-ruby)
