(use-package puppet-mode
  :mode (("\\.pp$" . puppet-mode))
  :config
  (progn
    (add-hook 'puppet-mode
              (lambda ()
                (add-hook 'before-save-hook 'delete-trailing-whitespace)))))

(provide 'init-puppet)
