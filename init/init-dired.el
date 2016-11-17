(use-package dired-details
  :ensure t
  :init
  (setq-default dired-details-hidden-string "-- ")

  (let ((gls "/usr/local/bin/gls"))
    (when (file-exists-p gls)
            (setq insert-directory-program gls)))

  :config
  (require 'dired-details)

  (dired-details-install))

(provide 'init-dired)
