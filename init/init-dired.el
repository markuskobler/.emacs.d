(use-package dired-details
  :ensure t
  :config
  (setq-default dired-details-hidden-string "-- ")
  
  :init
  (require 'dired-details)
  
  (dired-details-install))

(provide 'init-dired)
