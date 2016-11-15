(require 'dired)
(require 'dired-details)
(setq-default dired-details-hidden-string "-- ")
(dired-details-install)

(defadvice dired-do-rename
    (after revert-buffer-after-rename activate)
  (revert-buffer))

(defadvice dired-create-directory
    (after revert-buffer-after-create activate)
  (revert-buffer))

(provide 'init-dired)
