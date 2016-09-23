(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(package-install 'use-package)
(require 'use-package)

(use-package auto-package-update
  :ensure
  :config
  (progn
    (auto-package-update-maybe)
    (setq auto-package-update-delete-old-versions t)
    (setq auto-package-update-interval 30)))

(provide 'init-package)
