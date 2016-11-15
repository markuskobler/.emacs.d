(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; (package-install 'use-package)
;; (require 'use-package)

;; (use-package auto-package-update
;;   :ensure t
;;   :config (progn
;;             (auto-package-update-maybe)
;;             (setq auto-package-update-delete-old-versions t)
;;             (setq auto-package-update-interval 30)))

(provide 'init-package)
