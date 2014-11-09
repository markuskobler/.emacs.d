(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(unless (file-exists-p
         (expand-file-name "elpa/archives/melpa" user-emacs-directory))
  (package-refresh-contents))


(require 'use-package)

(provide 'init-package)
