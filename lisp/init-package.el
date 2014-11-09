(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))
     
(defun -missing-packages (items)
  (delq nil
	(mapcar (lambda (p)
		  (and (not (funcall 'package-installed-p p)) p))
		items)))

(defun install-missing-packages (items)
  (let ((missing (-missing-packages items)))
    (when missing
      (message "Installing missing packages...")
      (package-refresh-contents)
      (dolist (p missing)
	(package-install p))
      (delete-other-windows))))

(require 'use-package)

(provide 'init-package)
