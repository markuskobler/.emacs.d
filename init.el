;;; -*- lexical-binding: t -*-

(setq inhibit-startup-message t)
(progn
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

(defconst base-path
  (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path base-path)

(dolist (project (directory-files base-path t "\\w+"))
  (when (file-directory-p project)
	(add-to-list 'load-path project)))

(when (require 'server nil t)
  (unless (server-running-p)
    (server-start)))

(defconst *is-mac*   (eq system-type 'darwin)              "is macos")
(defconst *is-cocoa* (and *is-mac* (eq window-system 'ns)) "is cocoa")

(require 'init-package)

(install-missing-packages
 '(ido-ubiquitous
   dired-details
   ansible
   paredit
   auto-complete
   visual-regexp
   magit
   magit-filenotify
   magit-find-file
   git-blame
   git-gutter
   multiple-cursors
   flycheck
   go-mode
   gotest
   go-autocomplete
   go-errcheck
   js3-mode
   flyspell-lazy))

(progn
  (dolist (r '(init-dired
               init-multiple-cursors
               init-go))
    (funcall 'require r)))


;; (eval-after-load "multiple-cursors" '(require 'multiple-cursors))

;; (eval-after-load 'magit             '(require 'init-magit))

;; (autoload 'flycheck-mode "init-flycheck" nil t)
;; (autoload 'auto-complete-mode "auto-complete" nil t)
;; (autoload 'magit-status "magit")
;; (autoload 'dired-jump "dired")

(when *is-mac*
  (require 'init-mac))

(require 'init-defaults)
(require 'init-appearance)
(require 'init-keybindings)

(setq custom-file (concat base-path "custom.el"))
(load custom-file 'noerror)

(when after-init-time
  (run-hooks 'after-init-hook))
