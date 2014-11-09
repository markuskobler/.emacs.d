;;; -*- lexical-binding: t -*-

(setq inhibit-startup-message t)
(progn
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

(defconst base-path
  (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path base-path)

(make-directory
 (setq tmp-local-dir (expand-file-name "tmp" user-emacs-directory)) t)

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
   paredit
   auto-complete
   visual-regexp
   magit
   magit-filenotify
   magit-find-file
   git-blame
   git-gutter
   multiple-cursors
   ido-vertical-mode
   idomenu
   flx
   flx-ido
   flycheck
   go-mode
   go-autocomplete
   go-eldoc
   gotest
   go-errcheck
   sass-mode
   web-mode
   json-mode
   flymake-json
   js2-mode
   ac-js2
   js-doc
   tern
   tern-auto-complete
   jss
   nodejs-repl
   markdown-mode
   flyspell-lazy
   web-beautify
   ansible))

(progn
  (dolist (r '(init-dired
               init-recentf
               init-ansi
               init-multiple-cursors
               init-emacs-lisp
               init-markdown
               init-eshell
               init-tramp
               init-javascript
               init-go))
    (funcall 'require r)))

(when *is-mac*
  (require 'init-mac))

(require 'init-defaults)
(require 'init-appearance)
(require 'init-keybindings)

(setq custom-file (concat base-path "custom.el"))
(load custom-file 'noerror)

(when after-init-time
  (run-hooks 'after-init-hook))
