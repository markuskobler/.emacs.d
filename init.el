(setq inhibit-startup-message t)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq is-mac    (equal system-type 'darwin))
(setq lisp-path (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'load-path lisp-path)

;; Add external projects to load path
(dolist (project (directory-files lisp-path t "\\w+"))
  (when (file-directory-p project)
	(add-to-list 'load-path project)))

(require 'appearance)

(require 'setup-package)
(defun init--install-packages ()
  (packages-install
   '(magit
     magit-filenotify
     magit-find-file
     git-blame
     git-gutter
     paredit
     auto-complete
     multiple-cursors
     visual-regexp
     go-mode
     gotest
     go-autocomplete
     go-errcheck
     flymake
     flymake-go
     flymake-jshint
     flymake-json
     flymake-sass
     js3-mode
     flyspell-lazy
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


(eval-after-load 'magit             '(require 'setup-magit))
(eval-after-load "multiple-cursors" '(require 'multiple-cursors))

(autoload 'auto-complete-mode "auto-complete" nil t)
(autoload 'magit-status "magit")
(autoload 'dired-jump "dired")


(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))
(when is-mac (require 'mac))

(load (expand-file-name "custom.el" user-emacs-directory))
(require 'defaults)
(require 'key-bindings)
