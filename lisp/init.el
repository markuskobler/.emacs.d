;;; -*- lexical-binding: t -*-

(defconst base-path
  (file-name-directory
   (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path base-path)

(when (require 'server nil t)
  (unless (server-running-p)
    (server-start)))

(defconst *is-mac*   (eq system-type 'darwin)              "Is macos")
(defconst *is-cocoa* (and *is-mac* (eq window-system 'ns)) "Is cocoa")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; (defconst lisp-path  (expand-file-name "lisp" user-emacs-directory))
;; (add-to-list 'load-path lisp-path)
;; (dolist (project (directory-files lisp-path t "\\w+"))
;;   (when (file-directory-p project)
;; 	(add-to-list 'load-path project)))

;; (require 'init-package)
;; (defun init--install-packages ()
;;   (packages-install
;;    '(magit
;;      magit-filenotify
;;      magit-find-file
;;      git-blame
;;      git-gutter
;;      paredit
;;      auto-complete
;;      multiple-cursors
;;      visual-regexp
;;      flycheck
;;      go-mode
;;      gotest
;;      go-autocomplete
;;      go-errcheck
;;      js3-mode
;;      flyspell-lazy)))

;; (condition-case nil
;;     (init--install-packages)
;;   (error
;;    (package-refresh-contents)
;;    (init--install-packages)))

;; (require 'uniquify)
;; (setq uniquify-buffer-name-style 'forward)

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
