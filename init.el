;;; -*- lexical-binding: t -*-

(package-initialize)

(setq inhibit-startup-message t)
(progn
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

(defconst base-path
  (expand-file-name "init" user-emacs-directory))

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

(progn
  (dolist (r '(init-multiple-cursors
               init-helm
               init-dired
               init-recentf
               init-aspell
               init-projectile
;;               init-ansi
;;               init-emacs-lisp
               init-markdown
;;               init-eshell
;;               init-tramp
               init-web
               init-css
               init-javascript
               init-go
               init-rust
               init-toml
;;               init-docker
               init-nix))
    (funcall 'require r)))

(when *is-mac*
  (require 'init-mac))

(require 'init-defaults)
(require 'init-appearance)
(require 'init-keybindings)

;; (with-eval-after-load 'flycheck
;;   (flycheck-pos-tip-mode))

(setq custom-file (concat base-path "custom.el"))
(load custom-file 'noerror)

(when after-init-time
  (run-hooks 'after-init-hook))

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
