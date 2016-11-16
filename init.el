;;; -*- lexical-binding: t -*-

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(eval-when-compile
  (setq inhibit-startup-message t)

  (progn
    (dolist (m '(menu-bar-mode tool-bar-mode scroll-bar-mode))
      (when (fboundp m) (funcall m -1))))

  (defvar use-package-verbose t)
  (require 'use-package))

(defconst *is-linux*
 (eq system-type 'gnu/linux) "is linux")

(defconst *is-mac*
  (eq system-type 'darwin) "is macos")

(defconst *is-cocoa*
  (and *is-mac* (eq window-system 'ns)) "is cocoa")

(require 'bind-key)
(require 'diminish nil t)
(progn
  (dolist (p '("init"))
    (add-to-list 'load-path
                 (expand-file-name p user-emacs-directory))))

(require 'init-keybindings)

(progn
  (dolist (r '(init-helm
               ;; init-multiple-cursors
               ;; init-dired
               ;; init-recentf
               ;; init-aspell
               ;; init-projectile
               ;; init-toml
               ;; init-markdown
               ;; init-ansi
               ;; init-emacs-lisp
               ;; init-eshell
               ;; init-tramp
               ;; init-web
               ;; init-css
               ;; init-javascript
               ;; init-go
               ;; init-rust
               ;; init-docker
               ;; init-nix
               ;; init-ruby
               ))
    (funcall 'require r)))

(require 'init-appearance)

;; (when *is-mac*
;;   (require 'init-mac))

;; (require 'init-defaults)

;; (with-eval-after-load 'flycheck
;;   (flycheck-pos-tip-mode))

;;(setq custom-file (concat base-path "custom.el"))
;;(load custom-file 'noerror)

;; (when after-init-time
;;   (run-hooks 'after-init-hook))

;; (put 'erase-buffer 'disabled nil)
;; (put 'downcase-region 'disabled nil)


;; (make-directory
;;  (setq tmp-local-dir
;;        (expand-file-name "tmp" user-emacs-directory)) t)

;; (dolist (project (directory-files base-path t "\\w+"))
;;   (when (file-directory-p project)
;;     (add-to-list 'load-path project)))
