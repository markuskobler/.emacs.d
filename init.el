;;; -*- lexical-binding: t -*-

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(eval-when-compile
  (setq inhibit-startup-message t)

  (dolist (m '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp m) (funcall m -1)))

  (defconst *is-linux*
    (eq system-type 'gnu/linux) "is linux")

  (defconst *is-mac*
    (eq system-type 'darwin) "is macos")

  (defconst *is-cocoa*
    (and *is-mac* (eq window-system 'ns)) "is cocoa")

  (defvar use-package-verbose t)
  (require 'use-package))

(require 'bind-key)
(require 'diminish nil t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(progn
  (dolist (p '("init"))
    (add-to-list 'load-path
                 (expand-file-name p user-emacs-directory))))

(require 'init-defaults)
(require 'init-keybindings)

;;
;; helm
;;
(use-package helm
  :ensure t
  :commands
  (helm-get-sources helm-marked-candidates)
  :init
  (setq helm-autoresize-max-height 40
        helm-autoresize-min-height 20
        projectile-completion-system 'helm)

  :config
  (use-package helm-config)
  (helm-projectile-on)
  (helm-autoresize-mode 1)
  (helm-mode 1)

  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)))

(use-package helm-projectile
  :ensure t
  :config
  (projectile-global-mode)

  :bind (("C-c p h" . helm-projectile)
         ("C-c p p" . helm-projectile-switch-project)
         ("C-c p f" . helm-projectile-find-file)
         ("C-c p s g" . helm-projectile-grep)))

;;
;; flycheck
;;
(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-delay 0.3)
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp-checkdoc html-tidy))

  :config
  (set-face-foreground 'flycheck-error "red")

  :bind
  ("C-c C-l" . flycheck-list-errors))

;;
;; dired
;;
(use-package dired-details
  :ensure t
  :init
  (setq-default dired-details-hidden-string "-- ")
  (let ((gls "/usr/local/bin/gls"))
    (when (file-exists-p gls)
            (setq insert-directory-program gls)))

  :config
  (require 'dired-details)
  (dired-details-install))

;;
;; multiple-cursors
;;
(use-package multiple-cursors
  :ensure t
  :bind
  (("C->"     . mc/mark-next-like-this)
   ("C-<"     . mc/mark-previous-like-this)
   ("C-c C-l" . mc/mark-all-like-this)))

;;
;; git
;;
(use-package magit
  :ensure t
  :bind
  ("C-x m" . magit-status))

;;
;; toml
;;
(use-package toml-mode
  :ensure t
  :mode "\\.toml\\'")

;;
;; yaml
;;
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'"
  :config
  (use-package yaml-tomato))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" "\\.jshintrc\\'" "\\.eslintrc\\'" "\\.jscsrc\\'")

  :init
  (setq show-trailing-whitespace t
        js-indent-level 2)

  :config
  (add-hook 'json-mode-hook 'flycheck-mode)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))


;;
;; Dockerfile
;;
(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

;;
;; eldoc
;;
(use-package eldoc
  :ensure t
  :diminish eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))

;;
;; ruby
;;
(use-package ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :init
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;;
;; scala
;;
(use-package ensime
  :ensure t
  :init
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;;
;; nix
;;
(use-package nix-mode
  :mode "\\.nix\\'"
  :init
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;;
;; web
;;
(use-package web-mode
  :ensure t
  :mode "\\.html\\'"
  :init
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;;
;; go
;;
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :init
  (use-package auto-complete   :ensure t)
  (use-package flycheck        :ensure t)
  (use-package go-autocomplete :ensure t)
  (use-package go-eldoc        :ensure t)
  (use-package go-errcheck     :ensure t)
  (use-package gotest          :ensure t)
  (setq gofmt-command "goimports")

  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (go-set-project)

              (flycheck-mode)
              (auto-complete-mode)
              (go-eldoc-setup)
              (add-hook 'before-save-hook 'gofmt-before-save)
              (setq tab-width 4))))


;;
;; paredit
;;
(use-package paredit
  :diminish paredit-mode
  :init
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
  (add-hook 'json-mode-hook 'paredit-mode))


(progn
  (dolist (r '(;; init-recentf
               ;; init-aspell
               ;; init-markdown
               ;; init-ansi
               ;; init-emacs-lisp
               ;; init-tramp
               ;; init-rust
               init-web
               init-css
               init-javascript))
    (funcall 'require r)))

(require 'init-appearance)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (git-gutter-fringe+ darkokai-theme helm use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
