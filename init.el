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

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'init-defaults)
(require 'init-keybindings)
(require 'init-appearance)

(let ((p (concat (getenv "HOME") "/bin")))
  (setenv "PATH" (concat (getenv "PATH") ":" p))
  (add-to-list 'exec-path p))

(when *is-mac*
  (let ((p (concat (getenv "HOME") "/.n/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":" p))
    (add-to-list 'exec-path p)))

(let ((p "/usr/local/bin"))
  (setenv "PATH" (concat (getenv "PATH") ":" p))
  (add-to-list 'exec-path p))

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
         ("C-c p g" . helm-projectile-grep)))

;;
;; flycheck
;;
(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-delay 0.3)
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp-checkdoc
                  html-tidy
                  javascript-jshint))

  :config
  (set-face-foreground 'flycheck-error "red")

  :bind
  ("C-c C-l" . flycheck-list-errors))

;; (with-eval-after-load 'flycheck
;;   (flycheck-pos-tip-mode))

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
  :mode "Dockerfile")

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
  (use-package go-add-tags     :ensure t)
  (use-package go-autocomplete :ensure t)
  (use-package go-eldoc        :ensure t)
  (use-package go-errcheck     :ensure t)
  (use-package go-gopath       :ensure t)
  (use-package go-projectile   :ensure t)
  (use-package golint          :ensure t)
  (use-package gotest          :ensure t)

  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 4)
              (flycheck-mode t)
              (auto-complete-mode)
              (go-set-project)
              (go-eldoc-setup)))
  :bind
  (("M-." . godef-jump)))

;;
;; R
;;
(use-package ess-site
  :ensure ess
  :functions poin-max
  :mode ("\\.R\\'" . R-mode)
  :commands R
  :config
  (add-hook 'R-mode-hook #'subword-mode))

;;
;; paredit
;;
(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
  (add-hook 'json-mode-hook 'paredit-mode))

;;
;; css/sass
;;
(use-package css-mode
  :ensure t
  :mode "\\.css\\'"
  :config
  (setq-default css-basic-offset 2)
  (setq-default css-indent-offset 2)
  :init
  (add-hook 'css-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'delete-trailing-whitespace))))

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :config
  (setq-default css-basic-offset 2)
  (setq-default css-indent-offset 2)
  (setq-default scss-compile-at-save nil)
  :init
  (add-hook 'scss-mode-hook
            (lambda ()
              (rainbow-mode t)
              (flycheck-mode t)
              (add-hook 'before-save-hook 'delete-trailing-whitespace))))

;;
;; js
;;
(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :init
  (setq js2-highlight-level 3
        js2-strict-trailing-comma-warning nil
        js2-strict-missing-semi-warning nil
        js2-missing-semi-one-line-override t
        js2-allow-rhino-new-expr-initializer nil
        js2-include-node-externs t
        js2-warn-about-unused-function-arguments t
        js2-basic-offset 2)

  ;; (setq-default flycheck-temp-prefix ".")

  ;; (setq flycheck-eslintrc "~/.eslintrc")

  (add-hook 'js2-mode-hook
            (lambda ()
              (subword-mode 1)
              (flycheck-mode t)
              (diminish 'subword-mode)
              ;; (when (executable-find "eslint")
              ;;   (flycheck-select-checker 'javascript-eslint))
              ))

  :config
  (use-package tern
    :ensure t
    :diminish tern-mode
    :init
    (add-hook 'js2-mode-hook 'tern-mode)))

;;
;; coffee
;;
(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\\'"
  :init
  (setq whitespace-action '(auto-cleanup))
  (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)))

;;
;; HTML
;;
(use-package web-mode
  :ensure t
  :mode "\\.html\\'"
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (add-hook 'web-mode-hook
              (lambda ()
                (add-hook 'before-save-hook 'delete-trailing-whitespace)))))

(require 'init-aspell)

;; (progn
;;   (dolist (r '(;; init-aspell
;;                ;; init-markdown
;;                ;; init-ansi
;;                ;; init-emacs-lisp
;;                ;; init-tramp
;;                ;; init-rust
;;                init-web))
;;     (funcall 'require r)))

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
