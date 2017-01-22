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

;; TODO: Tidy this block up
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
;; multiple-cursors
;;
(use-package multiple-cursors
  :ensure t
  :bind
  (("C->"     . mc/mark-next-like-this)
   ("C-<"     . mc/mark-previous-like-this)
   ("C-c C-l" . mc/mark-all-like-this)))

;;
;; dired
;;
(use-package dired-details
  :ensure t
  :config
  (require 'dired-details :ensure t)
  (setq-default dired-details-hidden-string "-- ")
  (let ((gls "/usr/local/bin/gls"))
    (when (file-exists-p gls)
            (setq insert-directory-program gls)))

  (dired-details-install))

;;
;; helm
;;
(use-package helm
  :ensure t
  :commands
  (helm-get-sources helm-marked-candidates)
  :config
  (require 'helm-config)
  (setq helm-autoresize-min-height 20)
  (setq helm-autoresize-max-height 40)
  (setq projectile-completion-system 'helm)

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
  :defer t
  :config
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp-checkdoc
                  html-tidy
                  javascript-jscs
                  javascript-jshint))
  (setq flycheck-display-errors-delay 0.3)
  (set-face-foreground 'flycheck-error "red")
  ;; (global-flycheck-mode)
  ;; (setq flycheck-eslintrc "~/.eslintrc")
  ;; (flycheck-add-mode 'javascript-eslint 'js-mode)
  ;; (flycheck-add-mode 'javascript-eslint 'js2-mode)
  ;; (flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)

  :bind ("C-c C-l" . flycheck-list-errors))

;; (with-eval-after-load 'flycheck
;;   (flycheck-pos-tip-mode))

;;
;; paredit
;;
(use-package paredit
  :ensure t
  :defer t
  :init
  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

;;
;; git
;;
(use-package magit
  :ensure t
  :bind
  ("C-x m" . magit-status))


(use-package git-gutter-fringe
  :ensure t
  :config
  (set-face-foreground 'git-gutter-fr:modified "yellow")
  (set-face-foreground 'git-gutter-fr:added    "blue")
  (set-face-foreground 'git-gutter-fr:deleted  "white"))


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
  (use-package yaml-tomato :ensure t))

;;
;; json
;;
(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" "\\.jshintrc\\'")
  :config
  (setq show-trailing-whitespace t)
  (setq js-indent-level 2)
  (flycheck-mode t)
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
;; (use-package eldoc
;;   :ensure t
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))

;;
;; ruby
;;
(use-package ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :config
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;;
;; scala
;;
(use-package ensime
  :ensure t
  :mode ("\\.scala\\'" "\\.sc\\'")
  :config
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;;
;; nix
;;
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :config
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;;
;; web
;;
(use-package web-mode
  :ensure t
  :mode "\\.html\\'"
  :config
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
  :config
  (use-package auto-complete   :ensure t)
  (use-package go-add-tags     :ensure t)
  (use-package go-autocomplete :ensure t)
  (use-package go-eldoc        :ensure t)
  (use-package go-errcheck     :ensure t)
  (use-package go-gopath       :ensure t)
  (use-package go-projectile   :ensure t)
  (use-package golint          :ensure t)
  (use-package gotest          :ensure t)

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
;; rust
;;
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (setq racer-rust-src-path "~/code/vendor/rust/src/")
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.2)
  (setq rust-ident-offset 4)
  (setq tab-width 4)

  (use-package racer
    :ensure t
    :config
    (eldoc-mode t)
    (company-mode t))

  (racer-mode t))

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
;; OCaml
;;
(use-package merlin
  :ensure t
  :mode (("\\.ml\\'" . merlin-mode)
         ("\\.mli\\'" . merlin-mode))
  :init
  (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))))

  :config
  ;; (use-package ocp-indent :ensure t)
  (use-package tuareg :ensure t)
  (use-package flycheck-ocaml :ensure t)

  (tuareg-mode)
  (flycheck-mode t)
  (company-mode t)

  ;; (use-package utop
  ;;   :ensure t
  ;;   :config
  ;;   (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t))
  )

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
  :config
  (use-package tern :ensure t)
  
  (setq js2-allow-rhino-new-expr-initializer nil)
  (setq js2-basic-offset 2)
  (setq js2-highlight-level 3)
  (setq js2-include-node-externs t)
  (setq js2-missing-semi-one-line-override nil)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-strict-trailing-comma-warning nil)
  (setq js2-warn-about-unused-function-arguments t)

  (setq-default flycheck-temp-prefix ".")
  (setq flycheck-eslintrc "~/.eslintrc")

  (add-hook 'js2-mode-hook
            (lambda ()
              (flycheck-select-checker 'javascript-eslint)))

  (add-hook 'js2-jsx-mode
            (lambda ()
              ;; incremental dom does not need
              (setq js2-node-has-side-effects nil)))

  (tern-mode t)
  (flycheck-mode t))

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

;; (require 'init-aspell)

(when after-init-time
  (run-hooks 'after-init-hook))
