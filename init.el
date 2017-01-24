;;; -*- lexical-binding: t -*-

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

(when after-init-time
  (run-hooks 'after-init-hook))

(progn
  (dolist (p '("init"))
    (add-to-list 'load-path
                 (expand-file-name p user-emacs-directory))))

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'init-defaults)
(require 'init-appearance)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

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

(global-set-key (kbd "C-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-l") 'goto-line)

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
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)

  :config
  (use-package flycheck-pos-tip :ensure t)

  (setq-default flycheck-temp-prefix ".flycheck")
  (setq-default flycheck-disabled-checkers
                '(emacs-lisp-checkdoc
                  html-tidy
                  javascript-jscs
                  javascript-jshint))
  (setq flycheck-display-errors-delay 0.3)
  ;; (set-face-foreground 'flycheck-error "red")

  (flycheck-pos-tip-mode)

  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/.bin/eslint"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

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

(use-package whitespace
  :ensure t
  :init
  ;; (custom-set-faces
  ;;  '(my-carriage-return-face ((((class color)) (:background "blue"))) t)
  ;;  '(my-tab-face ((((class color)) (:background "green"))) t))

  ;; (add-hook
  ;;  'font-lock-mode-hook
  ;;  (function
  ;;   (lambda ()
  ;;     (setq
  ;;      font-lock-keywords
  ;;      (append
  ;;       font-lock-keywords
  ;;       '(
  ;;         ("\r" (0 'my-carriage-return-face t))
  ;;         ("\t" (0 'my-tab-face t))
  ;;         ))))))

  (setq whitespace-style
        (quote (face trailing lines-tail)))

  (add-hook 'find-file-hook 'whitespace-mode)

  (setq whitespace-display-mappings '((tab-mark 9 [9654 9] [92 9]))))

;;
;; git
;;
(use-package magit
  :ensure t
  :bind
  ("C-x m" . magit-status))

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config
  (setq git-gutter-fr:side 'right-fringe)
  (set-face-foreground 'git-gutter-fr:modified "blue")
  (set-face-foreground 'git-gutter-fr:added    "green")
  (set-face-foreground 'git-gutter-fr:deleted  "red")
  (global-git-gutter-mode t))

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

  (flycheck-mode t)
  (auto-complete-mode)
  (go-set-project)
  (go-eldoc-setup)

  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 4)
              (setq indent-tabs-mode 1)))

  :bind
  ("M-." . godef-jump))

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

  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :config
  (setq-default css-basic-offset 2)
  (setq-default css-indent-offset 2)
  (setq-default scss-compile-at-save nil)

  (rainbow-mode t)
  (flycheck-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;;
;; js/jsx
;;
(use-package tern
  :ensure t
  :defer t)

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :config
  (tern-mode t)
  (flycheck-mode t)

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

  ;; Use lambda for anonymous functions
  (font-lock-add-keywords
   'js2-mode `(("\\(function\\) *("
                (0 (progn (compose-region (match-beginning 1)
                                          (match-end 1) "\u0192")
                          nil)))))

  ;; Use right arrow for return in one-line functions
  (font-lock-add-keywords
   'js2-mode `(("function *([^)]*) *{ *\\(return\\) "
                (0 (progn (compose-region (match-beginning 1)
                                          (match-end 1) "\u2190")
                          nil)))))

  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;;
;; coffee
;;
(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\\'"
  :config
  (setq whitespace-action '(auto-cleanup))
  (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;;
;; HTML
;;
(use-package web-mode
  :ensure t
  :mode "\\.html\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;; (require 'init-aspell)
