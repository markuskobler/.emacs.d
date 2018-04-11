;;; -*- lexical-binding: t -*-

(setq gc-cons-threshold 100000000)

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (setq inhibit-startup-message t)

  (dolist (m '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp m) (funcall m -1)))

  (defconst *is-linux*
    (eq system-type 'gnu/linux) "is linux")

  (defconst *is-mac*
    (eq system-type 'darwin) "is macos")

  (defvar use-package-verbose t)
  (require 'use-package))

(require 'bind-key)
(require 'diminish nil t)

(when after-init-time
  (run-hooks 'after-init-hook))

(progn
  (dolist (p '("init" "vendor/lsp-rust" "vendor/reason"))
    (add-to-list 'load-path
                 (expand-file-name p user-emacs-directory))))

(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)

(require 'init-defaults)
(require 'init-appearance)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

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

;; (let ((p "/usr/local/bin"))
;;   (setenv "PATH" (concat (getenv "PATH") ":" p))
;;   (add-to-list 'exec-path p))

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

;; ivy
(use-package ivy
  :ensure t
  ;; :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1)
  :bind
  ("C-c C-r" . ivy-resume)
  ("C-x b" . ivy-switch-buffer))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-mode-line
        '(:eval (format " [%s]" (projectile-project-name))))
  (setq projectile-remember-window-configs t)
  (setq projectile-completion-system 'ivy))

;; counsel
(use-package counsel
  :ensure t
  :defer t
  :bind
  ("M-x" . counsel-M-x)
  ;; ("C-z f" . counsel-describe-function)
  ;; ("C-z v" . counsel-describe-variable)
  ("C-c k" . counsel-rg))

(use-package company
  ;; :bind
  ;; ("TAB" . company-complete-common-or-cycle)
  :config
  ;; (setq company-dabbrev-code-modes t)
  ;; (setq company-dabbrev-code-everywhere t)
  (setq company-tooltip-limit 20)                       ; bigger popup window
  (setq company-idle-delay .3)                          ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                           ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command))  ; start autocompletion only after typing
  (add-hook 'lisp-mode-hook 'company-mode))

;;
;; helm
;;
;; (use-package helm
;;   :ensure t
;;   :defer t
;;   :commands
;;   (helm-get-sources helm-marked-candidates)

;;   :init
;;   (require 'helm-config)

;;   :config
;;   (setq helm-autoresize-min-height 20)
;;   (setq helm-autoresize-max-height 40)
;;   (setq projectile-completion-system 'helm)

;;   (helm-projectile-on)
;;   (helm-autoresize-mode 1)
;;   (helm-mode 1)

;;   :bind (("M-x" . helm-M-x)
;;          ("C-x b" . helm-mini)
;;          ("C-x C-f" . helm-find-files)))

;; (use-package helm-projectile
;;   :ensure t
;;   :defer t
;;   :config
;;   (projectile-global-mode)
;;   (setq helm-projectile-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
;;   (setq helm-projectile-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))
;;   (setq projectile-enable-caching t)

;;   :bind (("C-c p h" . helm-projectile)
;;          ("C-c p p" . helm-projectile-switch-project)
;;          ("C-c p f" . helm-projectile-find-file)
;;          ("C-c p g" . helm-projectile-ag)))

;;
;; flycheck
;;
(use-package flycheck
  :ensure t
  :defer t
  ;; :init
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)

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
  (add-hook 'go-mode-hook 'flycheck-mode)

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
;; undo-tree
;;
;; (use-package undo-tree
;;   :ensure t
;;   :diminish undo-tree-mode
;;   :config
;;   (global-undo-tree-mode 1))

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

  (setq whitespace-line-column 120)

  (setq whitespace-style
        (quote (face trailing lines-tail)))

  ;; (add-hook 'find-file-hook 'whitespace-mode)

  (setq whitespace-display-mappings '((tab-mark 9 [9654 9] [92 9]))))

;;
;; git
;;
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x m" . magit-status))

(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config
  (setq git-gutter-fr:side 'left-fringe)
  (set-face-foreground 'git-gutter-fr:modified "blue")
  (set-face-foreground 'git-gutter-fr:added    "green")
  (set-face-foreground 'git-gutter-fr:deleted  "red")
  (global-git-gutter-mode t))

;;
;; toml
;;
(use-package toml-mode
  :ensure t
  :defer t
  :mode "\\.toml\\'")

;;
;; yaml
;;
(use-package yaml-mode
  :ensure t
  :defer t
  :mode "\\.\\(e?ya?\\|ra\\)ml\\'"
  :config
  (use-package yaml-tomato :ensure t))

;;
;; json
;;
(use-package json-mode
  :ensure t
  :defer t
  :mode ("\\.json\\'" "\\.jshintrc\\'")
  :config
  (setq js-indent-level 4)
  (flycheck-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;;
;; Dockerfile
;;
(use-package dockerfile-mode
  :ensure t
  :defer t
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
  :defer t
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :config
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;;
;; java
;;
;; (use-package java-mode
;;   :ensure t
;;   :mode ("\\.java\\'")
;;   :config
;;   ;; (setq eclimd-executable "/Applications/Eclipse.app/Contents/Eclipse/eclimd")
;;   (add-hook 'before-save-hook 'delete-trailing-whitespace))

;;
;; nix
;;
(use-package nix-mode
  :ensure t
  :defer t
  :mode "\\.nix\\'"
  :config
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;;
;; web
;;
(use-package web-mode
  :ensure t
  :defer t
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
  :defer t
  :mode "\\.go\\'"
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook
            (lambda ()
              (subword-mode)
              (go-set-project)
              (company-mode t)
              (setq tab-width 4)
              (setq indent-tabs-mode 1)))

  :bind
  ("M-." . godoc-at-point)
  ("C-x r" . go-rename)
  ("C-x C-k" . go-test-current-test)
  ("C-x t" . go-test-current-test)
  ("C-x f" . go-test-current-file)
  ("C-x b" . go-test-current-benchmark)
  ("C-x p" . go-test-current-project)
  ("C-x x" . go-run))

(use-package go-projectile
  :ensure t)

(use-package go-eldoc
  :after go-mode)

(use-package gotest
  :after go-mode)

(use-package go-gopath
  :after go-mode)

(use-package go-rename
  :after go-mode)

(use-package go-guru
  :ensure t
  :after go-mode
  :config
  (set-face-attribute 'go-guru-hl-identifier-face nil
                      :inherit 'isearch))

(use-package company-go
  :ensure t
  :after go-mode
  :config
  (add-to-list 'company-backends 'company-go))

;; (use-package go-add-tags
;;   :ensure t
;;   :config
;;   (with-eval-after-load 'go-mode
;;     (define-key go-mode-map (kbd "C-x t") #'go-add-tags)))

;; (use-package flycheck-gometalinter
;;   :ensure t
;;   :config
;;   (flycheck-gometalinter-setup)
;;   (setq flycheck-gometalinter-fast t)
;;   (setq flycheck-gometalinter-disable-linters '("gotype")))

;; go-direx
;; (use-package go-direx
;;   :ensure t
;;   :defer t
;;   :config
;;   (define-key go-mode-map (kbd "C-c C-t") 'go-direx-switch-to-buffer))

;; (use-package lsp-mode
;;   :ensure t
;;   :config
;;   (global-lsp-mode t)
;;   (with-eval-after-load 'lsp-mode
;;     (require 'lsp-flycheck)))

;;
;; rust
;;
(use-package rust-mode
  :ensure t
  :defer t
  :mode "\\.rs\\'"
  :init
  (add-hook 'rust-mode-hook #'racer-mode)

  :config
  (setq rust-ident-offset 4)
  (setq tab-width 4)

  ;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)

  ;; (setq company-tooltip-align-annotations t)
  ;; (setq company-minimum-prefix-length 1)
  ;; (setq company-idle-delay 0.2)

  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package racer
  :ensure t
  :defer t
  :init
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

;;
;; OCaml
;;
;; (use-package merlin
;;   :ensure t
;;   :mode (("\\.ml\\'" . merlin-mode)
;;          ("\\.mli\\'" . merlin-mode))
;;   :init
;;   (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
;;     (when (and opam-share (file-directory-p opam-share))
;;       (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))))

;;   :config
;;   ;; (use-package ocp-indent :ensure t)
;;   (use-package tuareg :ensure t)
;;   (use-package flycheck-ocaml :ensure t)

;;   (tuareg-mode)
;;   (flycheck-mode t)
;;   (company-mode t)

;;   ;; (use-package utop
;;   ;;   :ensure t
;;   ;;   :config
;;   ;;   (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t))
;;   )

;;
;; reason
;;
(use-package reason-mode
  :ensure f
  :mode (("\\.re\\'" . reason-mode)
         ("\\.rei\\'" . reason-mode))
  :config
  ;; (use-package merlin :ensure t)
  ;; (use-package ocp-indent :ensure t)
  ;; (add-hook 'reason-mode-hook
  ;;           (lambda ()
  ;;             ;; (add-hook 'before-save-hook 'refmt-before-save)
  ;;             (merlin-mode)))
  (setq merlin-ac-setup t))

;;
;; css/sass
;;
(use-package css-mode
  :ensure t
  :defer t
  :mode "\\.css\\'"
  :config
  (setq-default css-basic-offset 2)
  (setq-default css-indent-offset 2)

  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package scss-mode
  :ensure t
  :defer t
  :mode "\\.scss\\'"
  :init
  (use-package rainbow-mode :ensure t)

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

(use-package company-tern
  :ensure t
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package js2-mode
  :ensure t
  :defer t
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
;; HTML
;;
(use-package web-mode
  :ensure t
  :defer t
  :mode "\\.html\\'"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

;;
;; Org
;;
(use-package org-bullets
  :ensure t
  :defer t
  :config
  (add-hook 'org-mode-hook
            (lambda () (org-bullets-mode 1))))

;;
;; python
;;
(use-package python
  :defer t
  :mode (("\\.py\\'" . python-mode)
         ("\\.bzl\\'" . python-mode)
         ("\\.bazel\\'" . python-mode)
         ("WORKSPACE" . python-mode))
  :interpreter ("python" . python-mode))

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; (require 'init-aspell)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
