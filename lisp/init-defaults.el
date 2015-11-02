(defalias 'yes-or-no-p 'y-or-n-p) ;; Answering with just 'y' or 'n'

(auto-compression-mode t)

(set-default 'sentence-end-double-space nil)

(setq create-lockfiles nil) ;; this might be dangarous...
(setq make-backup-files nil)
(setq auto-save-default nil)

(setq shift-select-mode nil)

(setq truncate-partial-width-windows nil)
(setq-default truncate-lines nil)

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Show active region
(global-font-lock-mode t)
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

(global-subword-mode nil)

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-strip-common-suffix t)
(setq uniquify-after-kill-buffer-p t)

(use-package ido-ubiquitous
  :init (ido-ubiquitous-mode 1))
(use-package ido-vertical-mode
  :init (progn
          (ido-vertical-mode 1)
          (setq ido-max-prospects 5)))
(use-package idomenu
  :bind (("C-c i" . idomenu))
  :config (setq imenu-auto-rescan t))
(use-package flx
  :init
  (progn
    (setq gc-cons-threshold 20000000)
    (use-package flx-ido
      :init
      (flx-ido-mode 1))))

(when (window-system)
  (require 'git-gutter-fringe))

(global-git-gutter-mode +1)
(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

;; Represent undo-history as an actual tree (visualize with C-x u)
;;(setq undo-tree-mode-lighter "")

(provide 'init-defaults)
