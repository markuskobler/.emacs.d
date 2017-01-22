(defalias 'yes-or-no-p 'y-or-n-p)

(setq confirm-nonexistent-file-or-buffer nil
      create-lockfiles nil
      make-backup-files nil
      auto-save-default nil)

(auto-compression-mode t)

(use-package auto-complete :ensure)
(ac-config-default)

(global-auto-revert-mode 1)
(setq echo-keystrokes 0.1)

(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; (set-default 'sentence-end-double-space nil)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; fix the £/# on a GB keyboard
;; (global-set-key (kbd "M-3")
;;                 '(lambda()
;;                    (interactive) (insert "#")))

;; ignore mouse wheel swipes
(global-set-key [wheel-left] 'ignore)
(global-set-key [wheel-right] 'ignore)
(global-set-key [double-wheel-up] 'ignore)
(global-set-key [double-wheel-down] 'ignore)
(global-set-key [double-wheel-left] 'ignore)
(global-set-key [double-wheel-right] 'ignore)
(global-set-key [triple-wheel-up] 'ignore)
(global-set-key [triple-wheel-down] 'ignore)
(global-set-key [triple-wheel-left] 'ignore)
(global-set-key [triple-wheel-right] 'ignore)

(provide 'init-defaults)
