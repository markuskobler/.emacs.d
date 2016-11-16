(when (member "Source Code Pro" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Source Code Pro"))
    (add-to-list 'default-frame-alist '(font . "Source Code Pro")))

(when *is-linux*
  (set-face-attribute 'default nil :height 95))

(setq font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil
      line-number-mode t
      column-number-mode t
      fill-column 80
      redisplay-dont-pause t
      ring-bell-function nil)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(show-paren-mode 1)

(setq custom-theme-directory
      (expand-file-name "themes" user-emacs-directory))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

(use-package darkokai-theme
  :ensure t
  :config
  (progn
    (load-theme 'darkokai t)))

(use-package git-gutter-fringe+
  :ensure t
  :config
  (progn
    (global-git-gutter+-mode)))

(provide 'init-appearance)
