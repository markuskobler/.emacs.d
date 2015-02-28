(when (member "Source Code Pro" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Source Code Pro"))
    (add-to-list 'default-frame-alist '(font . "Source Code Pro")))

(setq font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil
      line-number-mode t
      column-number-mode t
      fill-column 80
      redisplay-dont-pause t)

;; ignore for now
;; (setq visible-bell t)
(setq ring-bell-function 'ignore)

(global-hl-line-mode 1)
(show-paren-mode 1)

(setq custom-theme-directory
      (expand-file-name "themes" user-emacs-directory))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

;;(load-theme 'default-black t)

(setq solarized-high-contrast-mode-line t)

(defun load-solarized ()
  (load-theme 'solarized-dark t))
(add-hook 'after-init-hook 'load-solarized)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(provide 'init-appearance)
