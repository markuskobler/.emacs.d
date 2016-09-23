(defconst *is-linux*
  (eq system-type 'gnu/linux) "is linux")

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
      redisplay-dont-pause t)

;; ignore for now
;; (setq visible-bell t)
(setq ring-bell-function 'ignore)

;;(global-hl-line-mode 1)
(show-paren-mode 1)

(setq custom-theme-directory
      (expand-file-name "themes" user-emacs-directory))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

;; (defun load-default-black ()
;;   (load-theme 'default-black t))

;; (setq solarized-high-contrast-mode-line t)

;; (defun load-solarized ()
;;   (load-theme 'solarized-dark t))

;; (defun load-afternoon ()
;;   (load-theme 'afternoon t))

;; (add-hook 'after-init-hook 'load-solarized)
;; (add-hook 'after-init-hook 'load-afternoon)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))


(use-package darkokai-theme
  :ensure
  :config
  (progn
    (load-theme 'darkokai t)))


(use-package git-gutter-fringe+
  :ensure
  :config
  (progn
    (global-git-gutter+-mode)))

;;(when (window-system)
;; (require 'git-gutter-fringe))

;;(global-git-gutter-mode +1)
;;(setq-default indicate-buffer-boundaries 'left)
;;(setq-default indicate-empty-lines +1)


(provide 'init-appearance)
