;; Cheat Sheet
;; ###########
;; C-x C-+, C-x C--, C-x C-0    ZOOM in, out, reset

(global-set-key (kbd "C-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-l") 'goto-line)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c p h") 'helm-projectile)
(global-set-key (kbd "C-c p p") 'helm-projectile-switch-project)
(global-set-key (kbd "C-c p f") 'helm-projectile-find-file)

;; multiple-cursors
(global-set-key (kbd "C->")     'mc/mark-next-like-this)
(global-set-key (kbd "C-<")     'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-l") 'mc/mark-all-like-this)

;; Magit
(global-set-key (kbd "C-x m") 'magit-status)

;; golang
;; (global-set-key (kbd "C-c C-t") 'run-go-tests)
(local-set-key (kbd "M-.") 'godef-jump)

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

(provide 'init-keybindings)
