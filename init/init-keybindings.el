;; Cheat Sheet
;; ###########
;; C-x C-+, C-x C--, C-x C-0    ZOOM in, out, reset

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

(global-set-key (kbd "C-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-l") 'goto-line)

;; golang
;; (global-set-key (kbd "C-c C-t") 'run-go-tests)
(local-set-key (kbd "M-.") 'godef-jump)

(provide 'init-keybindings)
