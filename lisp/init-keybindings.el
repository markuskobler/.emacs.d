;; Cheat Sheet
;; ###########
;; C-x C-+, C-x C--, C-x C-0    ZOOM in, out, reset

(global-set-key (kbd "C-/") 'comment-or-uncomment-region)

;; Turn on the menu bar for exploring new modes
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Jump to a definition in the current file.
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; File finding
(global-set-key (kbd "C-x C-p") 'find-or-create-file-at-point)
(global-set-key (kbd "C-x M-p") 'find-or-create-file-at-point-other-window)
(global-set-key (kbd "M-s f") 'find-name-dired)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-j") 'dired-jump)
;;(global-set-key (kbd "C-x o") 'find-file-in-project)

;; Magit
(global-set-key (kbd "C-x m") 'magit-status)

;;(global-set-key [remap goto-line] 'goto-line-with-feedback)

;;(global-set-key (kbd "C-c C-t") 'run-go-tests)

;; Remove stupid colour palette
(global-set-key (kbd "s-C") 'ns-copy-including-secondary)

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
