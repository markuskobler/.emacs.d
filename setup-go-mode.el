(require 'go-mode-load)

(require 'go-eldoc)
(require 'go-flymake)
(require 'go-flycheck)
(require 'go-autocomplete)
(require 'go-focused-test)

;; TODO check if in path
(setq gofmt-command "goimports")

(add-hook 'go-mode-hook 'auto-complete-mode)
(add-hook 'go-mode-hook
          (lambda ()
            (setq truncate-lines nil)
            (go-eldoc-setup)
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)))

(global-set-key (kbd "C-c C-t") 'run-go-tests)

(provide 'setup-go-mode)
