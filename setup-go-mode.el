(require 'go-mode-load)

(require 'go-eldoc)
(require 'go-flymake)
(require 'go-flycheck)
(require 'go-autocomplete)
(require 'go-focused-test)

;; TODO check if in path
(setq gofmt-command "goimports")

(add-hook 'go-mode-hook
          (lambda ()
            (setq truncate-lines nil)
            (auto-complete-mode)
            (go-eldoc-setup)
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 4)))

(provide 'setup-go-mode)
