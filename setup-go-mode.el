(require 'go-mode-load)


(require 'go-eldoc)
(require 'go-flymake)
;;(require 'go-flycheck)
(require 'go-autocomplete)

;; TODO check if in path
(setq gofmt-command "goimports")

(add-hook 'go-mode-hook 'auto-complete-mode)
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (go-eldoc-setup)
            (setq tab-width 2
                  indent-tabs-mode nil)))

(provide 'setup-go-mode)
