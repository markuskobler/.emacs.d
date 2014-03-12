(require 'go-mode-load)

;; TODO check if in path
(setq gofmt-command "goimports")

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            (setq tab-width 2
                  indent-tabs-mode nil)))

;;(require 'go-flymake)
(require 'go-flycheck)
(require 'go-autocomplete)

(provide 'setup-go-mode)
