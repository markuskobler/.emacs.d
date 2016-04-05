(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :init
  (progn
    (use-package auto-complete)
    (use-package go-eldoc)
    (use-package go-autocomplete)
    (use-package go-flymake)
    (use-package go-flycheck)
    (add-hook 'go-mode-hook
              (lambda ()
                (setenv "GO15VENDOREXPERIMENT" "1")
                (setenv "GOPATH" "")
                (setq truncate-lines nil)
                (auto-complete-mode)
                (go-eldoc-setup)
                (add-hook 'before-save-hook 'gofmt-before-save)
                (setq tab-width 4)

                ;; (let ((oracle (concat (getenv "GOPATH") "/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")))
                ;;   (when (file-exists-p oracle)
                ;;     (load oracle)
                ;;     (go-oracle-mode)))
                ))))

(setq gofmt-command "goimports")

(provide 'init-go)
