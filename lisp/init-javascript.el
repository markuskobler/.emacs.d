(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :config
  (progn
    ;; Use lambda for anonymous functions
    (font-lock-add-keywords
     'js2-mode `(("\\(function\\) *("
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1) "\u0192")
                            nil)))))

    ;; Use right arrow for return in one-line functions
    (font-lock-add-keywords
     'js2-mode `(("function *([^)]*) *{ *\\(return\\) "
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1) "\u2190")
                            nil)))))

    (defun my-paredit-js ()
      (interactive)
      (set (make-local-variable 'paredit-space-for-delimiter-predicates)
           '((lambda (endp delimiter) nil)))
      (paredit-mode 1))

    (setq-default js2-basic-offset 2)
    (setq-default js2-indent-level 2)
    (setq-default js2-show-parse-errors nil)
    (setq-default js2-strict-missing-semi-warning nil)
    (setq-default js2-strict-trailing-comma-warning nil)
    (setq-default js2-strict-inconsistent-return-warning nil)
    (setq-default js2-bounce-indent-p nil)

    (add-hook 'js2-mode-hook
              (lambda ()
                (setq js2-highlight-level 1

                      js2-consistent-level-indent-inner-bracket-p nil
                      js2-pretty-multiline-decl-indentation-p nil
                      js2-highlight-external-variables nil
                      js2-mode-show-parse-errors nil
                      js2-show-parse-errors nil

                      show-trailing-whitespace t)

                ;; (my-paredit-js)
                ;; (define-key js2-mode-map "{" 'paredit-open-curly)
                ;; (define-key js2-mode-map "}" 'paredit-close-curly-and-newline)

                (define-key js2-mode-map [(return)]
                  '(lambda()
                     (interactive)
                     (insert "\n")
                     ;;(indent-relative)
                     ))

                (tern-mode t)
                (auto-complete-mode)
                (tern-ac-setup)))))

(use-package jsx-mode
  :mode ("\\.jsx\\'" . jsx-mode)
  :config
  (progn
    ;; Use lambda for anonymous functions
    (font-lock-add-keywords
     'jsx-mode `(("\\(function\\) *("
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1) "\u0192")
                            nil)))))

    ;; Use right arrow for return in one-line functions
    (font-lock-add-keywords
     'jsx-mode `(("function *([^)]*) *{ *\\(return\\) "
                  (0 (progn (compose-region (match-beginning 1)
                                            (match-end 1) "\u2190")
                            nil)))))

    (defun my-paredit-js ()
      (interactive)
      (set (make-local-variable 'paredit-space-for-delimiter-predicates)
           '((lambda (endp delimiter) nil)))
      (paredit-mode 1))

    (setq-default jsx-basic-offset 2)
    (setq-default jsx-indent-level 2)
    (setq-default jsx-show-parse-errors nil)
    (setq-default jsx-strict-missing-semi-warning nil)
    (setq-default jsx-strict-trailing-comma-warning nil)
    (setq-default jsx-strict-inconsistent-return-warning nil)
    (setq-default jsx-bounce-indent-p nil)

    (add-hook 'jsx-mode-hook
              (lambda ()
                (setq js2-highlight-level 1

                      js2-consistent-level-indent-inner-bracket-p nil
                      js2-pretty-multiline-decl-indentation-p nil
                      js2-highlight-external-variables nil
                      jsx-mode-show-parse-errors nil
                      js2-show-parse-errors nil

                      show-trailing-whitespace t)

                ;; (my-paredit-js)
                ;; (define-key jsx-mode-map "{" 'paredit-open-curly)
                ;; (define-key jsx-mode-map "}" 'paredit-close-curly-and-newline)

                (define-key jsx-mode-map [(return)]
                  '(lambda()
                     (interactive)
                     (insert "\n")
                     ;;(indent-relative)
                     ))

                (tern-mode t)
                (auto-complete-mode)
                (tern-ac-setup)))))


(use-package json-mode
  :mode (("\\.json\\'" . json-mode)
         ("\\.jshintrc\\'" . json-mode))
  :config
  (progn
    (add-hook 'json-mode-hook
              (lambda ()
                (setq show-trailing-whitespace t)
                (setq js-indent-level 2)))))

(provide 'init-javascript)
;;; init-javascript.el ends here