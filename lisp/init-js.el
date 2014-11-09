(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :config
  (progn
    (use-package ac-js2)
    (use-package flymake-jsxhint
      :config
      (add-hook 'js2-mode-hook
                (lambda ()
                  (flycheck-mode t))))

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
    
    (defun my-paredit-nonlisp ()
      "Turn on paredit mode for non-lisps."
      (interactive)
      (set (make-local-variable 'paredit-space-for-delimiter-predicates)
           '((lambda (endp delimiter) nil)))
      (paredit-mode 1))

    (add-hook 'js2-mode-hook 'my-paredit-nonlisp) 
    (add-hook 'ruby-mode-hook 'git-gutter-mode)
    (add-hook 'js2-mode-hook
              (lambda ()
                (subword-mode)
                
                (setq show-trailing-whitespace 't)

                (define-key js2-mode-map "{" 'paredit-open-curly)
                (define-key js2-mode-map "}" 'paredit-close-curly-and-newline)
                (setq
                 js2-highlight-level 3
                 js2-show-parse-errors nil
                 js2-strict-missing-semi-warning nil
                 js2-strict-trailing-comma-warning nil
                 js2-basic-offset 2
                 js2-consistent-level-indent-inner-bracket-p t
                 js2-pretty-multiline-decl-indentation-p t
                 js2-highlight-external-variables nil)))))


(use-package json-mode
  :mode ("\\.json\\'" . json-mode)
  :config
    (add-hook 'json-mode-hook 'git-gutter-mode))

(provide 'init-js)
