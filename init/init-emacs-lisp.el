(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(use-package eldoc
  :diminish eldoc-mode)

(provide 'init-emacs-lisp)
