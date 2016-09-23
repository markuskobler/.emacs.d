(use-package ispell
  :config
  (progn
    (cond
     ((executable-find "aspell")
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args   '("--sug-mode=ultra"
                                  "--lang=en_US"
                                  "--run-together-limit=5"
                                  "--run-together-min=2"))))

    (use-package flyspell
      :init
      (progn
        ;; Below variables need to be set before `flyspell' is loaded.
        (setq flyspell-use-meta-tab nil)
        ;; Binding for `flyspell-auto-correct-previous-word'.
        (setq flyspell-auto-correct-binding (kbd "<S-f12>")))
      :config
      (progn
        (add-hook 'prog-mode-hook #'flyspell-prog-mode)
        (with-eval-after-load 'auto-complete
          (ac-flyspell-workaround))
        ;; https://github.com/larstvei/dot-emacs#flyspell
        (add-hook 'text-mode-hook #'turn-on-flyspell)
        (add-hook 'org-mode-hook  #'turn-on-flyspell)

        ;; Flyspell signals an error if there is no spell-checking tool is
        ;; installed. We can advice `turn-on-flyspell' and `flyspell-prog-mode'
        ;; to try to enable flyspell only if a spell-checking tool is available.
        (defun modi/ispell-not-avail-p (&rest args)
          "Return `nil' if `ispell-program-name' is available; `t' otherwise."
          (not (executable-find ispell-program-name)))
        (advice-add 'turn-on-flyspell   :before-until #'modi/ispell-not-avail-p)
        (advice-add 'flyspell-prog-mode :before-until #'modi/ispell-not-avail-p)

        ;; https://github.com/d12frosted/flyspell-correct
        ;;(use-package flyspell-correct-ivy
        ;;  :after flyspell-correct
        ;;  :bind (:map modi-mode-map
	;;      ("<f12>" . flyspell-correct-word-generic)))

        ;; (bind-keys
        ;;  :map flyspell-mode-map
	;;  ;; Stop flyspell overriding other key bindings
	;;  ("C-," . nil)
	;;  ("C-." . nil)
	;;  ("<C-f12>" . flyspell-goto-next-error))
        ))))

(provide 'init-aspell)
