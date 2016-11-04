(use-package markdown-mode
  :ensure
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  ;; :config
  ;; (progn
  ;;   (setq markdown-imenu-generic-expression
  ;;         '(("title" "^\\(.*\\)[\n]=+$" 1)
  ;;           ("h2-" "^\\(.*\\)[\n]-+$" 1)
  ;;           ("h1" "^# \\(.*\\)$" 1)
  ;;           ("h2" "^## \\(.*\\)$" 1)
  ;;           ("h3" "^### \\(.*\\)$" 1)
  ;;           ("h4" "^#### \\(.*\\)$" 1)
  ;;           ("h5" "^##### \\(.*\\)$" 1)
  ;;           ("h6" "^###### \\(.*\\)$" 1)
  ;;           ("fn" "^\\[\\^\\(.*\\)\\]" 1)))
  ;;   (add-hook 'markdown-mode-hook
  ;;             (lambda ()
  ;;               (setq imenu-generic-expression markdown-imenu-generic-expression))))
  )


(use-package flymd
  :ensure t)

(global-set-key[remap markdown-preview]
 'flymd-flyit)

(provide 'init-markdown)
