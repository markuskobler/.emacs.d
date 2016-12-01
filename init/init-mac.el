;; (require-package 'exec-path-from-shell)

(setq default-directory (expand-file-name "~/"))

(setq system-name (car (split-string system-name "\\.")))

;; fix the £/# on a GB keyboard
(global-set-key (kbd "M-3")
                '(lambda()
                   (interactive) (insert "#")))

(setq ls-lisp-use-insert-directory-program t)
(setq insert-directory-program "gls")
;; (require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Use aspell for spell checking: brew install aspell --lang=en
;;(setq ispell-program-name "/usr/local/bin/aspell")

(unless *is-cocoa*
  (setq ns-function-modifier 'control
        ns-command-modifier  'super
        ns-function-modifier 'hyper))
  ;; (defun osx-copy ()
  ;;   (shell-command-to-string "pbpaste"))
  ;; (setq interprogram-paste-function 'osx-copy)

  ;; (defun osx-paste (text &optional push)
  ;;   (let ((process-connection-type nil)
  ;;         (proc (start-process "pbcopy" "*Messages*" "pbcopy")))
  ;;     (process-send-string proc text)
  ;;     (process-send-eof proc)))
  ;; (setq interprogram-cut-function 'osx-paste)


(provide 'init-mac)
