(eval-after-load 'esh-opt
  '(progn
     (require 'em-term)
     (add-to-list 'eshell-visual-commands "ssh")
     ;; vagrant ssh is similar
     (add-to-list 'eshell-visual-subcommands '("vagrant" "ssh"))))

(add-hook 'eshell-preoutput-filter-functions
          'ansi-color-filter-apply)

(global-set-key (kbd "C-c e") 'eshell)

(defun eshell/git-log ()
  (magit-log))

(defun eshell/e (file)
  (find-file file))

(provide 'init-eshell)
