;; Use aspell for spell checking: brew install aspell --lang=en
(setq ispell-program-name "/usr/local/bin/aspell")

(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; Darwin/OS X ls doesn't support --dired out of the box.
;; First option: emulate ls via ls-lisp.
;(setq ls-lisp-use-insert-directory-program nil)
;(require 'ls-lisp)
;; Second options: if GNU coreutils is installed, use gls.
(setq ls-lisp-use-insert-directory-program t)
(setq insert-directory-program "gls")

(provide 'mac)
