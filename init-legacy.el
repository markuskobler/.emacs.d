(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings
                                  clojure-mode clojure-test-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq solarized-distinct-fringe-background t)
(setq solarized-high-contrast-mode-line t)
(load-theme 'solarized-light t)

(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
(setq slime-net-coding-system 'utf-8-unix)


(require 'clojure-mode)
(global-hl-line-mode 1)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

;; (require 'midje-mode)
;; (add-hook 'clojure-mode-hook 'midje-mode)

;;(require 'clojure-test-mode)

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-interaction-mode 'paredit-mode)
(setq nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "*nrepl*") 

(add-hook 'after-init-hook #'global-flycheck-mode)

(add-to-list 'load-path "~/.emacs.d/go-mode.el-master/")
(require 'go-mode-load)
(add-hook 'go-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))


(defcustom goflymake-debug nil
  "Enable failure debugging mode in goflymake."
  :type 'boolean
  :group 'goflymake)
(add-to-list 'load-path "~/.emacs.d/goflymake/")
(require 'go-flycheck)

(add-to-list 'load-path "~/.emacs.d/go-autocomplete/")
(require 'go-autocomplete)
(require 'auto-complete-config)
(add-hook 'go-mode-hook 'auto-complete-mode)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)


(add-to-list 'load-path "~/.emacs.d/go-errcheck/")
(require 'go-errcheck)

(add-hook 'go-mode-hook 'imenu-add-menubar-index)
;(add-hook 'go-mode-hook 'go-test-package)
;(add-hook 'go-mode-hook
;(lambda ()
;  (local-set-key (kbd "C-c C-t") 'go-test-package)))

(require 'iedit)

(defun my-go-mode-hook () 
  (setq tab-width 2
        indent-tabs-mode nil)
  (global-linum-mode 1))
(add-hook 'go-mode-hook 'my-go-mode-hook) 


;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

