(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)

(setq is-mac    (equal system-type 'darwin))
(setq lisp-path (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'load-path lisp-path)

;; Add external projects to load path
(dolist (project (directory-files lisp-path t "\\w+"))
  (when (file-directory-p project)
	(add-to-list 'load-path project)))

(require 'appearance)

(require 'setup-package)

(defun init--install-packages ()                                               
  (packages-install                                                            
   '(magit
     magit-filenotify
     magit-find-file
     git-blame
     git-gutter
     paredit                                                                   
     auto-complete                                                             
     visual-regexp)))

(condition-case nil                                                            
    (init--install-packages)                                                   
  (error                                                                       
   (package-refresh-contents)                                                  
   (init--install-packages)))

;;(eval-after-load 'magit '(require 'setup-magit))

(require 'uniquify)

;;(require 'undo-tree)
;;(global-undo-tree-mode)

(load (expand-file-name "custom.el" user-emacs-directory))

;;Setup environment variables from the user's shell.

(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))
(when is-mac (require 'mac))

(require 'defaults)
