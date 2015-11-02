;;; -*- lexical-binding: t -*-

(defconst base-path
  (file-name-directory
   (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path base-path)

(when (require 'server nil t)
  (unless (server-running-p)
    (server-start)))

(defconst *is-mac*   (eq system-type 'darwin)              "Is macos")
(defconst *is-cocoa* (and *is-mac* (eq window-system 'ns)) "Is cocoa")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(when *is-mac*
  (require 'init-mac))

(require 'init-defaults)
(require 'init-appearance)
(require 'init-keybindings)

(setq custom-file (concat base-path "custom.el"))
(load custom-file 'noerror)

(when after-init-time
  (run-hooks 'after-init-hook))
