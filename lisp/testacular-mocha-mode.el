(defvar tm-mode-map (make-sparse-keymap)
  "tm-mode keymap")

(defun tm-run-shell-commands (&rest commands)
  (let ((proc (get-buffer-process (current-buffer-name))))
    (dolist (cmd commands)
      (term-simple-send proc cmd))))

(defun tm-open-term (test-cmd)
  (interactive)
  (malko/kill-tm-tests)
  (split-window-below)
  (windmove-down)
  (halve-current-window-height)
  (let ((tm-git-base-path git-base-path))
    (ansi-term "/bin/bash" (tm-mode--compilation-buffer-name))
    (tm/setup-term-mode-map)
    (visual-line-mode -1)
    (tm-run-shell-commands "PROMPT_COMMAND=\"PS1='> '\""
                           "clear"
                           (tm-compile-command tm-git-base-path test-cmd))))
(defun tm-next-failure ()
  (interactive)
  (end-of-line)
  (search-forward "FAILED" nil t))

(defun tm-prev-failure ()
  (interactive)
  (beginning-of-line)
  (search-backward "FAILED" nil t))

(defun tm-compile-command (tm-git-base-path test-cmd)
  (format "cd %s; %s" tm-git-base-path test-cmd))

(defun tm-run-all-tests ()
  (interactive)
  (tm-open-term "make test"))

(defun tm-run-test ()
  (interactive)
  (let ((spec-file
         (car (last (split-string (file-name-no-extension) "specs/")))))
    (cond
     ((s-contains? "public/apps/jam" (buffer-file-name))
      (tm-open-term (format "make jam_tests FILE=%s" spec-file)))
     ((s-contains? "public/apps/main" (buffer-file-name))
      (tm-open-term (format "make main_tests FILE=%s" spec-file))))))

(defun tm--error-body-with-newlines ()
  (save-excursion
    (push-mark (point) t)
    (tm-next-failure)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun tm--error-body ()
  (replace-regexp-in-string "\n" "" (tm--error-body-with-newlines)))

(defun tm--match-string-all (&optional string)
  "Return the list of all expressions matched in last search.
STRING is optionally what was given to `string-match'."
  (let ((n-matches (1- (/ (length (match-data)) 2))))
    (mapcar (lambda (i) (match-string i string))
            (number-sequence 1 n-matches))))

(defun tm--match-strings-all (re str &optional pos)
  (if (string-match re str (or pos 0))
      (cons (tm--match-string-all str)
            (tm--match-strings-all re str (match-end 0)))
    '()))

(defun tm--error-stack ()
  (let ((error-body (tm--error-body)))
    (tm--match-strings-all
     "(\\([^\)]*\\):\\([0-9]+\\):\\([0-9]+\\))" error-body)))

(defun tm-jump-to-failure ()
  (interactive)
  (let* ((error-stack (tm--error-stack))
         (last-error (car error-stack)))
    (if error-stack
        (let ((file (nth 0 last-error))
              (line (string-to-number (nth 1 last-error)))
              (column (string-to-number (nth 2 last-error))))
          (windmove-up)
          (find-file file)
          (goto-line-and-column line column)))))

(defun tm-switch-to-test-window ()
  (interactive)
  (switch-to-window-by-name "*tm-test*")
  (end-of-buffer))

(defun tm-switch-to-test-window-and-expand ()
  (interactive)
  (switch-to-window-by-name "*tm-test*")
  (balance-windows)
  (end-of-buffer))

(define-key tm-mode-map (kbd "C-j ta") 'tm-run-all-tests)
(define-key tm-mode-map (kbd "C-j te") 'tm-switch-to-test-window-and-expand)
(define-key tm-mode-map (kbd "C-j ts") 'tm-switch-to-test-window)
(define-key tm-mode-map (kbd "C-j tt") 'tm-run-test)

(defun tm/setup-term-mode-map ()
  (when (term-in-char-mode)
    (use-local-map term-old-mode-map)
    (define-key term-old-mode-map (kbd "<return>") 'tm-jump-to-failure)
    (define-key term-old-mode-map (kbd "M-n") 'tm-next-failure)
    (define-key term-old-mode-map (kbd "M-p") 'tm-prev-failure)))

(defun tm-mode--compilation-buffer-name (&rest ignore) "tm-test")
(define-minor-mode testacular-mocha-mode
  "Testacular-Mocha mode" nil " TM" tm-mode-map)

;;;###autoload
(add-hook 'js2-mode-hook (lambda () (testacular-mocha-mode)))

(provide 'testacular-mocha-mode)
