;;; clinic-locator-project --- package to help with development in Python projects

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:

(provide 'clinic-locator-project)

(setq async-shell-command-display-buffer nil)

(defun async-shell-command-no-window (command)
  "Requisite Documentation"
  (interactive)
  (let ((display-buffer-alist
         (list
          (cons
           "\\*Async Shell Command\\*.*"
           (cons #'display-buffer-no-window nil)))))
    (async-shell-command command)))

(defun run-angular-dev-mode ()
  (interactive)
  (async-shell-command-no-window "source ~/bin/locator-client-dev.sh"))

(global-set-key (kbd "<f5>") 'run-angular-dev-mode)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
