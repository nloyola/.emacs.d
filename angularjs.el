;;; angularjs --- package to help with development of projects that use AngularJS

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:

(defun ngcomp-snippet-get-controller-name ()
  "Replace the word `Component' in the file name with `Controller'."
  (upcase-initials (replace-regexp-in-string "Component" "Controller" (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))))

(defun ngcomp-snippet-get-service-name-no-suffix ()
  "Remove the word `Service' from the file name."
  (replace-regexp-in-string "Service" "" (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))

(defun ngcomp-snippet-get-template-url ()
  "Return the name of the templateUrl for a component."
  (concat "./" (replace-regexp-in-string "\\(Component\\|Directive\\)$" ".html" (file-name-base buffer-file-name) t)))

;; borrowed from js2-refactor
(defun nl/angularjs--current-quotes-char ()
  "The char that is the current quote delimiter."
  (nth 3 (syntax-ppss)))

(defun nl/angularjs-gettext-surround-string ()
  "Surrounds string with a call to gettext.  Cursor must be at string's start."
  (interactive)
  (if (looking-at "'")
      (save-excursion
        (insert "gettext(")
        (forward-char)
        (while (nl/angularjs--current-quotes-char)
          (forward-char))
        (insert ")"))
    (message "not at the start of a string")))

(defun nl/angularjs-js-mode-keys ()
  "Key definitions for 'js-mode' in bbweb project."
  (interactive)
  (local-set-key (kbd "C-c g") 'nl/angularjs-gettext-surround-string))

(add-hook 'js-mode-hook 'nl/angularjs-js-mode-keys)

(defun nl/angularjs-gettext-surround-html-string ()
  "Surrounds string with a call to gettext.  Cursor must be at string's start."
  (interactive)
  (if (looking-at "\"")
      (save-excursion
        (forward-char)
        (insert "{{'")
        (forward-char)
        (while (nl/angularjs--current-quotes-char)
          (forward-char))
        (backward-char)
        (insert "'|translate}}"))
    (message "not at the start of a string")))

(defun nl/angularjs-html-mode-keys ()
  "Key definitions for 'web-mode' in bbweb project."
  (interactive)
  (local-set-key (kbd "C-c g") 'nl/angularjs-gettext-surround-html-string))

(add-hook 'web-mode-hook 'nl/angularjs-html-mode-keys)

(provide 'angularjs)
;;; angularjs.el ends here
