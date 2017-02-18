;;; con-project --- package to help with development of the con project

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:

(eval-and-compile
  (require 'projectile)
  (require 'eclimd))

(setq projectile-test-suffix-function (lambda (project-type) "" "Spec")
      projectile-find-dir-includes-top-level t
      eclimd-default-workspace (concat (locate-dominating-file default-directory ".dir-locals.el") ".."))

(setq-default indent-tabs-mode nil)
(puthash (projectile-project-root) "grunt karma:unit" projectile-test-cmd-map)

;;
;; override this function, from the projectile package, so that tests are created in the proper
;; location for this project
;;
(defun projectile-create-test-file-for (impl-file-path)
  "Create a test file for the file given by IMPL-FILE-PATH."
  (let* ((test-file (projectile--test-name-for-impl-name impl-file-path))
         (test-dir (replace-regexp-in-string "public/" "test/" (file-name-directory impl-file-path))))
    (unless (file-exists-p (expand-file-name test-file test-dir))
      (progn (unless (file-exists-p test-dir)
               (make-directory test-dir :create-parents))
             (concat test-dir test-file)))))

(defun js-filename-p (filename)
  (string-match "\.js$" filename))

(defun html-filename-p (filename)
  (string-match "\.html$" filename))

(defhydra hydra-nl-con-project (:hint nil)
  "con project commands"
  ("m" karma-mode "toggle karma-mode" :color blue)
  ("k" karma-start "karma unit test" :color blue)
  ("p" (lambda () (interactive) (helm-projectile-test-project (projectile-project-root))) "test project" :color blue)
  ("x" xref-find-definitions "find definition" :color blue))

;; this def uses a lambda to show that it is possible, id does not need to use it
(key-chord-define-global "jc" '(lambda () (interactive)
                                 (hydra-nl-con-project/body)))

;; borrowed from js2-refactor
(defun con--current-quotes-char ()
  "The char that is the current quote delimiter."
  (nth 3 (syntax-ppss)))

(defun con-gettext-surround-string ()
  "Surrounds string with a call to gettext.  Cursor must be at string's start."
  (interactive)
  (if (looking-at "'")
      (save-excursion
        (insert "gettext(")
        (forward-char)
        (while (con--current-quotes-char)
          (forward-char))
        (insert ")"))
    (message "not at the start of a string")))

(defun con-js-mode-keys ()
  "Key definitions for 'js-mode' in con project."
  (interactive)
  (local-set-key (kbd "C-c g") 'con-gettext-surround-string))

(add-hook 'js-mode-hook 'con-js-mode-keys)

(defun con-gettext-surround-html-string ()
  "Surrounds string with a call to gettext.  Cursor must be at string's start."
  (interactive)
  (if (looking-at "\"")
      (save-excursion
        (forward-char)
        (insert "{{'")
        (forward-char)
        (while (con--current-quotes-char)
          (forward-char))
        (backward-char)
        (insert "'|translate}}"))
    (message "not at the start of a string")))

(defun con-html-mode-keys ()
  "Key definitions for 'html-mode' in con project."
  (interactive)
  (local-set-key (kbd "C-c g") 'con-gettext-surround-html-string))

(add-hook 'html-mode-hook 'con-html-mode-keys)

(provide 'con-project)
;;; con-project.el ends here
