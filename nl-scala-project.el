;;; nl-scala-project --- package to help with development in Scala projects

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'sbt-mode))

(defvar scala-mode-map)

(defconst scalatest-class-package-regexp
  (rx line-start
      (* (syntax whitespace))
      "package"
      (+ (syntax whitespace))
      (group (+ (or (syntax word) (syntax symbol) (syntax punctuation))))
      (* (syntax whitespace)))
  "Regular expression that returns the package a scala test suite is in.")

(defconst scalatest-beginning-of-class-regexp
  (rx line-start
      (* (syntax whitespace))
      "class"
      (+ (syntax whitespace))
      (group (+ (or (syntax word) (syntax symbol))))
      (* (syntax whitespace)))
  "Regular expression for a scalatest suite.")

(defconst scalatest-beginning-of-behaviour-regexp
  (eval-when-compile
    (rx line-start
        (one-or-more (syntax whitespace))
        (| "it" "describe") (syntax open-parenthesis) (syntax string-quote)
        (group (+ (not (syntax string-quote))))
        (syntax string-quote)
        ;; account for optional tag
        (group (? (not (syntax close-parenthesis))))
        (syntax close-parenthesis)))
  "Regular expression for a scalatest FunSepc behaviour.")

(defun scala-filename-p (filename)
  (string-match "\.scala$" filename))

;; created after reading this article:
;;
;; http://kitchingroup.cheme.cmu.edu/blog/2015/01/24/Anatomy-of-a-helm-source/
(setq nl/helm-scala-project-test-spec
      '((name . "Select a ScalaTest specification to run:")
        (candidates
         . (lambda ()
             (remove-if-not (lambda (filename)
                              (string-match "Spec.scala$" filename))
                            (projectile-current-project-files))
             ))
        (nomark . t)
        (action
         . (lambda (candidate)
             ;;(message "file name is: %s" candidate)
             ;;(message "test name is: %s" (file-name-nondirectory candidate))
             (sbt-command (message "testOnly *.%s"
                                   (file-name-sans-extension (file-name-nondirectory candidate))))
             ))))

(defun nl/scalatest-test-only ()
  (interactive)
  (helm :sources '(nl/helm-scala-project-test-spec)))

(defun sbt-hydra:run-sbt-command (command)
  (interactive)
  (sbt-switch-to-active-sbt-buffer)
  (sbt-hydra:add-to-history command)
  (sbt-hydra:run-previous-sbt-command))

(defun nl/scalstest-find-suite-package-name ()
  "Determines the name of the package the ScalaTest suite is in."
  (save-excursion
    (when (re-search-backward scalatest-class-package-regexp nil t)
      (match-string-no-properties 1))))

(defun nl/scalstest-find-suite-class-name ()
  "Determines the name of the ScalaTest suite name."
  (save-excursion
    (when (re-search-backward scalatest-beginning-of-class-regexp nil t)
      (match-string-no-properties 1))))

(defun nl/scalatest-test-this-package ()
  "For the class the cursor is in, run the scalatest test suite.
The class name must have the postfix 'Spec' for this function to work."
  (interactive)
  (sbt-command (format "testOnly %s.*" (nl/scalstest-find-suite-package-name))))

(defun nl/scalatest-test-project ()
  "For the class the cursor is in, run the scalatest test suite.
The class name must have the postfix 'Spec' for this function to work."
  (interactive)
  (sbt-command (format "test")))

(defun nl/scalatest-test-only-this-class ()
  "For the class the cursor is in, run the scalatest test suite.
The class name must have the postfix 'Spec' for this function to work."
  (interactive)
  (sbt-command (format "testOnly *.%s" (nl/scalstest-find-suite-class-name))))

(defun nl/scalatest-test-only-this-buffer-with-substring-tag ()
  "For the class in the current buffer, run the scalatest test suite.
The class name must have the postfix 'Spec' for this function to work."
  (interactive)
  ;;(message (format "testOnly *.%s -- -z \"%s\"" (nl/scalstest-find-suite-name) (nl/scalstest-find-scalatest-behaviour)))
  (sbt-command (format "testOnly *.%s -- -z \"%s\"" (nl/scalstest-find-suite-class-name) (nl/scalstest-find-scalatest-behaviour))))

(defun nl/scalatest-find-file ()
  "From a scalatest failure backtrace, opens the file under the cursor at the line specified."
  (interactive)
  (let (p1 p2 err-line filename line-num file-with-proj-path)
    (save-some-buffers t)
    (setq p1 (line-beginning-position) )
    (setq p2 (line-end-position) )
    (setq err-line (buffer-substring-no-properties p1 p2))
    (save-match-data ; is usually a good idea
      (and (string-match "\\[info\\]\\s-+at\\s-+org.biobank[^(]+(\\([^:]+\\):\\([0-9]+\\))" err-line)
           (setq filename (concat "/" (match-string 1 err-line)) ; add leading slash as a delimiter
                 line-num (string-to-number (match-string 2 err-line))
                 )))
    (setq file-with-proj-path
          (car (remove-if-not
                (lambda (projfile)
                  (string-match filename projfile))
                (projectile-current-project-files))))
    (message "%s %s" file-with-proj-path line-num)
    (ace-select-window)
    (find-file (expand-file-name file-with-proj-path (projectile-project-root)))
    (goto-char (point-min))
    (forward-line (- line-num 1))))

(defun nl/scalatest-find-next-failure ()
  "Find the next failed test in an SBT buffer that just ran scalatests."
  (interactive)
  (sbt-switch-to-active-sbt-buffer)
  (search-forward "*** FAILED ***")
  (move-end-of-line nil)
  (recenter))

(defhydra hydra-nl-scalatest (:hint nil)
  "Testing scala code with scalatest"
  ("f" nl/scalatest-find-file "find file from traceback" :color blue)
  ("n" nl/scalatest-find-next-failure "find next scalatest failure in SBT buffer" :color blue)
  ("s" nl/scalatest-test-only "select suite to run" :color blue))

(defun scala-mode-newline-comments ()
  "Custom newline appropriate for `scala-mode'."
  ;; shouldn't this be in a post-insert hook?
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

(defun nl/scala-mode-keys ()
  "My extra key definitions for 'scala-mode'."
  (interactive)
  ;; use sbt-run-previous-command to re-compile your code after changes
  (local-set-key (kbd "C-x '") 'sbt-run-previous-command)
  (local-set-key (kbd "C-c , t") 'nl/scalatest-test-only-this-buffer-with-substring-tag)
  (local-set-key (kbd "C-c , c") 'nl/scalatest-test-only-this-class)
  (local-set-key (kbd "C-c , d") 'nl/scalatest-test-this-package)
  (local-set-key (kbd "C-c , p") 'nl/scalatest-test-project)
  (local-set-key (kbd "RET") 'newline-and-indent)
  ;; Bind the backtab (shift tab) to
  ;; 'scala-indent:indent-with-reluctant-strategy command. This is usefull
  ;; when using the 'eager' mode by default and you want to "outdent" a
  ;; code line as a new statement.
  (local-set-key (kbd "<backtab>") 'scala-indent:indent-with-reluctant-strategy)
  (bind-key "M-j" 'scala-mode-newline-comments scala-mode-map))

(defun nl/scala-project-mode-hook ()
  (nl/scala-mode-keys))

(add-hook 'scala-mode-hook 'nl/scala-project-mode-hook)

(provide 'nl-scala-project)
;;; bbweb-project.el ends here
