;;; con-project --- package to help with development of the con project

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:

(eval-and-compile
  (require 'projectile))

(setq projectile-test-suffix-function (lambda (project-type) "" "Spec")
      projectile-find-dir-includes-top-level t)

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

(defun nl/phpunit-test-this-package ()
  "For the class the cursor is in, run the scalatest test suite.
The class name must have the postfix 'Spec' for this function to work."
  (interactive)
  (let ((args (s-concat (file-name-directory (buffer-file-name)))))
    (phpunit-run args)))

(defhydra hydra-nl-con-project (:hint nil)
  "con project commands"
  ("p" (lambda () (interactive) (helm-projectile-test-project (projectile-project-root))) "test project" :color blue)
  ("x" xref-find-definitions "find definition" :color blue))

;; this def uses a lambda to show that it is possible, id does not need to use it
(key-chord-define-global "jc" '(lambda () (interactive)
                                 (hydra-nl-con-project/body)))

;; Before running phpunit, remove the logs/test.log file
(defun nl/con-project-remove-test-log(orig-function args)
  (let ((old-command (funcall orig-function args)))
    ;;(message "---------> %s" old-command)
    (concat "rm -rf logs/test.log && " (funcall orig-function args))))

(defun nl/con-project-php-mode-hook ()
  ;;(advice-remove 'phpunit-get-compile-command 'nl/con-project-remove-test-log)
  (advice-add 'phpunit-get-compile-command :around #'nl/con-project-remove-test-log))

;; Before running phpunit, remove the logs/test.log file
(add-hook 'php-mode-hook 'nl/con-project-php-mode-hook)

(define-key php-mode-map (kbd "C-c , d") 'nl/phpunit-test-this-package)


(provide 'con-project)
;;; con-project.el ends here
