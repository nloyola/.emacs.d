;;; con-project --- package to help with development of the con project

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:

(require 'php-mode)
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

(defun nl/counsel-ag-php ()
  "Perform counsel-ag on the project's PHP files excluding spec files."
  (interactive)
  (counsel-ag "" (projectile-project-root) "-G '((?!Test)).php$'"))

(defun nl/counsel-ag-php-test ()
  "Perform counsel-ag on the project's TypeScript spec files."
  (interactive)
  (counsel-ag "" (projectile-project-root) "-G Test.php$"))

(defhydra hydra-nl/php-search (:color blue)
  ("p" nl/counsel-ag-php "PHP files" :column "Search")
  ("s" nl/counsel-ag-php-test "PHP test specificaton files"))

(defhydra hydra-nl-con-project (:color red :hint nil)
  "PHP project commands"
  ("a" hydra-nl-align/body "align" :color blue :column "PHP")
  ("s" hydra-nl/php-search/body "search" :color blue))

;; this def uses a lambda to show that it is possible, id does not need to use it
(key-chord-define-global "jc" '(lambda () (interactive) (hydra-nl-con-project/body)))

(define-key php-mode-map (kbd "C-c , d") 'nl/phpunit-test-this-package)

(provide 'nl-php-project)
;;; nl-php-project.el ends here
