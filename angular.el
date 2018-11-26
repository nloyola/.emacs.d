;;; angular --- package to help with development in Angular projects

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:

(eval-and-compile
  (require 'projectile))

(defun ts-spec-filename-p (filename)
  "Return TRUE if FILENAME is a match for a TypseScript spec."
  (string-match "\.spec\.ts$" filename))

(projectile-register-project-type 'npm '("package.json")
                                  :compile "npm install"
                                  :test "npm test"
                                  :run "npm run start"
                                  :test-suffix ".spec")

(setq projectile-test-suffix-function (lambda (project-type) "" ".spec")
      projectile-find-dir-includes-top-level t)

;; functions for finding files in project
(defun nl-angular-find-with-filetypes (predicate)
  (let ((files (remove-if-not predicate (projectile-current-project-files))))
    (helm-comp-read "Find file: " files)))

(defun nl-angular-find-ts-file ()
  (interactive)
  (nl-angular-find-with-filetypes 'ts-filename-p))

(defun nl-angular-find-ts-spec-file ()
  (interactive)
  (nl-angular-find-with-filetypes 'ts-spec-filename-p))

(defun nl-angular-find-html-file ()
  (interactive)
  (nl-angular-find-with-filetypes 'html-filename-p))

(defhydra hydra-nl-angular-find-file (:color blue)
  "bbweb-find-file"
  ("t" nl-angular-find-ts-file "TypeScript file")
  ("s" nl-angular-find-ts-spec-file "TypeScript spec file")
  ("h" nl-angular-find-html-file "HTML file"))

(key-chord-define-global "jc" '(lambda () (interactive) (hydra-nl-angular-find-file/body)))

(provide 'angular)
;;; angular.el ends here
