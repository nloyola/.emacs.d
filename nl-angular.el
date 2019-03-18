;;; angular --- package to help with development in Angular projects

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:

(eval-and-compile
  (require 'projectile))

(defun ts-filename-p (filename)
  "Return non-nil if FILENAME is for a TypeScript file."
  (string-match "\.ts$" filename))

(defun nl/command-in-proj-root (command)
  "Run the compile COMMAND at the project's root directory."
  (interactive)
  (compile (format "cd %s && %s" (projectile-project-root) command)))

(defun nl/typescrip-compile-this-file ()
  "Run the jest test suite only for this file."
  (interactive)
  (nl/command-in-proj-root (format "node_modules/.bin/tsc --noEmit %s" (buffer-file-name))))

(defun nl/jest-test-only-this-file ()
  "Run the jest test suite only for this file."
  (interactive)
  (nl/command-in-proj-root (format "node --inspect node_modules/.bin/jest --runInBand %s"
                                   (buffer-file-name))))

(defun nl/jest-test-and-coverage-only-this-file ()
  "Run the jest test suite only for this file."
  (interactive)
  (nl/command-in-proj-root (format "node --inspect node_modules/.bin/jest --coverage --runInBand %s"
                                   (buffer-file-name))))

(defun nl/jest-test-coverage ()
  "Run the jest test suite with code coverage."
  (interactive)
  (nl/command-in-proj-root (format "npm run test:cov")))

(defun nl/counsel-ag-ts ()
  "Perform counsel-ag on the project's TypeScript files excluding spec files."
  (interactive)
  (counsel-ag "" (projectile-project-root) "-G '((?!spec)).ts$'"))

(defun nl/counsel-ag-ts-spec ()
  "Perform counsel-ag on the project's TypeScript spec files."
  (interactive)
  (counsel-ag "" (projectile-project-root) "-G spec.ts$"))

(defun ts-spec-filename-p (filename)
  "Return TRUE if FILENAME is a match for a TypseScript spec."
  (string-match "\.spec\.ts$" filename))

(projectile-register-project-type 'ts '("package.json")
                                  :compile "ng build"
                                  :test "npm run test"
                                  :run "npm run start"
                                  :test-suffix ".spec")

(setq projectile-test-suffix-function (lambda (project-type) "" ".spec")
      projectile-find-dir-includes-top-level t)

;; functions for finding files in project
(defun nl/angular-find-with-filetypes (predicate)
  "Find project files given a file name PREDICATE."
  (let ((files (remove-if-not predicate (projectile-current-project-files))))
    (helm-comp-read "Find file: " files)))

(defun nl/angular-find-ts-file ()
  "Finds a TypeScript file in the project."
  (interactive)
  (nl/angular-find-with-filetypes 'ts-filename-p))

(defun nl/angular-find-ts-spec-file ()
  "Finds a TypeScript test file in the project."
  (interactive)
  (nl/angular-find-with-filetypes 'ts-spec-filename-p))

(defun nl/angular-find-html-file ()
  "Finds an HTML file in the project."
  (interactive)
  (nl/angular-find-with-filetypes 'html-filename-p))

(defhydra hydra-nl/angular-find-file (:color blue)
  "angular project find file"
  ("t" nl/angular-find-ts-file "TypeScript file")
  ("s" nl/angular-find-ts-spec-file "TypeScript spec file")
  ("h" nl/angular-find-html-file "HTML file"))

(key-chord-define-global "jc" '(lambda () (interactive) (hydra-nl/angular-find-file/body)))

(defun nl/jest-failure-find-file ()
  "From a Jest backtrace, opens the file under the cursor at the line specified."
  (interactive)
  (let (filename)
    (save-some-buffers t)
    (save-match-data
      (and (re-search-forward (rx line-start
                                  (one-or-more (syntax whitespace))
                                  "FAIL"
                                  (one-or-more (syntax whitespace))
                                  (group (+ (not (syntax whitespace)))))
                              nil t)
           (setq filename (match-string-no-properties 1))))
    (when filename
      (recenter-top-bottom 'top)
      (ace-select-window)
      (find-file (expand-file-name filename (projectile-project-root)))
      (goto-char (point-min)))))

(provide 'nl-angular)
;;; nl-angular.el ends here
