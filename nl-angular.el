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

(defun nl/typescript-compile-this-file ()
  "Run the jest test suite only for this file."
  (interactive)
  (nl/command-in-proj-root (format "node_modules/.bin/tsc --noEmit %s" (buffer-file-name))))

(defun nl/jest-test-only-this-file ()
  "Run the jest test suite only for this file."
  (interactive)
  (nl/command-in-proj-root (format "node --inspect node_modules/.bin/jest --runInBand %s"
                                   (buffer-file-name))))

(defun nl/jest-test-only-this-directory ()
  "Run the jest test suite only for all files in the directory this file is in."
  (interactive)
  (nl/command-in-proj-root (format "node --inspect node_modules/.bin/jest --runInBand %s"
                                   (file-name-directory buffer-file-name))))

(defun nl/jest-test-and-coverage-only-this-file ()
  "Run the jest test suite only for this file."
  (interactive)
  (nl/command-in-proj-root (format "node --inspect node_modules/.bin/jest --coverage --runInBand %s"
                                   (buffer-file-name))))

(defun nl/jest-test-and-coverage-only-this-directory ()
  "Run the jest test suite only for all files in the directory this file is in."
  (interactive)
  (nl/command-in-proj-root (format "node --inspect node_modules/.bin/jest --coverage --runInBand %s"
                                   (file-name-directory buffer-file-name))))

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

(defun nl/ng-compile ()
    "Uses Angular CLI to build the project."
  (interactive)
  (with-output-to-temp-buffer "*ng-compile*"
    (async-shell-command "ng build" "*ng-compile*" "*Messages*")
    (pop-to-buffer "*ng-compile*"))
  "")

(defun nl/ng-test ()
    "Uses Angular CLI to run the project tests."
  (interactive)
  (with-output-to-temp-buffer "*ng-test*"
    (async-shell-command "npm run test" "*ng-test*" "*Messages*")
    (pop-to-buffer "*ng-test*"))
  "")

(defun nl/related-files (path)
  "Tell Projectile that implementation and test files are in the same directory (PATH)."
  (let ((dir (file-name-directory path))
        (base-name (file-name-nondirectory (file-name-sans-extension path))))
    (if (string-suffix-p ".spec" base-name)
        (let ((impl-name (file-name-sans-extension base-name)))
          (list :impl (concat dir impl-name ".ts")
                :other (concat dir impl-name ".html")
                :scss (concat dir impl-name ".scss")))
      (list :test (concat dir base-name ".spec.ts")
            :other (concat dir base-name ".html")
            :scss (concat dir base-name ".scss")))))

(projectile-register-project-type 'ts '("package.json")
                                  ;;:compile "ng build"   -- using projectile-project-compilation-cmd, see below
                                  ;;:test "npm run test"  -- using projectile-project-test-cmd, see below
                                  :run "npm run start"
                                  :test-suffix ".spec"
                                  :related-files-fn #'nl/related-files)

;;
;; override this function, from the projectile package, so that tests are created in the proper
;; location for this project
;;
(defun projectile-create-test-file-for (impl-file-path)
  "Create a test file for the file given by IMPL-FILE-PATH."
  (let* ((test-file (projectile--test-name-for-impl-name impl-file-path))
         (test-file-extension (file-name-extension impl-file-path))
         (test-dir))
    (cond
     ((string= test-file-extension "ts")
      (setq test-dir (file-name-directory impl-file-path))))
    (unless (file-exists-p (expand-file-name test-file test-dir))
      (progn (unless (file-exists-p test-dir)
               (make-directory test-dir :create-parents))
             (concat test-dir test-file)))))

(setq projectile-test-suffix-function (lambda (project-type) "" ".spec")
      projectile-find-dir-includes-top-level t
      projectile-project-compilation-cmd #'nl/ng-compile
      projectile-project-test-cmd #'nl/ng-test)

;; functions for finding files in project
(defun nl/angular-find-with-filetypes (predicate)
  "Find project files given a file name PREDICATE."
  (let ((files (remove-if-not predicate (projectile-current-project-files))))
    (helm-comp-read "Find file: " files)))

(defun nl/angular-find-ts-file ()
  "Find TypeScript files in the project."
  (interactive)
  (nl/angular-find-with-filetypes 'ts-filename-p))

(defun nl/angular-find-ts-spec-file ()
  "Find TypeScript test file in the project."
  (interactive)
  (nl/angular-find-with-filetypes 'ts-spec-filename-p))

(defun nl/angular-find-html-file ()
  "Find HTML file in the project."
  (interactive)
  (nl/angular-find-with-filetypes 'html-filename-p))

(defhydra hydra-nl/angular-compile (:color blue)
  "angular project test"
  ("t" nl/typescript-compile-this-file "compile this file"))

(defhydra hydra-nl/angular-test-coverage (:color blue)
  "angular project test"
  ("d" nl/jest-test-and-coverage-only-this-directory "test only this directory")
  ("t" nl/jest-test-and-coverage-only-this-file "test only this file"))

(defhydra hydra-nl/angular-test (:color blue)
  "angular project test"
  ("c" hydra-nl/angular-test-coverage/body "test coverage")
  ("d" nl/jest-test-only-this-directory "test only this directory")
  ("t" nl/jest-test-only-this-file "test only this file"))

(defhydra hydra-nl/angular-search (:color blue)
  "angular project search"
  ("s" nl/counsel-ag-ts-spec "search only test files"))

(defhydra hydra-nl/angular-find-file (:color blue)
  "angular project find file"
  ("t" nl/angular-find-ts-file "TypeScript file")
  ("s" nl/angular-find-ts-spec-file "TypeScript spec file")
  ("h" nl/angular-find-html-file "HTML file"))

(defhydra hydra-nl/angular-project (:hint nil)
  "angular project commands"
  ("c" hydra-nl/angular-compile/body "TypeScript compile" :color blue)
  ("i" nl/indent-whole-buffer "indent buffer" :color blue)
  ("f" hydra-nl/angular-find-file/body "find files" :color blue)
  ("s" hydra-nl/angular-search/body "search" :color blue)
  ("t" hydra-nl/angular-test/body "test" :color blue))

(key-chord-define-global "jc" '(lambda () (interactive) (hydra-nl/angular-project/body)))

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

(define-key tide-mode-map (kbd "C-c C-t c c") 'nl/typescript-compile-this-file)
(define-key tide-mode-map (kbd "C-c C-t c t") 'nl/jest-test-coverage)
(define-key tide-mode-map (kbd "C-c C-t i") 'nl/indent-whole-buffer)
(define-key tide-mode-map (kbd "C-c C-t s") 'nl/counsel-ag-ts-spec)
(define-key tide-mode-map (kbd "C-c C-t t t") 'nl/jest-test-only-this-file)
(define-key tide-mode-map (kbd "C-c C-t t d") 'nl/jest-test-only-this-directory)
(define-key tide-mode-map (kbd "C-c C-t t c t") 'nl/jest-test-and-coverage-only-this-file)
(define-key tide-mode-map (kbd "C-c C-t t c d") 'nl/jest-test-and-coverage-only-this-directory)

(provide 'nl-angular)
;;; nl-angular.el ends here
