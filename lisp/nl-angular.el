;;; angular --- package to help with development in Angular projects

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:
(require 'web-mode)

(eval-when-compile
  (require 'seq)
  (require 'projectile)
  (require 'tide))

(defun angular-project-source-file-p (filename)
  "Return non-nil if FILENAME is for a TypeScript file."
  (string-match "\\.\\(html\\|ts\\)$" filename))

(defun angular-component-filename-p (filename)
  "Return non-nil if FILENAME is for a TypeScript file."
  (string-match "\\.component\\." filename))

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
  (if (buffer-file-name)
      (nl/command-in-proj-root (format "node --inspect node_modules/.bin/jest -e --runInBand %s"
                                       (buffer-file-name)))
    (message "not in a buffer visiting a file")))

(defun nl/jest-test-only-this-directory ()
  "Run the jest test suite only for all files in the directory this file is in."
  (interactive)
  (if (buffer-file-name)
      (nl/command-in-proj-root (format "node --inspect node_modules/.bin/jest --runInBand %s"
                                       (file-name-directory buffer-file-name)))
    (message "not in a buffer visiting a file")))

(defun nl/jest-test-and-coverage-only-this-file ()
  "Run the jest test suite only for this file."
  (interactive)
  (if (buffer-file-name)
      (nl/command-in-proj-root (format "node --inspect node_modules/.bin/jest --coverage --runInBand %s"
                                       (buffer-file-name)))
    (message "not in a buffer visiting a file")))

(defun nl/jest-test-and-coverage-only-this-directory ()
  "Run the jest test suite only for all files in the directory this file is in."
  (interactive)
  (if (buffer-file-name)
      (nl/command-in-proj-root (format "node --inspect node_modules/.bin/jest --coverage --runInBand %s"
                                       (file-name-directory buffer-file-name)))
    (message "not in a buffer visiting a file")))

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

(defun nl/counsel-ag-html ()
  "Perform counsel-ag on the project's HTML files."
  (interactive)
  (counsel-ag "" (projectile-project-root) "-G .html$"))

(defun ts-spec-filename-p (filename)
  "Return TRUE if FILENAME is a match for a TypseScript spec."
  (string-match "\.spec\.ts$" filename))

(defun nl/compile-command-in-proj-root (command)
  "Run the compile COMMAND at the project's root directory."
  (interactive)
  (compile (format "cd %s && %s" (projectile-project-root) command)))

(defun nl/ng-compile ()
  "Uses Angular CLI to build the project."
  (interactive)
  (nl/compile-command-in-proj-root "ng build"))

(defun nl/ng-test ()
  "Use Angular CLI to run the project tests."
  (interactive)
  (nl/compile-command-in-proj-root "npm run test"))

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
  (let ((files (seq-filter predicate (projectile-current-project-files))))
    (find-file (helm-comp-read "Find file: " files))))

(defun nl/angular-find-ts-file ()
  "Find TypeScript files in the project."
  (interactive)
  (nl/angular-find-with-filetypes #'ts-filename-p))

(defun nl/angular-find-ts-spec-file ()
  "Find TypeScript test file in the project."
  (interactive)
  (nl/angular-find-with-filetypes 'ts-spec-filename-p))

(defun nl/angular-find-html-file ()
  "Find HTML file in the project."
  (interactive)
  (nl/angular-find-with-filetypes 'html-filename-p))

(defun nl/angular-component-base-name ()
  "Return the component's base name for the file that the buffer is visiting."
  (interactive)
  (if (buffer-file-name)
      (let ((file-name (file-name-nondirectory (buffer-file-name))))
        (if (angular-component-filename-p file-name)
            (file-name-base (replace-regexp-in-string "\.spec" "" file-name))
          (progn
            (message "not an angular component file")
            nil)))
    (progn
      (message "not a file buffer")
      nil)))

(defun nl/angular-component-jump-to-file (ext)
  "Switch to buffer visiting angular component file with extension EXT."
  (let ((component-base-name (nl/angular-component-base-name)))
    (if component-base-name
        (find-file (concat component-base-name ext)))))

(defhydra hydra-nl/angular-jump-to-file (:color red :hint nil)
  ("c" (nl/angular-component-jump-to-file ".ts") "jump to component" :color blue :column "Angular")
  ("t" (nl/angular-component-jump-to-file ".spec.ts") "jump to test" :color blue)
  ("h" (nl/angular-component-jump-to-file ".html") "jump to HTML" :color blue)
  ("s" (nl/angular-component-jump-to-file ".scss") "jump to SCSS" :color blue))

(defhydra hydra-nl/angular-compile (:exit t)
  ("p" nl/ng-compile "project" :column "Compile")
  ("t" nl/typescript-compile-this-file "this file"))

(defhydra hydra-nl/angular-test (:color blue)
  "Test"
  ("f" nl/jest-test-only-this-file "only this file" :column "Test")
  ("d" nl/jest-test-only-this-directory "only this directory" :column "Test")
  ("p" nl/ng-test "project" :column "Test")
  ("e" nl/jest-failure-find-file "find failure" :column "Test")
  ("F" nl/jest-test-and-coverage-only-this-file "only this file" :column "Coverage")
  ("D" nl/jest-test-and-coverage-only-this-directory "only this directory")
  ("P" nl/jest-test-coverage "project")
  ("g" coverlay-toggle-overlays "toggle overlay" :column "Coverage")
  ("l" (coverlay-load-file (concat (projectile-project-root) "coverage/lcov.info")) "load data")
  ("s" coverlay-display-stats "display stats"))

(defhydra hydra-nl/angular-search (:color blue)
  ("f" nl/counsel-ag-ts "TypeScript files" :column "Search")
  ("s" nl/counsel-ag-ts-spec "TypeScript test specificaton files")
  ("h" nl/counsel-ag-html "HTML files"))

(defhydra hydra-nl/angular-project (:color red :hint nil)
  ("j" hydra-nl/angular-jump-to-file/body "Jump to file" :color blue :column "Angular")
  ("c" hydra-nl/angular-compile/body "TypeScript compile" :color blue)
  ("a" hydra-nl-align/body "align" :color blue)
  ("i" nl/indent-whole-buffer "indent buffer" :color blue)
  ("s" hydra-nl/angular-search/body "search" :color blue)
  ("t" hydra-nl/angular-test/body "test" :color blue))

(key-chord-define typescript-mode-map "jc" '(lambda () (interactive) (hydra-nl/angular-project/body)))
(key-chord-define web-mode-map "jc" '(lambda () (interactive) (hydra-nl/angular-project/body)))

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

(define-key typescript-mode-map (kbd "C-c C-t c") 'nl/typescript-compile-this-file)
(define-key typescript-mode-map (kbd "C-c C-t s") 'nl/counsel-ag-ts-spec)

(define-key typescript-mode-map (kbd "C-c , f") 'nl/jest-test-only-this-file)
(define-key typescript-mode-map (kbd "C-c , F") 'nl/jest-test-and-coverage-only-this-file)
(define-key typescript-mode-map (kbd "C-c , d") 'nl/jest-test-only-this-directory)
(define-key typescript-mode-map (kbd "C-c , p") 'nl/ng-test)
(define-key typescript-mode-map (kbd "C-c , P") 'nl/jest-test-coverage)

(define-key typescript-mode-map (kbd "C-x '") 'recompile)

(defun nl/prettier-on-project-files ()
  (interactive)
  (let ((files (seq-filter 'angular-project-source-file-p (projectile-current-project-files))))
    (loop for file in files do
          (progn
            (setq default-directory (projectile-project-root))
            (message "formatting: %s" file)
            (find-file file)
            (goto-char (point-min))
            (prettier-js)
            (save-buffer)))))

(provide 'nl-angular)
;;; nl-angular.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
