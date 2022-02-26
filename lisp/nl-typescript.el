;;; angular --- package to help with development in Angular projects

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:
(require 'web-mode)

(eval-when-compile
  (require 'seq)
  (require 'projectile)
  (require 'typescript-mode)
  )

(defun typescript-project-source-file-p (filename)
  "Return non-nil if FILENAME is for a TypeScript file."
  (string-match "\\.\\(html\\|ts\\)$" filename))

(defun ts-filename-p (filename)
  "Return non-nil if FILENAME is for a TypeScript file."
  (string-match "\.ts$" filename))

(defun nl/command-in-typescript-proj-root (command)
  "Run the compile COMMAND at the project's root directory."
  (interactive)
  (compile (format "cd %s && %s" (projectile-project-root) command)))

;; (defun nl/jest-test-only-this-file ()
;;   "Run the jest test suite only for this file."
;;   (interactive)
;;   (if (buffer-file-name)
;;       (nl/command-in-typescript-proj-root (format "node --inspect node_modules/.bin/jest -e --runInBand %s"
;;                                        (buffer-file-name)))
;;     (message "not in a buffer visiting a file")))

(defun nl/jest-test-only-this-directory ()
  "Run the jest test suite only for all files in the directory this file is in."
  (interactive)
  (if (buffer-file-name)
      (nl/command-in-typescript-proj-root (format "node --inspect node_modules/.bin/jest --runInBand %s"
                                       (file-name-directory buffer-file-name)))
    (message "not in a buffer visiting a file")))

(defun nl/jest-test-and-coverage-only-this-file ()
  "Run the jest test suite only for this file."
  (interactive)
  (if (buffer-file-name)
      (nl/command-in-typescript-proj-root (format "node --inspect node_modules/.bin/jest --coverage --runInBand %s"
                                       (buffer-file-name)))
    (message "not in a buffer visiting a file")))

(defun nl/jest-test-and-coverage-only-this-directory ()
  "Run the jest test suite only for all files in the directory this file is in."
  (interactive)
  (if (buffer-file-name)
      (nl/command-in-typescript-proj-root (format "node --inspect node_modules/.bin/jest --coverage --runInBand %s"
                                       (file-name-directory buffer-file-name)))
    (message "not in a buffer visiting a file")))

(defun nl/jest-test-coverage ()
  "Run the jest test suite with code coverage."
  (interactive)
  (nl/command-in-typescript-proj-root (format "npm run test:cov")))

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

(defun nl/counsel-ag-css ()
  "Perform counsel-ag on the project's CSS files."
  (interactive)
  (counsel-ag "" (projectile-project-root) "-G .s\\?css$"))

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

(projectile-register-project-type 'ts '("package.json")
                                  ;;:compile "ng build"   -- using projectile-project-compilation-cmd, see below
                                  ;;:test "npm run test"  -- using projectile-project-test-cmd, see below
                                  :run "npm run start"
                                  :test-suffix ".spec"
                                  :related-files-fn #'nl/related-files)

(defhydra hydra-nl/typescript-project (:color red :hint nil)
  ("a" hydra-nl-align/body "align" :color blue)
  ("i" nl/indent-whole-buffer "indent buffer" :color blue))

(key-chord-define typescript-mode-map "jc" '(lambda () (interactive) (hydra-nl/typescript-project/body)))
(key-chord-define web-mode-map "jc" '(lambda () (interactive) (hydra-nl/typescript-project/body)))

(define-key typescript-mode-map (kbd "C-c C-t c") 'nl/typescript-compile-this-file)

(provide 'nl-typescript)
;;; nl-angular.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
