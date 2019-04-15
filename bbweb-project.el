;;; bbweb-project --- package to help with development of the bbweb project

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
         (test-file-extension (file-name-extension impl-file-path))
         (test-dir))
    (cond
     ((string= test-file-extension "js")
      (setq test-dir (file-name-directory impl-file-path)))
     ((string= test-file-extension "scala")
      (setq test-dir (replace-regexp-in-string "app/" "test/" (file-name-directory impl-file-path)))))
    (unless (file-exists-p (expand-file-name test-file test-dir))
      (progn (unless (file-exists-p test-dir)
               (make-directory test-dir :create-parents))
             (concat test-dir test-file)))))

;; functions for finding files in project
(defun bbweb-find-with-filetypes (predicate)
  (let ((files (remove-if-not predicate (projectile-current-project-files))))
    (helm-comp-read "Find file: " files)))

(defun bbweb-find-scala-file ()
  (interactive)
  (bbweb-find-with-filetypes 'scala-filename-p))

(defun bbweb-find-js-file ()
  (interactive)
  (bbweb-find-with-filetypes 'js-filename-p))

(defun bbweb-find-html-file ()
  (interactive)
  (bbweb-find-with-filetypes 'html-filename-p))

(defhydra hydra-nl-bbweb-find-file (:color blue)
  "bbweb-find-file"
  ("s" bbweb-find-scala-file "Scala file")
  ("j" bbweb-find-js-file "JS file")
  ("h" bbweb-find-html-file "HTML file"))

;; JavaScript hydras

(defhydra hydra-nl-bbweb-js-test (:color red)
  "bbweb js test"
  ("t" (lambda ()
         (interactive)
         (compile (concat "cd " (projectile-project-root) " && npm run test-watch"))) "test-watch"))

(defhydra hydra-nl-bbweb-js-build (:color red)
  "bbweb js build"
  ("d" (lambda ()
         (interactive)
         (compile (concat "cd " (projectile-project-root) " && npm run dev-build"))) "dev-build")
  ("w" nl/webpack-find-next-error "webpack find next error and find file"))

(defhydra hydra-nl-bbweb-js (:color blue)
  "bbweb js test"
  ("b" hydra-nl-bbweb-js-build/body "Build" :exit t)
  ("t" hydra-nl-bbweb-js-test/body "Test" :exit t))

;; Scala hydras

(defhydra hydra-nl-bbweb-scala-build (:color red)
  "bbweb scala build"
  ("d" (lambda () (interactive) (sbt-command "reload")) "sbt reload")
  ("r" (lambda () (interactive) (sbt-command "run")) "sbt run")
  ("t" (lambda () (interactive) (sbt-command "test:compile")) "sbt test:compile"))

(defhydra hydra-nl-bbweb-scala (:color blue)
  "bbweb scala test"
  ("b" hydra-nl-bbweb-scala-build/body "Build" :exit t)
  ("t" hydra-nl-scalatest/body "Test" :exit t))

;; npm hydras

(defhydra hydra-nl-bbweb-npm (:color blue)
  "bbweb npm"
  ("c" (lambda () (interactive)
         (async-shell-command
          (concat "cd " (projectile-project-root) " && npm run test-coverage")
          "*bbweb test coverage*"))
   "test coverage" :exit t)
  ("d" (lambda () (interactive)
         (compile (concat "cd " (projectile-project-root) " && npm run dev")))
   "dev" :exit t)
  ("s" (lambda () (interactive)
         (setq async-shell-command-buffer 'confirm-kill-process)
         (async-shell-command (concat "cd " (projectile-project-root) " && npm run dev-server") "*bbweb server*"))
   "dev-server" :exit t))

(defhydra hydra-nl-bbweb-project (:hint nil)
  "bbweb project commands"
  ("j" hydra-nl-bbweb-js/body "javascript" :color blue)
  ("n" hydra-nl-bbweb-npm/body "npm" :color blue)
  ("s" hydra-nl-bbweb-scala/body "scala" :color blue)
  ("x" xref-find-definitions "find definition" :color blue))

;; this def uses a lambda to show that it is possible, id does not need to use it
(key-chord-define-global "jc" '(lambda () (interactive)
                                 (hydra-nl-bbweb-project/body)))

;;
;;
(defun nl-karma-compile-filter ()
  "Filters unwanted lines from karma compile buffer."
  (interactive)
  (if (get-buffer "*karma start*")
      (progn
        (switch-to-buffer "*karma start*")
        (read-only-mode -1)2
        (delete-matching-lines "\\bbweb\/(node_modules\\|target\\)" (point-min) (point-max))
        (read-only-mode 1))))

;;
;;
(defun nl-sbt-buffer-filter ()
  "Filters unwanted lines from karma compile buffer."
  (interactive)
  (if (get-buffer "*sbt*")
      (progn
        (switch-to-buffer "*sbt*")
        (read-only-mode -1)
        (delete-matching-lines "\\bbweb\/(node_modules\\|target\\)" (point-min) (point-max))
        (read-only-mode 1))))


(provide 'bbweb-project)
;;; bbweb-project.el ends here
