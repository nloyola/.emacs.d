;;; bbweb-project --- package to help with development of the bbweb project

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
(puthash (projectile-project-root) "grunt --no-color karma:coverage" projectile-test-cmd-map)

;;
;; override this function, from the projectile package, so that tests are created in the proper
;; location for this project
;;
(defun projectile-create-test-file-for (impl-file-path)
  "Create a test file for the file given by IMPL-FILE-PATH."
  (let* ((test-file (projectile--test-name-for-impl-name impl-file-path))
         (test-dir (replace-regexp-in-string "app/" "test/" (file-name-directory impl-file-path))))
    (unless (file-exists-p (expand-file-name test-file test-dir))
      (progn (unless (file-exists-p test-dir)
               (make-directory test-dir :create-parents))
             (concat test-dir test-file)))))

(defhydra hydra-nl-bbweb-project (:hint nil)
  "bbweb project build commands"
  ("t" (lambda () (interactive) (sbt-command "test:compile")) "sbt test:compile" :color blue)
  ("s" sbt-command "sbt command" :color blue)
  ("k" karma-start "karma unit test" :color blue)
  ("c" (lambda () (interactive) (helm-projectile-test-project (projectile-project-root))) "karma coverage" :color blue))

(global-set-key [f5] '(lambda () (interactive)
                        (hydra-nl-bbweb-project/body)))

(provide 'bbweb-project)
;;; bbweb-project.el ends here
