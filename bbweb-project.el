;;; bbweb-project --- package to help with development of the bbweb project

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:

(eval-and-compile
  (require 'projectile))

(setq projectile-test-suffix-function (lambda (project-type) "" "Spec")
      projectile-find-dir-includes-top-level t)

(setq-default indent-tabs-mode nil)

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
    (find-file (helm-comp-read "Find file: " files))))

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

(defhydra hydra-nl-bbweb-scala (:hint nil)
  "bbweb scala build"
  ("d" (lambda () (interactive) (sbt-command "reload")) "sbt reload" :color blue :column "Scala")
  ("r" (lambda () (interactive) (sbt-command "run")) "sbt run" :color blue)
  ("t" (lambda () (interactive) (sbt-command "test:compile")) "sbt test:compile" :color blue)
  ("f" nl/scalatest-find-file "find file from traceback" :color blue :column "Scala Test")
  ("n" nl/scalatest-find-next-failure "find next scalatest failure in SBT buffer" :color blue)
  ("s" nl/scalatest-test-only "select suite to run" :color blue))

;; this def uses a lambda to show that it is possible, id does not need to use it
(key-chord-define-global "jc" '(lambda () (interactive)
                                 (hydra-nl-bbweb-scala/body)))

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
