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
(puthash (projectile-project-root) "grunt karma:unit" projectile-test-cmd-map)

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

;; functions for finding files in project
(defun scala-filename-p (filename)
  (string-match "\.scala$" filename))

(defun js-filename-p (filename)
  (string-match "\.js$" filename))

(defun html-filename-p (filename)
  (string-match "\.html$" filename))

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

(defhydra hydra-nl-bbweb-project (:hint nil)
  "bbweb project commands"
  ("f" hydra-nl-bbweb-find-file/body "Find file" :exit t)
  ("d" (lambda () (interactive) (sbt-command "reload")) "sbt reload" :color blue)
  ("r" (lambda () (interactive) (sbt-command "run")) "run" :color blue)
  ("s" sbt-command "sbt command" :color blue)
  ("t" (lambda () (interactive) (sbt-command "test:compile")) "sbt test:compile" :color blue)
  ("m" karma-mode "toggle karma-mode" :color blue)
  ("k" karma-start "karma unit test" :color blue)
  ("p" (lambda () (interactive) (helm-projectile-test-project (projectile-project-root))) "test project" :color blue)
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


;; borrowed from js2-refactor
(defun bbweb--current-quotes-char ()
  "The char that is the current quote delimiter."
  (nth 3 (syntax-ppss)))

(defun bbweb-gettext-surround-string ()
  "Surrounds string with a call to gettext.  Cursor must be at string's start."
  (interactive)
  (if (looking-at "'")
      (save-excursion
        (insert "gettext(")
        (forward-char)
        (while (bbweb--current-quotes-char)
          (forward-char))
        (insert ")"))
    (message "not at the start of a string")))

(defun bbweb-js-mode-keys ()
  "Key definitions for 'js-mode' in bbweb project."
  (interactive)
  (local-set-key (kbd "C-c g") 'bbweb-gettext-surround-string))

(add-hook 'js-mode-hook 'bbweb-js-mode-keys)

(defun bbweb-gettext-surround-html-string ()
  "Surrounds string with a call to gettext.  Cursor must be at string's start."
  (interactive)
  (if (looking-at "\"")
      (save-excursion
        (forward-char)
        (insert "{{'")
        (forward-char)
        (while (bbweb--current-quotes-char)
          (forward-char))
        (backward-char)
        (insert "'|translate}}"))
    (message "not at the start of a string")))

(defun bbweb-html-mode-keys ()
  "Key definitions for 'html-mode' in bbweb project."
  (interactive)
  (local-set-key (kbd "C-c g") 'bbweb-gettext-surround-html-string))

(add-hook 'html-mode-hook 'bbweb-html-mode-keys)

(defun ngcomp-snippet-get-controller-name ()
  (upcase-initials (replace-regexp-in-string "Component" "Controller" (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))))

(defun ngcomp-snippet-get-template-url ()
  "Returns the name of the templateUrl for a component."
  (let ((path-in-assets (replace-regexp-in-string (concat (projectile-project-root) "app") "" buffer-file-name)))
    (replace-regexp-in-string "Component$" ".html" (file-name-sans-extension path-in-assets) t)))

(provide 'bbweb-project)
;;; bbweb-project.el ends here
