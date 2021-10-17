;;; nl-python-project --- package to help with development in Python projects

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:

(provide 'nl-python-project)

(require 'python)
(require 'projectile)
(require 'lsp)

(defvar nl/django-project-root (projectile-project-root))

(defconst python-beginning-of-class-regexp
  (rx line-start
      (* (syntax whitespace))
      "class"
      (+ (syntax whitespace))
      (group (+ (or (syntax word) (syntax symbol))))
      (* (syntax whitespace)))
  "Regular expression for a python class name.")

(defconst python-test-beginning-regexp
  (eval-when-compile
    (rx line-start
        (one-or-more (syntax whitespace))
        "def"
        (one-or-more (syntax whitespace))
        (group "test" (+ (not (syntax open-parenthesis))))
        (one-or-more (not ":"))
        ":"
        ))
  "Regular expression for a python test function.")

(defun nl/lsp-pyright-organize-imports ()
  (interactive)
  (let ((fname (vector (concat "file://" buffer-file-name))))
    (lsp-send-execute-command "pyright.organizeimports" (vector fname))))

(defun nl/python-test-find-method-name ()
  "Determines the name of the python test's method name."
  (save-excursion
    (when (re-search-backward python-test-beginning-regexp nil t)
      (match-string-no-properties 1))))

(defun nl/python-test-find-class-name ()
  "Determines the name of the Pytest suite name."
  (save-excursion
    (when (re-search-backward python-beginning-of-class-regexp nil t)
      (match-string-no-properties 1))))

(defun python-filename-p (filename)
  (string-match-p "\\.py$" filename))

(defun nl/pytest-file-name ()
  "Return the python's file module name for the file that the buffer is visiting."
  (unless (buffer-file-name) (error "not a file buffer"))
  (unless (boundp 'nl/django-project-root) (error "the file is not in a python project"))
  (let ((file-name (nth 1 (split-string buffer-file-name (concat nl/django-project-root "/")))))
    (unless file-name (error "file not in project"))
    (message "%s" file-name)
    (unless (python-filename-p file-name) (error "not a python file"))
    file-name)
  )

(defun nl/python-module-name ()
  "Return the python's file module name for the file that the buffer is visiting."
  (interactive)
  (if (buffer-file-name)
      (let ((file-name (file-name-nondirectory (buffer-file-name))))
        (if (python-filename-p file-name)
            (file-name-base (replace-regexp-in-string "\.py" "" file-name))
          (progn
            (message "not a python file")
            nil)))
    (progn
      (message "not a file buffer")
      nil)))

(defun nl/django-command-in-proj-root (command)
  "Run the compile COMMAND at the project's root directory."
  (interactive)
  (compile (format "cd %s && python manage.py %s" nl/django-project-root command)))

(defun nl/django-test-command-in-proj-root (command)
  "Run the compile COMMAND at the project's root directory."
  (interactive)
  (compile (format "cd %s && pytest -s --no-migrations --reuse-db %s" nl/django-project-root command)))

(defun nl/django-test-project ()
  "For the class the cursor is in, run the Pytest test suite."
  (interactive)
  (nl/django-test-command-in-proj-root "-m \"not end_to_end\""))

(defun nl/django-test-this-file ()
  "For the class the cursor is in, run the Pytest test suite."
  (interactive)
  (nl/django-test-command-in-proj-root (nl/pytest-file-name)))

(defun nl/django-test-only-this-class ()
  "For the class the cursor is in, run the Pytest test suite."
  (interactive)
  (nl/django-test-command-in-proj-root
   (string-join (list (nl/pytest-file-name)
                      (nl/python-test-find-class-name))
                "::")
   ))

(defun nl/django-test-only-this-method ()
  "For the class the cursor is in, run the Pytest test suite."
  (interactive)
    (nl/django-test-command-in-proj-root
     (string-join (list (nl/pytest-file-name)
                        (nl/python-test-find-class-name)
                        (nl/python-test-find-method-name))
                  "::")
     ))


(defhydra hydra-nl/python-test (:color blue)
  "Test"
  ("c" nl/django-test-only-this-class "only this class" :column "Test")
  ("m" nl/django-test-only-this-method "only this class" :column "Test")
  ("p" nl/ng-test "project" :column "Test"))

(defhydra hydra-nl-python (:hint nil)
  "Project"
  ("a" hydra-nl-align/body "align" :color blue :column "Python")
  ("i" nl/indent-whole-buffer "indent buffer" :color blue)
  ("x" lsp-find-references "find references" :color blue)
  ("p" dumb-jump-go-prompt "prompt" :color blue)
  ("t" hydra-nl/python-test/body "test" :color blue))

;; this def uses a lambda to show that it is possible, id does not need to use it
(key-chord-define python-mode-map "jc" '(lambda () (interactive)
                                         (hydra-nl-python/body)))


(define-key python-mode-map (kbd "C-c , m") 'nl/django-test-only-this-method)
(define-key python-mode-map (kbd "C-c , c") 'nl/django-test-only-this-class)
(define-key python-mode-map (kbd "C-c , f") 'nl/django-test-this-file)
(define-key python-mode-map (kbd "C-c , p") 'nl/django-test-project)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
