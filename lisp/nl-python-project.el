;;; nl-python-project --- package to help with development in Python projects

;;; Commentary:

;; Settings and functions to support software development.

;;; Code:

(provide 'nl-python-project)

(require 'python)

(defhydra hydra-nl-python (:hint nil)
  "Project"
  ("a" hydra-nl-align/body "align" :color blue :column "Python")
  ("i" nl/indent-whole-buffer "indent buffer" :color blue)
  ("x" lsp-find-references "find references" :color blue)
  ("p" dumb-jump-go-prompt "prompt" :color blue))

;; this def uses a lambda to show that it is possible, id does not need to use it
(key-chord-define python-mode-map "jc" '(lambda () (interactive)
                                         (hydra-nl-python/body)))


;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
