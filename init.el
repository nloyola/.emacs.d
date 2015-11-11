;;; Begin initialization
;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;; Bootstrap use-package
;; Install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)
(setq use-package-verbose t)

;; see http://emacs.stackexchange.com/questions/539/how-do-i-measure-performance-of-elisp-code
(defmacro with-timer (&rest forms)
  "Run the given FORMS, counting and displaying the elapsed time."
  (declare (indent 0))
  (let ((nowvar (make-symbol "now"))
        (body `(progn ,@forms)))
    `(let ((,nowvar (current-time)))
       (prog1 ,body
         (let ((elapsed (float-time (time-subtract (current-time) ,nowvar))))
           (when (> elapsed 0.001)
             (message "spent (%.3fs)" elapsed)))))))

;; Parses the config.org file and processes the emacs-lisp blocks. If any of the blocks generates an
;; error, Emacs will not halt, instead it will continue and accumulate the errors. If any errors
;; were encountered, they will be reported in the *init errors* buffer.
;;
;; see http://emacsninja.com/posts/failing-gracefully.html
(let (errors)
  (with-temp-buffer
    (insert-file "~/.emacs.d/config.org")
    (goto-char (point-min))
    (while (not (eobp))
      (forward-line 1)
      (cond
       ;; report headers - look at headers of levels 1, 2, or 3
       ((looking-at "\\*\\{1,3\\} +.*$")
        (message "%s" (match-string 0)))
       ;; skip untangled blocks
       ((looking-at "^#\\+BEGIN_SRC +emacs-lisp +:tangle +no$")
        (message "...skipped"))
       ;; evaluate code blocks
       ((looking-at "^#\\+BEGIN_SRC +emacs-lisp$")
        (let (src-beg src-end)
          (condition-case error
              (progn
                (setq src-beg (match-end 0))
                (search-forward "\n#+END_SRC")
                (setq src-end (match-beginning 0))
                (with-timer (eval-region src-beg src-end)))
            (error
             (push (format "%s for:\n%s\n\n---\n"
                           (error-message-string error)
                           (buffer-substring src-beg src-end))
                   errors)))))
       ;; finish on the next level-1 header
       ((looking-at "^\\* ")
        (goto-char (point-max))))))
  (when errors
    (with-current-buffer (get-buffer-create "*init errors*")
      (insert (format "%i error(s) found\n\n" (length errors)))
      (dolist (error (nreverse errors))
        (insert error "\n"))
      (goto-char (point-min))
      (special-mode))
    (setq initial-buffer-choice (lambda () (get-buffer "*init errors*")))))
