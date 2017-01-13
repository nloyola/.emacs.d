;;; Begin initialization
;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;; A secure Emacs environment
;;
;; see https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(require 'cl)
(setq tls-checktrust t)

(setq python (executable-find "python"))

(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string (concat python " -m certifi"))))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

;;; Set up package
(require 'package)

(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))

(setq package-archives nil)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)

(package-initialize)

;; Using HTTPS for downloading packages, make sure HTTPS is not going through a proxy.
(setenv "https_proxy" "")

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
(setq use-package-verbose nil)

(use-package pl
  :load-path "~/src/github/elisp/emacs-pl"
  :commands pl-parse
  )

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

(defun load-config-org()
  (let (errors)
    (with-temp-buffer
      (insert-file-contents "~/.emacs.d/config.org")
      (goto-char (point-min))
      (let (heading section-decl src-beg src-end)
	(while (not (eobp))
	  (forward-line 1)
	  (pl-parse
	   (pl-re "^\\*\\{1,3\\} +.*$" :beg)
	   (setq heading (match-string 0)))
	  (pl-parse
	   (pl-re "^#\\+BEGIN_SRC +emacs-lisp.*$" :beg)
	   (setq src-beg (match-end 0))
	   (setq section-decl (match-string 0))
	   (pl-until
	    (pl-re "\n#\\+END_SRC$" :end))
	   (setq src-end (match-beginning 0))

	   (if (string-match ":tangle +no" section-decl)
	       (message "Skipped: %s" heading)
	     (condition-case error
		 (progn
		   (message "%s" heading)
		   (with-timer (eval-region src-beg src-end)))
	       (error
		(push (format "%s for:\n%s\n\n---\n"
			      (error-message-string error)
			      (buffer-substring src-beg src-end))
		      errors)))
             )))))
    (when errors
      (with-current-buffer (get-buffer-create "*init errors*")
	(insert (format "%i error(s) found\n\n" (length errors)))
	(dolist (error (nreverse errors))
	  (insert error "\n"))
	(goto-char (point-min))
	(special-mode))
      (setq initial-buffer-choice (lambda () (get-buffer "*init errors*"))))))

(load-config-org)
