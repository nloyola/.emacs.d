;;; Begin initialization

(defconst emacs-start-time (current-time))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (fringe-mode -1))

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq inhibit-startup-message t
      initial-scratch-message "")

;; when tramp is slow
;;
;; - not working really
;;(setq projectile-mode-line "Projectile")

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
(setq package-archives
      '(("gnu"     . "https://elpa.gnu.org/packages/")
        ;;("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("marmalade"    . "https://marmalade-repo.org/packages/"))
      package-archive-priorities
      '(;;("melpa-stable" . 10)
        ("gnu"       . 10)
        ("marmalade" . 7)
        ("melpa"     . 5)))

(package-initialize)

;; Using HTTPS for downloading packages, make sure HTTPS is not going through a proxy.
(setenv "https_proxy" "")
(setenv "http_proxy" "")

;;; Bootstrap use-package
(setq-default use-package-always-ensure t         ; Auto-download package if not exists
              use-package-always-defer nil        ; Always defer load package to speed up startup time
              use-package-verbose t               ; report loading details
              use-package-expand-minimally t      ; make the expanded code as minimal as possible
              use-package-enable-imenu-support t) ; Let imenu finds use-package definitions

;; Install use-package if it's not already installed.
;; use-package is used to configure the rest of the packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

(require 'diminish)
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
;;; Finalization

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)
