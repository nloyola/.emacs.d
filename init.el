;;; init.el --- Emacs Initialization File

;;; Commentary:
;;; initialization

;;; Code:

;; For debugging startup time only
;; (with-current-buffer (messages-buffer)
;;   (goto-char (point-max))
;;   (switch-to-buffer (current-buffer)))

(defun nl/display-startup-time ()
  (let ((elapsed
         (float-time
          (time-subtract (current-time) emacs-start-time))))
    (message "Emacs loaded in %.3f seconds with %d garbage collections"
             elapsed gcs-done)))

(add-hook 'emacs-startup-hook #'nl/display-startup-time)

(defconst emacs-start-time (current-time))

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      read-process-output-max (* 1024 1024)
      auto-window-vscroll nil)

(defun nl/after-init ()
  (setq file-name-handler-alist file-name-handler-alist-old
        gc-cons-threshold 200000000
        gc-cons-percentage 0.1)
  (garbage-collect))

(add-hook 'after-init-hook `nl/after-init t)

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

;;; Set up package
(require 'package)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))

(setq package-archive-priorities
      '(;;("melpa-stable" . 10)
        ;; ("marmalade" . 7)
        ("gnu"       . 5)
        ("org"       . 7)
        ("melpa"     . 10)))

(package-initialize)

;; Using HTTPS for downloading packages, make sure HTTPS is not going through a proxy.
;; (setenv "https_proxy" "")
;; (setenv "http_proxy" "")

;;; Bootstrap use-package
(setq-default use-package-always-ensure t         ; Auto-download package if not exists
              use-package-always-defer t          ; Always defer load package to speed up startup time
              use-package-expand-minimally nil    ; make the expanded code as minimal as possible
              use-package-enable-imenu-support t) ; Let imenu finds use-package definitions

;; use only for debugging startup time
(setq use-package-verbose t)               ; report loading details)

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
  :load-path "~/.emacs.d/lisp"
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

(defun load-config-org()
  "Parse the config.org file to process each emacs-lisp block.

If any of the blocks generates an error, Emacs will not
halt, instead it will continue and accumulate the errors.
If any errors were encountered, they will be reported in
the *init errors* buffer.

See http://emacsninja.com/posts/failing-gracefully.html"
  (let (errors)
    (with-temp-buffer
      (insert-file-contents "~/.emacs.d/config.org")
      (goto-char (point-min))
      (let (heading section-decl src-beg src-end)
	(while (not (eobp))
	  (forward-line 1)
	  (pl-parse
	   (pl-re "^\\*\\{1,5\\} +.*$" :beg)
	   (setq heading (match-string 0)))
	  (pl-parse
	   (pl-re "^#\\+BEGIN_SRC +emacs-lisp.*$" :beg)
	   (setq src-beg (match-end 0))
	   (setq section-decl (match-string 0))
	   (pl-until
	    (pl-re "\n#\\+END_SRC$" :end))
	   (setq src-end (match-beginning 0))

	   (if (string-match ":tangle +no" section-decl)
	       ;;(message "Skipped: %s" heading)
               (ignore)
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

;; Loading "config.org" with org-babel-load-file is slower than paring out all the emacs-lisp blocks.
;; See load-config-org.
;;
;;(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(defun nl/show-messages-on-startup ()
  "Show the *Messages* buffer after starting Emacs."
  (setq initial-buffer-choice (lambda () (get-buffer "*Messages*"))))


;; (nl/show-messages-on-startup)
