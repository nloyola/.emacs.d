(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((eval global-set-key
           [f5]
           (quote
            (lambda nil
              (interactive)
              (eclim-run-configuartion "Run PlateDecoder"))))
     (eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (emacs-lisp-mode)
           (when
               (fboundp
                (quote flycheck-mode))
             (flycheck-mode -1))
           (unless
               (featurep
                (quote package-build))
             (let
                 ((load-path
                   (cons ".." load-path)))
               (require
                (quote package-build))))
           (package-build-minor-mode)
           (set
            (make-local-variable
             (quote package-build-working-dir))
            (expand-file-name "../working/"))
           (set
            (make-local-variable
             (quote package-build-archive-dir))
            (expand-file-name "../packages/"))
           (set
            (make-local-variable
             (quote package-build-recipes-dir))
            default-directory))
     (eval setq my-project-dir
           (concat
            (locate-dominating-file default-directory ".dir-locals.el"))
           my-project-include-dir
           (expand-file-name
            (concat my-project-dir "src"))
           flycheck-gcc-include-path
           (list my-project-include-dir)
           flycheck-gcc-language-standard "c++11")
     (eval setq my-project-dir
           (concat
            (locate-dominating-file default-directory ".dir-locals.el"))
           my-project-include-dir
           (expand-file-name
            (concat my-project-dir "src"))
           flycheck-gcc-include-path
           (list my-project-include-dir)
           flycheck-clang-language-standard "c++11")
     (eval setq my-project-dir
           (concat
            (locate-dominating-file default-directory ".dir-locals.el"))
           my-project-include-dir
           (expand-file-name
            (concat my-project-dir "src"))
           flycheck-gcc-include-path
           (list my-project-include-dir))
     (eval setq my-project-dir
           (concat
            (locate-dominating-file default-directory ".dir-locals.el"))
           my-project-include-dir
           (expand-file-name
            (concat my-project-dir "src"))
           flycheck-gcc-include-path my-project-include-dir)
     (eval add-hook
           (quote flycheck-before-syntax-check-hook)
           (lambda nil
             (setq flycheck-clang-include-path
                   (list my-project-include-dir))))
     (eval setq my-project-dir
           (concat
            (locate-dominating-file default-directory ".dir-locals.el"))
           my-project-include-dir
           (expand-file-name
            (concat my-project-dir "src")))
     (eval add-hook
           (quote c++-mode-hook)
           (lambda nil
             (setq flycheck-clang-include-path
                   (list my-project-include-dir))))
     (eval message "my-project-include-dir: %s" my-project-include-dir)
     (eval setq my-project-dir
           (concat
            (locate-dominating-file default-directory ".dir-locals.el"))
           my-project-include-dir
           (concat my-project-dir "src"))
     (eval add-hook
           (quote c++-mode-hook)
           (lambda nil
             (setq flycheck-clang-include-path
                   (list
                    (concat my-project-dir "src")))))
     (eval add-hook
           (quote c++-mode-hook)
           (lambda nil
             (setq flycheck-clang-include-path
                   (list
                    (concat my-project-dir "/src")))))
     (eval global-set-key
           [f5]
           (quote
            (lambda nil
              (interactive)
              (let*
                  ((default-directory my-project-dir))
                (call-interactively
                 (function compile))))))
     (eval setq my-project-dir
           (concat
            (locate-dominating-file default-directory ".dir-locals.el")))
     (eval global-set-key
           [f5]
           (quote sbt-command))
     (eval setq-default indent-tabs-mode nil)
     (eval setq projectile-find-dir-includes-top-level t)
     (eval global-set-key
           [(control f5)]
           (quote
            (lambda nil
              (interactive)
              (nl/gradle-javadoc))))
     (eval global-set-key
           [f5]
           (quote
            (lambda nil
              (interactive)
              (nl/eclim-run-configuartion "Run PlateDecoder"))))
     (eval global-set-key
           [f5]
           (quote
            (lambda nil
              (interactive)
              (eclim-java-run-run "Run PlateDecoder"))))
     (eval setq eclimd-default-workspace
           (concat
            (locate-dominating-file default-directory ".dir-locals.el")
            ".."))
     (projectile-project-run-cmd . "./gradlew -q --console=plain run")
     (projectile-project-compilation-cmd . "gradle -q --console=plain build")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-emacs-eclim-candidate-face ((t (:inherit ac-candidate-face))))
 '(ac-emacs-eclim-selection-face ((t (:inherit ac-selection-face))))
 '(mode-line ((t (:foreground "#030303" :background "#6b6b6b" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#6b6b6b" :box nil)))))
