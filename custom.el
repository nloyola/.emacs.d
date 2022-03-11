(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Numbers are helpful.")
 '(delete-by-moving-to-trash t nil nil "Customized with use-package dired")
 '(dired-dwim-target t nil nil "Customized with use-package dired")
 '(dired-recursive-copies 'always nil nil "Customized with use-package dired")
 '(global-auto-revert-non-file-buffers t nil nil "Customized with use-package dired")
 '(package-selected-packages
   '(direnv typescript-mode sbt-mode lsp-metals flycheck-rust rust-mode python-black pyvenv lsp-pyright lua-mode ormolu company-ghci flycheck-haskell haskell-mode js2-mode string-inflection multiple-cursors yasnippet visual-regexp orderless vertico treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile org-present package-utils ob-async meson-mode markdown-preview-mode macrostep glsl-mode flycheck embark-consult embark marginalia editorconfig dockerfile-mode docker dired-narrow dashboard consult company zzz-to-char ztree yasnippet-snippets yaml-mode wgrep web-mode wand visual-regexp-steroids use-package tide suscolors-theme super-save smooth-scrolling smartscan smartparens slime selected scratch scala-mode scad-mode ripgrep rainbow-delimiters protobuf-mode prettier-js phpunit php-mode org-bullets nswbuff move-text magit lsp-ui lsp-scala lsp-php lsp-java lorem-ipsum key-chord karma js2-refactor ivy-hydra ibuffer-projectile ialign hungry-delete highlight-indent-guides helm-xref helm-rg helm-projectile helm-flx helm-c-yasnippet helm-ag guide-key grunt gradle-mode goto-last-change git-timemachine git-gutter frog-menu flymd flycheck-ycmd flycheck-pos-tip fic-mode eyebrowse expand-region emr edit-server easy-kill dumb-jump doom-modeline diredful dired-sidebar diminish devdocs deadgrep dap-mode counsel company-ycmd company-lsp column-enforce-mode clipmon bm beacon anzu all-the-icons-dired ag ace-window))
 '(safe-local-variable-values
   '((eval progn
           (use-package nl-nordita-php-project :demand :load-path "~/.emacs.d/lisp")
           (use-package nl-typescript :demand :load-path "~/.emacs.d/lisp")
           (setq create-lockfiles nil php-cs-fixer-config-option
                 (concat
                  (projectile-project-root)
                  ".php-cs-fixer.php"))
           (projectile-register-project-type 'php-symfony
                                             '("composer.json" "src" "test" "vendor")
                                             :project-file "composer.json" :src-dir "src/" :test "make test" :test-suffix "Test" :test-dir "test/"))
     (eval progn
           (use-package nl-php-project :demand :load-path "~/.emacs.d/lisp")
           (setq create-lockfiles nil))
     (eval progn
           (use-package con-project :load-path "~/.emacs.d")
           (use-package angularjs :load-path "~/.emacs.d"))
     (eval progn
           (use-package bbweb-project :load-path "~/.emacs.d")
           (use-package angularjs :load-path "~/.emacs.d")
           (setq dumb-jump-default-project
                 (projectile-project-root)))
     (eval progn
           (use-package nl-angular :after typescript-mode :load-path "~/.emacs.d" :init
             (require 'tide))
           (setq dumb-jump-default-project
                 (projectile-project-root)))
     (eval progn
           (use-package nl-scala-project :after scala-mode :load-path "~/.emacs.d/lisp")
           (use-package bbweb-project :load-path "~/.emacs.d/lisp")
           (setq dumb-jump-default-project
                 (projectile-project-root)))
     (org-edit-src-content-indentation . 0)
     (org-src-preserve-indentation))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t (:family "IBM Plex Mono Medium" :height 120))))
 '(doom-modeline-project-parent-dir ((t (:foreground "#0f3b82"))))
 '(ediff-current-diff-A ((nil :background "#2b538a")))
 '(ediff-current-diff-B ((nil :background "#2b538a")))
 '(ediff-current-diff-C ((nil :background "#2b538a")))
 '(ediff-even-diff-A ((nil :background "#1f772c")))
 '(ediff-even-diff-B ((nil :background "#1f772c")))
 '(ediff-even-diff-C ((nil :background "#1f772c")))
 '(ediff-odd-diff-A ((nil :background "#862b27")))
 '(ediff-odd-diff-B ((nil :background "#862b27")))
 '(ediff-odd-diff-C ((nil :background "#862b27")))
 '(header-line ((t (:inherit mode-line :background "gray30"))))
 '(lsp-ui-peek-highlight ((nil :foreground "gray60" :background "gray20")))
 '(lsp-ui-peek-peek ((nil :background "gray30")))
 '(org-table ((t :foreground "#91b831")))
 '(show-paren-match ((t (:background "#def" :box nil))))
 '(vterm-color-black ((t (:foreground "#000000" :background "#777777")))))
