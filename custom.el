(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-by-moving-to-trash t)
 '(dired-dwim-target t)
 '(dired-recursive-copies 'always)
 '(global-auto-revert-non-file-buffers t)
 '(package-selected-packages
   '(zzz-to-char ztree yasnippet-snippets yaml-mode wgrep web-mode wand visual-regexp-steroids use-package tide suscolors-theme super-save smooth-scrolling smartscan smartparens slime selected scratch scala-mode scad-mode ripgrep rainbow-delimiters protobuf-mode prettier-js phpunit php-mode org-bullets nswbuff move-text magit lsp-ui lsp-scala lsp-php lsp-java lorem-ipsum key-chord karma js2-refactor ivy-hydra ibuffer-projectile ialign hungry-delete highlight-indent-guides helm-xref helm-rg helm-projectile helm-flx helm-c-yasnippet helm-ag guide-key grunt gradle-mode goto-last-change git-timemachine git-gutter frog-menu flymd flycheck-ycmd flycheck-pos-tip fic-mode eyebrowse expand-region emr edit-server easy-kill dumb-jump doom-modeline diredful dired-sidebar diminish devdocs deadgrep dap-mode counsel company-ycmd company-lsp column-enforce-mode clipmon bm beacon anzu all-the-icons-dired ag ace-window))
 '(safe-local-variable-values
   '((eval progn
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
 '(vterm-color-black ((t (:foreground "#000000" :background "#777777")))))
