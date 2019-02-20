(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-threshold 50)
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(eclimd-autostart nil)
 '(helm-ag-use-temp-buffer nil)
 '(js-indent-level 2 t)
 '(js2-basic-offset 2 t)
 '(js2-bounce-indent-p nil)
 '(js2-highlight-level 3)
 '(package-selected-packages
   (quote
    (nswbuff ztree eglot easy-kill eyebrowse flycheck-ycmd company-ycmd workgroups wgrep tide selected scad-mode phpunit move-text ialign helm-xref headlong git-gutter dired-sidebar devdocs scala-mode php-mode zop-to-char yaml-mode web-mode wand visual-regexp bm company-tern tern super-save spaceline slime org-bullets protobuf-mode lorem-ipsum less-css-mode karma js2-refactor counsel swiper ivy-hydra ivy highlight-indent-guides helm grunt emr company-emacs-eclim dumb-jump diredful clipmon zeal-at-point whole-line-or-region visual-regexp-steroids use-package undo-tree transpose-frame smooth-scrolling smartscan smartparens shackle scratch sbt-mode revive rainbow-delimiters powerline noflet monokai-theme markdown-mode magit macrostep key-chord js2-mode hydra hungry-delete helm-swoop helm-projectile helm-gtags helm-flx helm-c-yasnippet helm-ag guide-key groovy-mode gradle-mode goto-last-change git-timemachine ggtags framemove flycheck fic-mode expand-region emacs-eclim edit-server company color-identifiers-mode beacon ag ace-window)))
 '(safe-local-variable-values
   (quote
    ((eval progn
           (use-package con-project :load-path "~/.emacs.d")
           (use-package angularjs :load-path "~/.emacs.d"))
     (eval progn
           (use-package angular :load-path "~/.emacs.d")
           (setq dumb-jump-default-project
                 (projectile-project-root)))
     (eval progn
           (use-package bbweb-project :load-path "~/.emacs.d")
           (use-package angularjs :load-path "~/.emacs.d")
           (setq dumb-jump-default-project
                 (projectile-project-root)))
     (eval progn
           (setq dumb-jump-default-project
                 (projectile-project-root)
                 projectile-test-suffix-function
                 (lambda
                   (project-type)
                   "" ".spec")))
     (org-edit-src-content-indentation . 0)
     (org-src-preserve-indentation)
     (eval progn
           (use-package bbweb-project :load-path "~/.emacs.d")))))
 '(whole-line-or-region-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-emacs-eclim-candidate-face ((t (:inherit ac-candidate-face))))
 '(ac-emacs-eclim-selection-face ((t (:inherit ac-selection-face))))
 '(mode-line ((t (:foreground "#030303" :background "#6b6b6b" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#6b6b6b" :box nil)))))
