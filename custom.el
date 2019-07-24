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
 '(coverlay:mark-tested-lines nil)
 '(coverlay:tested-line-background-color "#1f772c")
 '(coverlay:untested-line-background-color "#862b27")
 '(eclimd-autostart nil)
 '(helm-ag-use-temp-buffer nil)
 '(js-indent-level 2 t)
 '(js2-basic-offset 2 t)
 '(js2-bounce-indent-p nil)
 '(js2-highlight-level 3)
 '(package-selected-packages
   (quote
    (prettier-js flymd column-enforce-mode yaml-mode deadgrep frog-menu frog-jump-buffer wgrep zzz-to-char ztree yasnippet-snippets tide selected ripgrep scad-mode phpunit nswbuff move-text lsp-scala lsp-php lsp-java company-lsp lsp-ui ialign posframe helm-xref helm-rg flycheck-pos-tip eyebrowse flycheck-ycmd company-ycmd ycmd git-gutter easy-kill doom-modeline dired-sidebar devdocs dap-mode anzu all-the-icons-dired ycmd-eldoc scala-mode php-mode zop-to-char web-mode wand visual-regexp bm company-tern tern super-save spaceline slime org-bullets protobuf-mode lorem-ipsum less-css-mode karma js2-refactor counsel swiper ivy-hydra ivy highlight-indent-guides helm grunt emr company-emacs-eclim dumb-jump diredful clipmon zeal-at-point whole-line-or-region visual-regexp-steroids use-package undo-tree transpose-frame smooth-scrolling smartscan smartparens shackle scratch sbt-mode revive rainbow-delimiters powerline noflet monokai-theme markdown-mode magit macrostep key-chord js2-mode hydra hungry-delete helm-swoop helm-projectile helm-gtags helm-flx helm-c-yasnippet helm-ag guide-key groovy-mode gradle-mode goto-last-change git-timemachine ggtags framemove flycheck fic-mode expand-region emacs-eclim edit-server company color-identifiers-mode beacon ag ace-window)))
 '(php-mode-coding-style (quote nl/php))
 '(safe-local-variable-values
   (quote
    ((eval progn
           (use-package nl-scala-project :load-path "~/.emacs.d")
           (use-package bbweb-project :load-path "~/.emacs.d")
           (setq dumb-jump-default-project
                 (projectile-project-root)))
     (eval progn
           (use-package bbweb-project :load-path "~/.emacs.d")
           (setq dumb-jump-default-project
                 (projectile-project-root)))
     (projectile-project-run-cmd . "mkdir -p build; cd build; cmake ..; make run")
     (projectile-project-compilation-cmd . "mkdir -p build; cd build; cmake ..; make")
     (eval setq require-final-newline nil)
     (eval progn
           (use-package bbweb-project :load-path "~/.emacs.d")
           (use-package angularjs :load-path "~/.emacs.d")
           (setq dumb-jump-default-project
                 (projectile-project-root)))
     (eval progn
           (use-package nl-angular :load-path "~/.emacs.d" :init
             (require
              (quote tide)))
           (setq dumb-jump-default-project
                 (projectile-project-root)))
     (eval progn
           (use-package con-project :load-path "~/.emacs.d")
           (use-package angularjs :load-path "~/.emacs.d"))
     (eval progn
           (use-package angular :load-path "~/.emacs.d")
           (setq dumb-jump-default-project
                 (projectile-project-root)))
     (org-edit-src-content-indentation . 0)
     (org-src-preserve-indentation)
     (eval progn
           (use-package bbweb-project :load-path "~/.emacs.d"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-emacs-eclim-candidate-face ((t (:inherit ac-candidate-face))))
 '(ac-emacs-eclim-selection-face ((t (:inherit ac-selection-face))))
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
 '(font-lock-negation-char-face ((nil :foreground "red" :background "#0f3b82")))
 '(font-lock-warning-face ((t (:background "#0f3b82"))))
 '(mode-line ((t (:foreground "#030303" :background "#6b6b6b" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#6b6b6b" :box nil)))))
