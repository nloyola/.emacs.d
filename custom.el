(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zeal-at-point whole-line-or-region visual-regexp-steroids use-package undo-tree transpose-frame smooth-scrolling smartscan smartparens shackle scratch sbt-mode revive rainbow-delimiters powerline noflet monokai-theme markdown-mode magit macrostep key-chord js2-mode hydra hungry-delete helm-swoop helm-projectile helm-gtags helm-flx helm-c-yasnippet helm-ag guide-key groovy-mode gradle-mode goto-last-change git-timemachine ggtags framemove flycheck fic-mode expand-region emacs-eclim edit-server company color-identifiers-mode beacon ag ace-window)))
 '(safe-local-variable-values
   (quote
    ((eval global-set-key
           [f5]
           (quote sbt-command))
     (eval setq-default indent-tabs-mode nil)
     (eval setq projectile-find-dir-includes-top-level t)
     (projectile-test-suffix-function lambda
                                      (project-type)
                                      "" "Spec")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-emacs-eclim-candidate-face ((t (:inherit ac-candidate-face))))
 '(ac-emacs-eclim-selection-face ((t (:inherit ac-selection-face))))
 '(mode-line ((t (:foreground "#030303" :background "#6b6b6b" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#6b6b6b" :box nil)))))
