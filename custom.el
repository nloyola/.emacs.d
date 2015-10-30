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
