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
 )
