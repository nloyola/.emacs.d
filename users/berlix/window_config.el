;; (defun nl/monitor-pixel-width ()
;;   (caddr (cdr (assoc 'geometry (frame-monitor-attributes)))))

;; (defun nl/preferred-font-size ()
;;   (let ( (pixel-width (nl/monitor-pixel-width)) )
;;   (cond
;;     ((<= pixel-width 1920) 7)
;;     ((<= pixel-width 3840) 18))))

;; (defvar nl/preferred-font-size (nl/preferred-font-size))

;; (defvar nl/regular-font
;;    (format "Ubuntu Mono-%d:weight=normal" nl/preferred-font-size))

;; (defvar nl/symbol-font
;;    (format "Ubuntu Mono-%d:weight=normal" nl/preferred-font-size))

(defun nl/main-frame-set-size-and-position ()
  "Set the size and position of the Emacs window."
  (interactive)
  (let* ((frame (selected-frame))
         (display-attributes (display-monitor-attributes-list))
         (num-displays (length display-attributes)))
    (cond
     ((eq num-displays 2)
      (set-frame-position frame -1 0)
      (set-frame-size frame 229 (/ (x-display-pixel-height) (frame-char-height))))
     ((eq num-displays 3)
      (set-frame-position frame 2744 0)
      (set-frame-size frame
                      (- (/ (x-display-pixel-width) (frame-char-width)) 390)
                      (/ (x-display-pixel-height) (frame-char-height)))
      (set-face-attribute 'default frame :font "Ubuntu Mono" :height 180 :weight 'medium)))))

(defun nl/frame-set-size-and-position ()
  "Set the size and position of the Emacs window."
  (interactive)
  (let ((frame (selected-frame)))
    (nl/frame-set-size-and-position-hook frame)))

(defun nl/frame-set-size-and-position-hook (frame)
  (set-frame-position frame -1 0)
  (set-frame-size frame
                  (/ (x-display-pixel-width) (frame-char-width))
                  (floor (* (/ (x-display-pixel-height) (frame-char-height)) 0.80)))
  (set-face-attribute 'default frame :font "Ubuntu Mono" :height 110 :weight 'medium))

(add-hook 'after-make-frame-functions 'nl/frame-set-size-and-position-hook t)

(defun nl/window-setup-hook ()
  (let ((frame (selected-frame)))
    (nl/main-frame-set-size-and-position)
    (resume)
    (make-frame-command)
    (select-frame-set-input-focus frame)))

(add-hook 'window-setup-hook 'nl/window-setup-hook)
