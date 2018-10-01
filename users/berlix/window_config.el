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
      (set-frame-position frame 2704 0)
      (set-frame-size frame
                      (- (/ (x-display-pixel-width) (frame-char-width)) 356)
                      (/ (x-display-pixel-height) (frame-char-height)))
      (set-face-attribute 'default frame :font "Fira Mono Medium" :height 160 :weight 'medium)))))

(nl/main-frame-set-size-and-position)

(defun nl/new-frame ()
  "Create a new frame on the 4k display."
  (interactive)
  (let ((frame (make-frame)))
    (select-frame-set-input-focus frame)
    (switch-to-buffer "*scratch*")
    (set-frame-position frame 2200 120)
    (set-face-attribute 'default frame :font "Fira Mono Medium" :height 160 :weight 'medium)
    (set-frame-size frame
                    (* 100 (frame-char-width frame))
                    (* 70(frame-char-height frame))
                    t)))

(defun nl/frame-3rd-display ()
  "Create a new frame on the 3rd display."
  (interactive)
  (let ((frame (make-frame)))
    (select-frame-set-input-focus frame)
    (switch-to-buffer "*scratch*")
    (set-frame-position frame -1 0)
    (set-face-attribute 'default frame :font "Fira Mono Medium" :height 120)
    (set-frame-size frame
                    (/ (x-display-pixel-width frame) (frame-char-width frame))
                    (floor (* (/ (x-display-pixel-height frame) (frame-char-height frame)) 0.80)))))

(defun nl/window-setup-hook ()
  (let ((frame (selected-frame)))
    (nl/main-frame-set-size-and-position)
    (resume)
    (nl/frame-3rd-display)
    (select-frame-set-input-focus frame)))

(add-hook 'window-setup-hook 'nl/window-setup-hook)

;; 0      1         2         3         4         5         6        7          8         9         0
;; 456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789
