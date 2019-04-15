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
      (let* ((desired-width-in-chars 244)
             (desired-width-in-pixels (* desired-width-in-chars (frame-char-width)))
             (window-frame-in-pixels 16))
        (set-face-attribute 'default frame :font "Input Mono Condensed-14")
        ;; cannot use negative value for X, it is not placed in correct location on startup
        (set-frame-position frame (- (+ 1920 3840) window-frame-in-pixels desired-width-in-pixels) 0)
        (set-frame-size frame
                        desired-width-in-chars
                        (/ (x-display-pixel-height) (frame-char-height))))))))

(defun nl/new-frame ()
  "Create a new frame on the 4k display."
  (interactive)
  (let ((frame (make-frame)))
    (select-frame-set-input-focus frame)
    (switch-to-buffer "*scratch*")
    (set-frame-position frame 2200 120)
    (set-face-attribute 'default frame :font "Input Mono Condensed-14")
    (set-frame-size frame
                    (* 120 (frame-char-width frame))
                    (* 70 (frame-char-height frame))
                    t)))

(defun nl/frame-3rd-display ()
  "Create a new frame on the 3rd display."
  (interactive)
  (let ((frame (make-frame)))
    (select-frame-set-input-focus frame)
    (switch-to-buffer "*scratch*")
    (set-frame-position frame -1 0)
    (set-face-attribute 'default frame :font "Input Mono Condensed-9" :height 90)
    (set-frame-size frame
                    (/ (x-display-pixel-width frame) (frame-char-width frame))
                    (floor (* (/ (x-display-pixel-height frame) (frame-char-height frame)) 0.80)))))

(defun nl/window-setup-hook ()
  (let ((frame (selected-frame)))
    (nl/main-frame-set-size-and-position)
    ;;(resume)
    (nl/frame-3rd-display)
    (select-frame-set-input-focus frame)))

(add-hook 'window-setup-hook 'nl/window-setup-hook)

;; 0      1         2         3         4         5         6        7          8         9         0
;; 456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789
