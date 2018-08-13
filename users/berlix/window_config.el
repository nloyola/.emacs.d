(defun nl/monitor-pixel-width ()
  (caddr (cdr (assoc 'geometry (frame-monitor-attributes)))))

(defun nl/preferred-font-size ()
  (let ( (pixel-width (nl/monitor-pixel-width)) )
  (cond
    ((<= pixel-width 1920) 7)
    ((<= pixel-width 3840) 16))))

(defvar nl/preferred-font-size (nl/preferred-font-size))

(defvar nl/regular-font
   (format "Fira Code Medium-%d:weight=normal" nl/preferred-font-size))

(defvar nl/symbol-font
   (format "Fira Code Medium-%d:weight=normal" nl/preferred-font-size))

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
      (set-frame-position frame 2500 0)
      (set-frame-size frame 250 (/ (x-display-pixel-height) (frame-char-height)))))))

(defun nl/frame-set-size-and-position ()
  "Set the size and position of the Emacs window."
  (interactive)
  (let ((frame (selected-frame)))
    (nl/frame-set-size-and-position-hook frame)
    )
  )

(defun nl/frame-set-size-and-position-hook (frame)
  (set-frame-position frame 1920 60)
  (set-frame-size frame 120 (floor (* (/ (x-display-pixel-height) (frame-char-height)) 0.80)))
  (cond
   ((eq window-system 'x)
    (if (and (fboundp 'find-font) (find-font (font-spec :name nl/regular-font)))
        (set-frame-font nl/regular-font)
      (set-frame-font "7x14")))))

(add-hook 'after-make-frame-functions 'nl/frame-set-size-and-position-hook t)

(add-hook 'window-setup-hook (lambda ()
                               (nl/main-frame-set-size-and-position)
                               (resume)
                               (make-frame-command)))
