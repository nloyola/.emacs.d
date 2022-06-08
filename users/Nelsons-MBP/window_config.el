(defun nl/main-frame-set-size-and-position ()
  "Set the size and position of the Emacs window."
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-position frame 784 0)
    (set-frame-size frame 240 (/ (x-display-pixel-height) (frame-char-height)))
    ))

(defun nl/frame-set-size-and-position ()
  "Set the size and position of the Emacs window."
  (interactive)
  (let ((frame (selected-frame)))
    (nl/frame-set-size-and-position-hook frame)
    )
  )

(defun nl/frame-set-size-and-position-hook (frame)
  (set-frame-position frame 0 0)
  (set-frame-size frame 80 (floor (* (/ (x-display-pixel-height) (frame-char-height)) 0.80))))

(add-hook 'after-make-frame-functions 'nl/frame-set-size-and-position-hook t)

(add-hook 'window-setup-hook (lambda ()
                               (nl/main-frame-set-size-and-position)
                               ;;(make-frame-command)
                               ))
