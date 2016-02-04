(defun nl/set-size-and-position ()
  "Set the size and position of the Emacs window."
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-position frame -1 0)
    (set-frame-size frame 229 (/ (x-display-pixel-height) (frame-char-height)))
    ))

(defun nl/set-frame-size-and-position ()
  "Set the size and position of the Emacs window."
  (interactive)
  (let ((frame (selected-frame)))
    (nl/set-frame-size-and-position-hook frame)
    )
  )

(defun nl/set-frame-size-and-position-hook (frame)
  (set-frame-position frame 2200 60)
  (set-frame-size frame 120 (floor (* (/ (x-display-pixel-height) (frame-char-height)) 0.80)))
  )

(add-hook 'after-make-frame-functions 'nl/set-frame-size-and-position-hook t)

(add-hook 'window-setup-hook (lambda ()
                               (nl/set-size-and-position)
                               (resume)
                               (make-frame-command)
                               ))
