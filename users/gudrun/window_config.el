(defun nl/monitor-pixel-dimensions (monitor)
  (let* ((monitor-attributes (display-monitor-attributes-list))
         (num-displays (length (display-monitor-attributes-list))))
    (if (< monitor num-displays)
        (list (nth 3 (assq 'geometry (nth monitor monitor-attributes)))
              (nth 4 (assq 'geometry (nth monitor monitor-attributes))))
      (error "invalid monitor number: %d" monitor))))

(defun nl/monitor-pixel-width (monitor)
  (car (nl/monitor-pixel-dimensions monitor)))

(defun nl/monitor-pixel-height (monitor)
  (nth 1 (nl/monitor-pixel-dimensions monitor)))

(defun nl/main-frame-set-size-and-position ()
  "Set the size and position of the Emacs window."
  (interactive)
  (let* ((frame (selected-frame))
         (num-displays (length (display-monitor-attributes-list)))
         (desired-width-in-chars 235)
         (desired-width-in-pixels (* desired-width-in-chars (frame-char-width))))
    (set-face-attribute 'default frame :font (nl/gui-fixed-font-normal))
    (cond
     ((eq num-displays 2)
      (set-frame-size frame
                      desired-width-in-chars
                      (/ (nl/monitor-pixel-height 1) (frame-char-height)))
      ;; cannot use negative value for X since it is not placed in correct location on startup
      (set-frame-position frame
                          (- (+ (nl/monitor-pixel-width 0) (nl/monitor-pixel-width 1))
                             desired-width-in-pixels)
                          0))
     ((eq num-displays 3)
      (let* ((window-frame-in-pixels 16))
        ;; cannot use negative value for X since it is not placed in correct location on startup
        (set-frame-position frame
                            (- (+ (nl/monitor-pixel-width 0) (nl/monitor-pixel-width 1))
                               window-frame-in-pixels
                               desired-width-in-pixels)
                            0)
        (set-frame-size frame
                        desired-width-in-chars
                        (/ (nl/monitor-pixel-height 1) (frame-char-height))))))))

(defun nl/new-frame ()
  "Create a new frame on the 4k display."
  (interactive)
  (let* ((frame (make-frame)))
    (select-frame-set-input-focus frame)
    (switch-to-buffer (current-buffer))
    (set-frame-position frame (+ (nl/monitor-pixel-width 1) 400) 120)
    (set-face-attribute 'default frame :font (nl/gui-fixed-font-normal))
    (set-frame-size frame
                    (* 120 (frame-char-width frame))
                    (* 70 (frame-char-height frame))
                    t)))

(defun nl/frame-2nd-display ()
  "Create a new frame on the 3rd display."
  (interactive)
  (let ((frame (make-frame))
        (monitor-pixel-width (nth 3 (assq 'geometry (nth 0 (display-monitor-attributes-list)))))
        (monitor-pixel-height (nth 4 (assq 'geometry (nth 0 (display-monitor-attributes-list))))))
    (select-frame-set-input-focus frame)
    (set-face-attribute 'default frame :font (nl/gui-fixed-font-normal))
    (switch-to-buffer "*scratch*")
    (set-frame-position frame
                        (/ (nl/monitor-pixel-width 0) 2)
                        0)
    (set-frame-size frame
                    (/ (/ (nl/monitor-pixel-width 0) (frame-char-width frame)) 2)
                    (floor (* (/ (nl/monitor-pixel-height 0) (frame-char-height frame)) 0.97)))))

(defun nl/vterm-frame ()
  "Create a new frame, running vterm, at a specific location on the 4k display."
  (interactive)
  (let* ((frame (make-frame)))
    (select-frame-set-input-focus frame)
    (switch-to-buffer (current-buffer))
    (set-frame-position frame (+ (nl/monitor-pixel-width 1) 0) 0)
    (set-face-attribute 'default frame :font (nl/gui-fixed-font-normal))
    (set-frame-size frame
                    (* 146 (frame-char-width frame))
                    (* 54 (frame-char-height frame))
                    t)
    (funcall #'vterm)))

(defun nl/change-font-size (fixed-font-size variable-font-size)
  "Change font size for all buffers."
  (interactive)
  (setq nl/gui-current-fixed-font-size fixed-font-size
        nl/gui-current-variable-font-size variable-font-size)
  (let* ((frame (selected-frame))
         (fixed-font (nl/gui-font nl/gui-fixed-font-name fixed-font-size))
         (variable-font (nl/gui-font nl/gui-variable-font-name variable-font-size)))

    (set-face-attribute 'default frame :font fixed-font)
    (set-face-font 'default fixed-font)
    (set-face-font 'italic variable-font)
    (set-face-font 'bold-italic variable-font)
    (set-face-font 'fixed-pitch-serif variable-font)
    (set-face-font 'variable-pitch variable-font)

    (nl/org-mode-faces (* 10 (string-to-number fixed-font-size)))))

(defun nl/zoom-config ()
  "Set a large enough font size for all Emacs frames for screensharing on Zoom meetings."
  (interactive)
  (nl/change-font-size "16" "18"))

(defun nl/normal-config ()
  "Set the size and position of the Emacs window."
  (interactive)
  (nl/change-font-size nl/gui-fixed-font-size-normal nl/gui-variable-font-size-normal)
  (nl/main-frame-set-size-and-position))

(defun nl/window-setup-hook ()
  (let ((frame (selected-frame)))
    ;; (nl/main-frame-set-size-and-position)
    ;; (resume)
    (select-frame-set-input-focus frame)))

(add-hook 'window-setup-hook 'nl/window-setup-hook)

;; Search specified tree for the item with the specified key
;; http://stackoverflow.com/questions/11912027/emacs-lisp-search-anything in-a-nested-list
(defun tree-assoc (key tree)
  (when (consp tree)
    (cl-destructuring-bind (x . y) tree
  (if (eql x key) tree
    (or (tree-assoc key x) (tree-assoc key y))))))

;; adjust the font size based on the monitor's pixel height
;; http://arnab-deka.com/posts/2012/09/emacs-change-fonts-dynamically-based-on-screen-resolution
(defun fontify-frame (&optional frame)
  (interactive)
  (or frame (setq frame (selected-frame)))
  (if window-system
      (let ((monitor-pixel-height (nth 4 (assq 'geometry (frame-monitor-attributes frame))))
            (fixed-font (nl/gui-font nl/gui-fixed-font-name nl/default-font-size))
         (variable-font (nl/gui-font nl/gui-variable-font-name nl/default-variable-font-size)))
        (if (<= monitor-pixel-height 1080)
            (set-frame-parameter 'nil 'font fixed-font)
          (set-frame-parameter 'nil 'font fixed-font)))))

;;(push
;; 'fontify-frame after-make-frame-functions)

;;(run-with-idle-timer 2 t 'fontify-frame 'nil)

;; 0      1         2         3         4         5         6        7          8         9         0
;; 456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789
