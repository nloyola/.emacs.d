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
    (switch-to-buffer (current-buffer))
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

(setq default-font "Input Mono Condensed-14")
(set-frame-font default-font)

;; Search specified tree for the item with the specified key
;; http://stackoverflow.com/questions/11912027/emacs-lisp-search-anything in-a-nested-list
(defun tree-assoc (key tree)
  (when (consp tree)
    (destructuring-bind (x . y) tree
  (if (eql x key) tree
    (or (tree-assoc key x) (tree-assoc key y))))))

;; adjust the font size based on the dimensions (mm) of the monitor.
;; http://arnab-deka.com/posts/2012/09/emacs-change-fonts-dynamically-based-on-screen-resolution
(defun fontify-frame (frame)
  (interactive)
  (if window-system
  (let ((vert-mm (nth 2 (tree-assoc 'mm-size (frame-monitor-attributes (selected-frame))))))
    (if (< vert-mm 300)
        (set-frame-parameter 'nil 'font "Input Mono Condensed-9")
      (set-frame-parameter 'nil 'font "Input Mono Condensed-14")))))

(push
 'fontify-frame after-make-frame-functions)

;;(run-with-idle-timer 2 t 'fontify-frame 'nil)

;; 0      1         2         3         4         5         6        7          8         9         0
;; 456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789
