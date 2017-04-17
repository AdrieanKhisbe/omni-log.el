
(require 'color)
(require 'dash)


(defun color-gradient-name (start end step-number)
  (-map
   (lambda (rgb)
     (color-rgb-to-hex (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)))
   (color-gradient (color-name-to-rgb start) (color-name-to-rgb end) step-number)))

(color-gradient-name "#00ffff" "#0000ff" 100)
(color-gradient-name "#b2b2b2"  "#292b2e" 20)


(set-face-attribute 'region nil :background "#666")

(get-char-property (point) 'read-face-name)
(get-char-property (point) 'face)

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


(defun message-fading (msg)
  (interactive "s")
  (message "%s" (propertize msg 'face 'fringe))
  (-each-indexed (color-gradient-name "#b2b2b2"  "#292b2e" 20)
    (lambda (index color)
      (run-at-time index nil
                   (lambda (col)
                     (modify-face 'fringe col)
                     ) color)
      )))

  ;; §trigger the fadding

(message-fading "toto")
