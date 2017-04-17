
(require 'color)
(require 'dash)


(defun color-gradient-name (start end step-number)
  (let ((gradiant (-map
   (lambda (rgb)
     (color-rgb-to-hex (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)))
   (color-gradient (color-name-to-rgb start) (color-name-to-rgb end) step-number))))
    (-flatten (list start gradiant end))))

(color-gradient-name "#00ffff" "#0000ff" 100)
(color-gradient-name "#b2b2b2"  "#292b2e" 20)

(defface omni-log-face
  '((t (:inherit default
                 :weight bold :foreground "SteelBlue1")))
  "Face used for the first keyword of the tag")

(defface omni-log-fading-face
  '((t (:inherit omni-log-face
                 :foreground "SteelBlue1")))
  "Face used for the first keyword of the tag")


(defun message-fading (msg)
  (interactive "s")
  (modify-face 'omni-log-fading-face ; reset color
               (face-attribute 'omni-log-face :foreground nil t))
  (let ((timestamp (float-time)))
    (message "%s" (propertize msg 'face 'omni-log-fading-face 'log-p t
                            'timestamp timestamp))
    (-each-indexed (color-gradient-name (face-attribute 'omni-log-fading-face :foreground nil t)
                                        (face-attribute 'omni-log-fading-face :background nil t)
                                        20)
      (lambda (index color)
        (run-at-time index nil
                     (lambda (col timestamp)
                       (let ((cm (current-message)))
                         (if (and cm
                                  (get-text-property 0 'log-p cm)
                                  (equal timestamp (get-text-property 0 'timestamp cm)))
                             (modify-face 'omni-log-fading-face col))))
                     color timestamp)
        ))))

  ;; §trigger the fadding

(message-fading "toto")



;; ¤note: clignotant effet en lancant les deux en décalés.
