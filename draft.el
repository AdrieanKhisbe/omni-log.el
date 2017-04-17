
(require 'color)
(require 'dash)


(defun color-gradient-name (start end step-number)
  (-map
   (lambda (rgb)
     (color-rgb-to-hex (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)))
   (color-gradient (color-name-to-rgb start) (color-name-to-rgb end) step-number)))

(color-gradient-name "#00ffff" "#0000ff" 100)
(color-gradient-name "#b2b2b2"  "#292b2e" 20)
