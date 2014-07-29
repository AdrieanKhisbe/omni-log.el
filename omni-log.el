;;; omni-log.el --- Logging utilities

;; Copyright (C) 2014  Adrien Becchis
;; Created:  2014-07-27
;; Version: 0.1

;; Author: Adrien Becchis <adriean.khisbe@live.fr>
;; Keywords: convenience, languages, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Building Notes:
;; far too early [and pretentious] to call the 'the long lost logging api' ^^

;;; Code:

(require 'omni-log-buffer)

(defun l-message-no-log (message) ; ¤maybe: rest version (would have to splat it)
  "Print a message in the loggin area without recording it in the *Messages* buffer."
  ;; inspired from eldoc
  ;; §maybe: make a non internal function to reuse as internal
  (interactive)
  (let ((message-log-max nil))
    (message message)))


;; §then color. (highligh/bold: ou plus `emphasize')

;; insert color in message: log-message-with-color

;; then specific buffer.

;; §see: level of checying log buffer

(defun l-message-to-log-buffer (buffer message)
  ;; §later: evaluate message content now. and enable multi format (format style)
  (l-append-to-log-buffer (l-check-log-buffer buffer) message)
  (l-message-no-log message)
  ;;  message ; ¤see if giving message as return value?
)

;; §maybe:
(defun l-append-to-log-buffer (buffer message)
  ;; §here
  (message "NO Buffer for now")
  )

;; ¤test:
;;(setq a (l-make-log-buffer "*youo*"))
;; (l-message-to-log-buffer a "toto")


;; §see: proposer config avec aliasing des fonctions dans namespace, et advice de message?

(provide 'omni-log)
;;; omni-log.el ends here
