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
    ;; ¤note: centering, or right alignment should happen here.
    ;;        but choice to do so is responsability of the buffer.
    ;;        ¤maybe create another no log padding,whatever, and message-to-log-buffer would dispacth
    ;;        [object oriented programming where are you when we need you?]
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

(defun l-append-to-log-buffer (buffer message)
  (with-current-buffer (l-log-buffer-buffer buffer)
      ;; §maybe: create a with-current-log-buffer
    (goto-char (point-max)) ;; ¤note: maybe use some mark if the bottom of the buffer us some text or so
    (insert message) ;; §todo: call to special formater on message: add timestamp (maybe calling function? (if can be retrieved from namespace))
    ;; ¤note: message is supposed to be already formated. (-> color empahsize inside should be already done)
    (insert "\n")
      ))

;; ¤test:
(setq test-buffer (l-make-log-buffer "*ansible*"))
(l-message-to-log-buffer test-buffer (propertize "42" 'face 'font-lock-warning-face ))

;; §idea: add padding, centering functionnality. ¤maybe regroup in some class with all the other formating fonctionnality: color. etc
;; l-apply-font
;; ¤see: specific font

;; access to echo area with (get-buffer " *Echo Area 0*")
;; modif with setq-local.
;; ¤note: get size of echo area with:
;; (window-total-width (get-buffer-window  (get-buffer "*Echo Area 0*")))


;; §see: proposer config avec aliasing des fonctions dans namespace, et advice de message?

(provide 'omni-log)
;;; omni-log.el ends here
