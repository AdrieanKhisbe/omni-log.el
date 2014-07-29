;;; omni-log-buffer.el --- Logging utilities

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
;; for now: marker, name, buffer properties

;;; Code:

(defun l-log-buffer-p (buffer)
  "Return t if buffer is an omni-log buffer."
  ;; could be forged...[§maybe deeper check?]
  (and (consp buffer)
       (equal 'log-buffer (car buffer))))

(defun l-make-log-buffer (name &optional properties)
  "Create a loging buffer NAME and eventual PROPERTIES."
  ;; §todo: test buffer does not yet exist
  (let* ((buffer (get-buffer-create name))
	(log-buffer  (list 'log-buffer name buffer properties)))
    log-buffer
    ;; §todo: ensure read-only
    ;; §todo: register it
    ))

;; §todo: function wrapper.

(defun l-check-log-buffer (log-buffer)
  "Ensure That LOG-BUFFER is really one.  Throw exception otherwise.  Return the log-buffer"
  (if (l-log-buffer-p log-buffer)
      log-buffer
    (signal 'wrong-type-argument '("Provided buffer is not a log-buffer")))
)
;; ¤note:maybe two way to call to log. registered name, or particular logbuffer. (this checkying function is for that,)


;; §todo: wrapper avec une qui encaspule le nom
;; §todo:d get-log-buffer-or-create



(provide 'omni-log-buffer)
;;; omni-log-buffer.el ends here
