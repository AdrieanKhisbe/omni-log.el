;;; omni-log-logger.el --- Logging utilities

;; Copyright (C) 2014-2017  Adrien Becchis
;; Created: 2014-07-27
;; Author: Adrien Becchis <adriean.khisbe@live.fr>
;; Keywords: convenience, languages, tools

;; This file is NOT part of GNU Emacs.

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

;;; Building Notes:
;; for now: marker, name, buffer properties
;; ¤maybe: give a try to EIEIO: http://www.gnu.org/software/emacs/manual/html_mono/eieio.html
;; ¤see: http://nic.ferrier.me.uk/blog/2012_07/tips-and-tricks-for-emacslisp

;;; Commentary:

;; Intern structure of a logger.

;;; Code:

;;; ¤> Constructor
(defun omni-log--make-logger (name &optional properties)
  "Build a loging buffer object with NAME and eventual PROPERTIES."
  ;; ¤maybe: precise this is intern function?
  ;; ¤maybe: build properties from keyword at this level?
  (let* ((buffer (get-buffer-create  (concat "*" name "*")))
        (logger (list 'logger name buffer properties)))
    (with-current-buffer buffer
      (read-only-mode)) ; §later: log major mode
    logger))

;;; ¤> reconnaisseur & type checkying method
(defun omni-log-logger-p (object) ; ¤note: maybe rename logp?
  "Return t if OBJECT is an omni-log buffer."
  ;; could be forged...[§maybe deeper check?]
  ;; §bonux: check buffer still alive? if notn maybe replace it?
  (and (consp object)
       (equal 'logger (car object))))

(defun omni-log-check-logger (logger)
  "Ensure That LOGGER is really one.  Throw exception otherwise.  Return the logger."
  (if (omni-log-logger-p logger)
      logger
    (signal 'wrong-type-argument '(omni-log-logger-p logger)))) ; ¤check message: maybe send `type-of'
;; "Provided buffer is not a log")

;; ¤note: maybe two way to call to log. registered name, or particular logbuffer. (this checkying function is for that,)

;;; ¤> accessor functions
(defun omni-log-logger-name (logger)
  "Get name of LOGGER."
  (nth 1 (omni-log-check-logger logger)))

(defun omni-log-logger-buffer (logger)
  "Get buffer of LOGGER."
  (nth 2 (omni-log-check-logger logger)))

(defun omni-log-logger-properties (logger)
  "Get properties of LOGGER."
  (nth 3 (omni-log-check-logger logger)))

;; §maybe: renaming function
;; ¤tmp: test (omni-log-logger-properties a)

;; §todo: wrapper avec une qui encaspule le nom
;; §todo: get-logger-or-create

;; ¤> saving fonctionnality
;; §later: (defcustom omni-log-default-saving-interval 5
;; §see: how to handle this. (determine when to save)
;; §this should go in the major mode to create! [create file.]
;; §??? §what: did i meant by saving functionality
(provide 'omni-log-logger)
;;; omni-log-logger.el ends here
