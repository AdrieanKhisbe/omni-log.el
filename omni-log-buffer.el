;;; omni-log-buffer.el --- Logging utilities

;; Copyright (C) 2014-2015  Adrien Becchis
;; Created:  2014-07-27
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

;;; Commentary:

;; Intern structure of a logger.

;;; Building Notes:
;; for now: marker, name, buffer properties
;; ¤maybe: give a try to EIEIO: http://www.gnu.org/software/emacs/manual/html_mono/eieio.html
;; ¤see: http://nic.ferrier.me.uk/blog/2012_07/tips-and-tricks-for-emacslisp

;;; Code:

;; ¤maybe also rename intern to logger.
;;; ¤> Constructor
(defun l--make-log (name &optional properties)
  "Build a loging buffer object with NAME and eventual PROPERTIES."
  ;; ¤maybe: precise this is intern function?
  ;; ¤maybe: build properties from keyword at this level?
  (let* ((buffer (get-buffer-create  (concat "*" name "*")))
        (log (list 'log name buffer properties)))
    (with-current-buffer buffer
      (read-only-mode)) ; §later: log major mode
    log))

;;; ¤> reconnaisseur & type checkying method

(defun l-log-p (buffer) ; ¤note: maybe rename logp?
  "Return t if BUFFER is an omni-log buffer."
  ;; could be forged...[§maybe deeper check?]
  ;; §bonux: check buffer still alive? if notn maybe replace it?
  (and (consp buffer)
       (equal 'log (car buffer))))

(defun l-check-log (log)
  "Ensure That LOG is really one.  Throw exception otherwise.  Return the log."
  (if (l-log-p log)
      log
    (signal 'wrong-type-argument '(l-log-p log)))) ; ¤check message: maybe send `type-of'
;; "Provided buffer is not a log")

;; ¤note:maybe two way to call to log. registered name, or particular logbuffer. (this checkying function is for that,)

;;; ¤> accessor functions
(defun l-log-name (log)
  "Get name of LOG."
  (cadr (l-check-log log)))
;; ¤todo: cadr said to be coming from the `cl' package. (flycheck warning)
;; maybe use `nth' instead?

(defun l-log-buffer (log)
  "Get buffer of LOG."
  (caddr (l-check-log log)))

(defun l-log-properties (log)
  "Get properties of LOG."
  (cadddr (l-check-log log)))

;; §maybe: renaming function
;; ¤tmp: test (l-log-properties a)

;; §todo: wrapper avec une qui encaspule le nom
;; §todo: get-log-or-create

;; ¤> saving fonctionnality
;; §later: (defcustom l-default-saving-interval 5
;; §see: how to handle this. (determine when to save)
;; §this shouuld go in the major mode to create! [create file.]

(provide 'omni-log-buffer)
;;; omni-log-buffer.el ends here
