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

;; ¤maybe: give a try to EIEIO: http://www.gnu.org/software/emacs/manual/html_mono/eieio.html
;; ¤see: http://nic.ferrier.me.uk/blog/2012_07/tips-and-tricks-for-emacslisp

;; ¤maybe also rename intern to ogger.
;;; ¤> Constructor
(defun l-make-log-buffer (name &optional properties)
  "Create a loging buffer NAME and eventual PROPERTIES."
  ;; §TODO! c'est le buffer qui est wrappé des quotes. §here. [éventuellement option pour bypass]

  ;; §todo: test buffer does not yet exist
  ;; ¤maybe: precise this is intern function?
  ;; ¤maybe: build properties from keyword at this level?
  (let* ((buffer (get-buffer-create  (concat "*" name "*")))
	(log-buffer  (list 'log-buffer name buffer properties)))
    log-buffer
    ;; §todo: ensure read-only -> make a log-buffer major mode
    ;; §todo: register it to list of lo-buffer.
    ))

;; §todo: function wrapper. ¤? what did I meant?



;;; ¤> reconnaisseur & type checkying method

(defun l-log-buffer-p (buffer)
  "Return t if buffer is an omni-log buffer."
  ;; could be forged...[§maybe deeper check?]
  ;; §bonux: check buffer still alive? if notn maybe replace it?
  (and (consp buffer)
       (equal 'log-buffer (car buffer))))

(defun l-check-log-buffer (log-buffer)
  "Ensure That LOG-BUFFER is really one.  Throw exception otherwise.  Return the log-buffer"
  (if (l-log-buffer-p log-buffer)
      log-buffer
    (signal 'wrong-type-argument '(l-log-buffer-p log-buffer))

    ;; "Provided buffer is not a log-buffer")
    ))

;; ¤note:maybe two way to call to log. registered name, or particular logbuffer. (this checkying function is for that,)


;;; ¤> accessor functions
(defun l-log-buffer-name (log-buffer)
  "Get name of LOG-BUFFER"
  (cadr (l-check-log-buffer log-buffer))) ;; ¤see: cadr said to be comming from the `cl' package. (flycheck warning); myabe use ` nth' instead?

(defun l-log-buffer-buffer (log-buffer)
  "Get buffer of LOG-BUFFER"
  (caddr (l-check-log-buffer log-buffer)))

(defun l-log-buffer-properties (log-buffer)
  "Get properties of LOG-BUFFER"
  (cadddr (l-check-log-buffer log-buffer)))

;; §maybe: renaming function
;; ¤tmp:test (l-log-buffer-properties a)


;; §todo: wrapper avec une qui encaspule le nom
;; §todo:d get-log-buffer-or-create


;; ¤> saving fonctionnality
;; §later: (defcustom l-default-saving-interval 5
;; §see: how to handle this. (determine when to save)
;; §this shouuld go in the major mode to create! [create file.]

(provide 'omni-log-buffer)
;;; omni-log-buffer.el ends here
