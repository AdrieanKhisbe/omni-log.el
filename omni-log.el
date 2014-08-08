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
;; §IMP: DETERMINE EXTERIOR API YOU WANNA, and INDIVIDUAL COMPONENTS!!

;;; Code:

(require 'ht) ;§maybe start with alist not to have dependancy
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


;; §see: level of checking log buffer. (si endessous niveau courrant insérer dans buffer mais pas afficher?)
;;        high level check. for input for user, extrenal api.


(defvar l-logger-hash (ht) ;§maybe create Message equivalent?
  "Logger hash containing associating between name and logger.")

;;; ¤mmmmm: refactor: a logger should be a fonction, that will call the append-to-log buffer, : différentier non?
;; with some captured option (leading string, where it come from, and so on.)
;; keyword to signal intensity-> l-log-/name/ :info "blable"
;; find name to differentiate this [¤so rename this and create function]
(defun l-create-logger (name &optional filename)
  "DOCSTRING §TODO"
  ;;§other param: filename, saving frequenci, etc.
  ;; ¤see if retrive them just as name
  ;; §keywordp

  ;; §maybe: create holding var and functions?
  ;; fset to set a variable!!
  (interactive "sName of the logger: ")
  ;;§todosanitze name?
  ;; §todo:then check no name conflict
  (let ((log-buffer (l-make-log-buffer name `(filename ,filename))))
    ;; §maybe register it to variable? name-logger??
    ;; also send it back
     (ht-set! l-logger-hash name log-buffer)
     log-buffer))

;; §todo: make also a get function
(defun l-create-logging-function (logger)
  ;; §for now unique
  ;;§why macro?
  (if  (l-log-buffer-p logger)
       ;;§todo: check not set!
      (l--make-logging-function logger) ;§see how to get symbol
    (warn "%s is not a logger!"  logger)))

(defmacro l--make-logging-function (logger)
;; §todo: warn should not be used
  (let ((name (intern (concat "log-" (l-log-buffer-name (eval logger))))))
    `(defun ,name
       (message) ;§todo:doc
       (interactive)
       (l-message-to-logger ,logger message))))
;; §maybe had warning if already defined
;; fboundp check!!
 ;; §see: why ad to creat two?

;; §maybe? l-log to current. -> set current or latest register?

;; §later: to logger by name?
(defun log (logger-or-name message)
  ;; §doc
  (if (l-log-buffer-p logger-or-name)
      (l-message-to-logger logger-or-name message)
    (let ((logger (ht-get l-logger-hash logger-or-name)))
      (if logger (l-message-to-logger logger message)
	(warn "There is no logger of name %s." logger-or-name)))))

(defun l-message-to-logger (logger message)
  ;; §later: evaluate message content now. and enable multi format (format style)
  (l-append-to-logger (l-check-log-buffer logger) message)
  (l-message-no-log message)
  ;;  message ; ¤see if giving message as return value?
  )

(defun l-append-to-logger (logger message)
  (with-current-buffer (l-log-buffer-buffer logger)
      ;; §maybe: create a with-current-log-buffer
    (goto-char (point-max)) ;; ¤note: maybe use some mark if the bottom of the buffer us some text or so
    (insert message) ;; §todo: call to special formater on message: add timestamp (maybe calling function? (if can be retrieved from namespace))
    ;; ¤note: message is supposed to be already formated. (-> color empahsize inside should be already done)
    (insert "\n")
      ))

;; ¤test:
(setq test-logger (l-create-logger "ansible"))
(l-message-to-logger test-logger (propertize "42" 'face 'font-lock-warning-face ))


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
