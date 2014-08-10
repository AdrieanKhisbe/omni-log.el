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
;; §todo: see terminology
;; find name to differentiate make/create [¤so rename this and create function]

;;; Code:

(require 'ht) ;§maybe start with alist not to have dependancy
(require 'omni-log-buffer)

(defun l-message-no-log (message) ; ¤maybe: rest version (would have to splat it)
  "Print a MESSAGE in the loggin area without recording it in the *Messages* buffer."
  ;; inspired from eldoc
  (let ((message-log-max nil))
    ;; ¤note: centering, or right alignment should happen here.
    ;;        but choice to do so is responsability of the buffer.
    ;;        ¤maybe create another no log padding,whatever, and message-to-log-buffer would dispacth
    ;;        [object oriented programming where are you when we need you?]
    (message message)))

;; §then color. (highligh/bold: ou plus `emphasize')

;; insert color in message: log-message-with-color

(defvar l-logger-hash (ht) ;§maybe create Message equivalent?
  "Logger hash containing associating between name and logger.")

;;; ¤idea: keyword to signal intensity-> l-log-/name/ :info "blable"
(defun l-create-logger (name &optional filename)
  "Create and return a logger with given NAME
The logger is both registered and returned to be eventually asigned to a variable.
Warning will be issued if a logger with same NAME already exists"
  ;;§other param: filename, saving frequenci, etc.
  ;; §keywordp
  ;; §maybe: create holding var and functions? [maybe at a higher level?]
  (interactive "sName of the logger: ")
  ;;§todosanitze name?
  ;; §todo:then check no name conflict
  (if (l-get-logger name)
      (let ((log-buffer (l--make-log-buffer name `(filename ,filename))))
	(ht-set! l-logger-hash name log-buffer)
	log-buffer)
    (warn "A logger named %s already exists")))

(defun l-get-logger (name)
  "Send back the eventual buffer with specified NAME"
  (ht-get l-logger-hash name))

(defun l-create-logging-function (logger)
  "Create a function to directly append to LOGGER the given message.
This function would be named log- followed by logger name"
  ;; §for now unique
  (if  (l-log-buffer-p logger)
       ;;§todo: check not set!
      (let ((name (concat "log-" (l-log-buffer-name  logger))))
	(if (fboundp (intern name))
	    (warn "%s logging function has already been made!" name)
	  (l--make-logging-function logger name)))
    (warn "%s is not a logger!" logger)))

(defmacro l--make-logging-function (log-buffer fname)
  "Macro to create the logging function attached to a LOG-BUFFER.
This is not inteded for users."
  ;; ¤note: beware macro name conflict
  `(defun ,(intern (eval fname))
     (message) ;§todo:doc
     (interactive)
     (l-message-to-logger ',(symbol-value log-buffer) message)))

;; §maybe? l-log to current. -> set current or latest register?


(defun log (logger-or-name message)
  "Log given MESSAGE to specified LOGGER-OR-NAME.
LOGGER-OR-NAME is either a log-buffer or the name of the existing log-buffer"
  (if (l-log-buffer-p logger-or-name)
      (l-message-to-logger logger-or-name message)
    (let ((logger (l-get-logger logger-or-name)))
      (if logger
	  (l-message-to-logger logger message)
	(warn "There is no logger of name %s." logger-or-name)))))

(defun l-message-to-logger (logger message)
  "Display MESSAGE to the Echo area and append it the given LOGGER"
  ;; §later: evaluate message content now. and enable multi format (format style)
  (l-append-to-logger (l-check-log-buffer logger) message)
  (l-message-no-log message)
  ;;  message ; ¤see if giving message as return value?
  )

(defun l-append-to-logger (logger message)
  "Add MESSAGE to specified LOGGER."
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

;; ¤note: access to echo area with (get-buffer " *Echo Area 0*")
;; modif with setq-local.
;;  get size of echo area with:
;; (window-total-width (get-buffer-window  (get-buffer "*Echo Area 0*")))


;; §see: proposer config avec aliasing des fonctions dans namespace, et advice de message?

(provide 'omni-log)
;;; omni-log.el ends here
