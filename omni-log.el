;;; omni-log.el --- Logging utilities

;; Copyright (C) 2014-2015  Adrien Becchis
;; Created:  2014-07-27
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (ht "2.0") (s "1.6.1") (dash "1.8.0") )
;; URL: https://github.com/AdrieanKhisbe/omni-log.el
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

;; Logging Utilities for packages.
;; Offer function to log messages to dedicated buffers

;;; Building Notes:

;; far too early [and pretentious] to call the 'the long lost logging api' ^^

;; §IMP: DETERMINE EXTERIOR API YOU WANNA, and INDIVIDUAL COMPONENTS!!

;;; Code:

(require 'dash)
(require 's)
(require 'ht)
(require 'omni-log-logger)

(defvar l-logger-index (ht) ; §maybe create Message equivalent? ¤maybe: alist
  "Logger hash containing associating between name and logger.")

(defun l-quiet-message (message) ; ¤maybe: rest version (would have to splat it)
  "Print a MESSAGE in the loggin area without recording it in the *Messages* buffer."
  ;; inspired from eldoc
  (let ((message-log-max nil))
    ;; ¤note: centering, or right alignment should happen here.
    ;;        but choice to do so is responsability of the buffer.
    ;;        ¤maybe create another no log padding,whatever, and message-to-log would dispacth
    ;;        [object oriented programming where are you when we need you?]
    (message message)))

;; §then color. (highligh/bold: ou plus `emphasize')
;; insert color in message: log-message-with-color

(defun l-logger (logger-or-name)
  "Return logger from LOGGER-OR-NAME or nil if non existing."
  (if (l-logger-p logger-or-name)
      logger-or-name
    (l-get-logger logger-or-name)))

(defun l-get-logger (name)
  "Send back the eventual buffer with specified NAME."
  (ht-get l-logger-index name nil))


(defun l-create-logger (name &optional filename)
  "Create and return a logger with given NAME.

The logger is both registered and returned to be eventually
asigned to a variable.  An optional FILENAME to which log will
be outputed can be provided too.

Warning will be issued if a logger with same NAME already exists."
  ;; ¤idea: keyword to signal intensity-> l-logger-/name/ :info "blable"

  ;; §maybe: anonym logger?
  ;; §otherParam: filename, saving frequenci, etc.
  ;; §keywordp?
  ;; §maybe: create holding var and functions? [maybe at a higher level?]
  (interactive "sName of the logger: ")
  ;; §todo: sanitize name?
  ;; §todo: then check no name conflict
  (if (l-get-logger name)
      (message "A logger named %s already exists" name)
      (let ((logger (l--make-logger name `(filename ,filename))))
        (ht-set! l-logger-index name logger)
        logger)))

(defun l-kill-logger (logger-or-name &optional archive)
  "Kill LOGGER-OR-NAME.  If ARCHIVE ask, the buffer will be renamed (and returned)."
  ;; §todo: kill attached logging methods
  (let ((logger (l-logger logger-or-name)))
    (unless logger (signal 'wrong-type-argument '(l-logger-p logger)))
    (let ((name (l-logger-name logger)))
      (ht-remove! l-logger-index name)
      ;; remove logging function (whether it has bee create of not)
      (fmakunbound (intern (concat "log-" name)))
      (if archive
          (with-current-buffer (l-logger-buffer logger)
            (rename-buffer (format "%s-old" name) t))
        (kill-buffer (l-logger-buffer logger))))))


(defun l-create-log-function (logger)
  ;; §todo: add possibility to bypass logger name?
  "Create a function to directly append to LOGGER the given message.
This function would be named `log-' followed by logger name"
  ;;§todo: check not set!
  (let ((name (concat "log-" (l-logger-name logger))))
    (if (fboundp (intern name))
        (warn "%s logging function has already been made!" name)
      (l--make-log-function name logger)))
    ;; §todo: save it in the log object
  )

(defmacro l--make-log-function (function-name logger-)
  "Macro to create the logging FUNCTION-NAME attached to the given LOGGER-.

This is not intended for users."
  ;; ¤note: beware macro name conflict: var name must be different from the one used in log.
  `(defun ,(intern (eval function-name)) (message)
     ,(format "Log given MESSAGE to the %s logger" (l-logger-name (eval logger-)))
     (interactive)
     (l--append-to-logger ',(symbol-value logger-) message)
     (l-quiet-message message))) ; ¤note: maybe subst?
;;; ¤note: inlined, without check

;; §maybe: l-log to current. -> set current or latest register?

(defun log (logger-or-name message)
  "Log to specified LOGGER-OR-NAME given MESSAGE.
LOGGER-OR-NAME is either a logger or the name of the existing logger"
  ;; §maybe: optional log-or-name. would use default if not set
  ;; §FIXME Refactor after renaming
    (let ((logger (l-logger logger-or-name)))
      (if logger
          (l-message-to-logger logger message)
        (warn "There is no logger of name %s." logger-or-name))))

(defun l-message-to-logger (logger message)
  "Add to LOGGER  MESSAGE and display it in the Echo area."
  ;; §later: evaluate message content now. and enable multi format (format style)
  (l--append-to-logger (l-check-logger logger) message)
  (l-quiet-message message) ; ¤note: maybe subst?
  ;;  message ; ¤see if giving message as return value? [latter when evaluation occur inside? &rest]
  )

(defun l--append-to-logger (logger message)
  "Append to LOGGER given MESSAGE."
  ;; ¤note: type checking supposed to be done at a higher level
  (with-current-buffer (l-logger-buffer logger)
    ;; §maybe: create a with-current-logger
    (goto-char (point-max)) ;; ¤note: maybe use some mark if the bottom of the buffer us some text or so
    (let ((inhibit-read-only t))
      (insert message)
      ;; §todo: call to special formater on message: add timestamp (maybe calling function? (if can be retrieved from namespace))
      ;; ¤note: message is supposed to be already formated. (-> color empahsize inside should be already done)
    (newline))))

;; §todo: maybe wmessage + qmessage (or t transient)

;; §idea: add padding, centering functionnality.
;; ¤maybe regroup in some class with all the other formating fonctionnality: color. etc
;; l-apply-font
;; ¤see: specific font

;; ¤note: access to echo area with (get-buffer " *Echo Area 0*")
;; modif with setq-local.
;;  get size of echo area with:
;; (window-total-width (get-buffer-window  (get-buffer "*Echo Area 0*")))

;; §note: not accessible with C-x b

;; §see: proposer config avec aliasing des fonctions dans namespace, et advice de message?

(provide 'omni-log)
;;; omni-log.el ends here
