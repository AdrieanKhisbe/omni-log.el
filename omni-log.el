;;; omni-log.el --- Logging utilities  -*- lexical-binding: t -*-

;; Copyright (C) 2014-2017  Adrien Becchis

;; Author: Adrien Becchis <adriean.khisbe@live.fr>
;; Created:  2014-07-27
;; Version: 0.1.2
;; Package-Requires: ((emacs "24") (ht "2.0") (s "1.6.1") (dash "1.8.0") )
;; Url: https://github.com/AdrieanKhisbe/omni-log.el
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

;; far too early [and pretentious] to call it `l' the 'the long lost logging api' ^^

;; §IMP: DETERMINE EXTERIOR API YOU WANNA, and INDIVIDUAL COMPONENTS!!

;;; Commentary:

;; Logging Utilities for packages.
;; Offer function to log messages to dedicated buffers

;;; Code:

(require 'dash)
(require 's)
(require 'ht)
(require 'omni-log-logger)

(defvar omni-log-logger-index (ht) ; §maybe create Message equivalent? ¤maybe: alist
  "Logger hash containing associating between name and logger.")

(defun omni-log-quiet-message (message) ; ¤maybe: rest version (would have to splat it)
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

(defun omni-log-logger (logger-or-name)
  "Return logger from LOGGER-OR-NAME or nil if non existing."
  (if (omni-log-logger-p logger-or-name)
      logger-or-name
    (omni-log-get-logger logger-or-name)))

(defun omni-log-get-logger (name)
  "Send back the eventual buffer with specified NAME."
  (ht-get omni-log-logger-index name nil))


(defun omni-log-create-logger (name &optional filename)
  "Create and return a logger with given NAME.

The logger is both registered and returned to be eventually
asigned to a variable.  An optional FILENAME to which log will
be outputed can be provided too.

Warning will be issued if a logger with same NAME already exists."
  ;; ¤idea: keyword to signal intensity-> omni-log-logger-/name/ :info "blable"

  ;; §maybe: anonym logger?
  ;; §otherParam: filename, saving frequenci, etc.
  ;; §keywordp?
  ;; §maybe: create holding var and functions? [maybe at a higher level?]
  (interactive "sName of the logger: ")
  ;; §todo: sanitize name?
  ;; §todo: then check no name conflict
  (if (omni-log-get-logger name)
      (message "A logger named %s already exists" name)
      (let ((logger (omni-log--make-logger name `(filename ,filename))))
        (ht-set! omni-log-logger-index name logger)
        logger)))

(defun omni-log-kill-logger (logger-or-name &optional archive)
  "Kill LOGGER-OR-NAME.  If ARCHIVE ask, the buffer will be renamed (and returned)."
  ;; §todo: kill attached logging methods
  (let ((logger (omni-log-logger logger-or-name)))
    (unless logger (signal 'wrong-type-argument '(omni-log-logger-p logger)))
    (let ((name (omni-log-logger-name logger)))
      (ht-remove! omni-log-logger-index name)
      ;; remove logging function (whether it has bee create of not)
      (fmakunbound (intern (concat "log-" name)))
      (if archive
          (with-current-buffer (omni-log-logger-buffer logger)
            (rename-buffer (format "%s-old" name) t))
        (kill-buffer (omni-log-logger-buffer logger))))))


(defun omni-log-create-log-function (logger)
  ;; §todo: add possibility to bypass logger name?
  "Create a function to directly append to LOGGER the given message.
This function would be named `log-' followed by logger name"
  ;;§todo: check not set!
  (let ((name (intern (concat "log-" (omni-log-logger-name logger)))))
    (if (fboundp name)
        (warn "%s logging function has already been made!" name)
      (omni-log--make-log-function name logger)))
    ;; §todo: save it in the log obxject
  )

;; §check lexical biniding
(defun omni-log--make-log-function (function-name logger)
  "Create the logging FUNCTION-NAME attached to the given LOGGER.

This is not intended for users."
  ;; ¤note: beware macro name conflict: var name must be different from the one used in log.
  (defalias function-name
    (function (lambda (message)
                (format "Log given MESSAGE to the %s logger" (omni-log-logger-name logger))
                (interactive)
                (omni-log-message-to-logger logger message)))))
;;¤note: maybe subst?
;;; ¤note: inlined, without check

;; §maybe: omni-log-log to current. -> set current or latest register?

(defun log (logger-or-name message)
  "Log to specified LOGGER-OR-NAME given MESSAGE.
LOGGER-OR-NAME is either a logger or the name of the existing logger"
  ;; §maybe: optional log-or-name. would use default if not set
  ;; §FIXME Refactor after renaming
    (let ((logger (omni-log-logger logger-or-name)))
      (if logger
          (omni-log-message-to-logger logger message)
        (warn "There is no logger of name %s." logger-or-name))))

(defun omni-log-message-to-logger (logger message)
  "Add to LOGGER  MESSAGE and display it in the Echo area."
  ;; §later: evaluate message content now. and enable multi format (format style)
  (omni-log--append-to-logger (omni-log-check-logger logger) message)
  (omni-log-quiet-message message) ; ¤note: maybe subst?
  ;;  message ; ¤see if giving message as return value? [latter when evaluation occur inside? &rest]
  )

(defun omni-log--append-to-logger (logger message)
  "Append to LOGGER given MESSAGE."
  ;; ¤note: type checking supposed to be done at a higher level
  (with-current-buffer (omni-log-logger-buffer logger)
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
;; omni-log-apply-font
;; ¤see: specific font

;; ¤note: access to echo area with (get-buffer " *Echo Area 0*")
;; modif with setq-local.
;;  get size of echo area with:
;; (window-total-width (get-buffer-window  (get-buffer "*Echo Area 0*")))

;; §note: not accessible with C-x b

;; §see: proposer config avec aliasing des fonctions dans namespace, et advice de message?

(provide 'omni-log)
;;; omni-log.el ends here
