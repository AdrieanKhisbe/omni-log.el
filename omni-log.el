;;; omni-log.el --- Logging utilities

;; Copyright (C) 2014  Adrien Becchis
;; Created:  2014-07-27
;; Version: 0.1
;; §todo: URL

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

;; §todo

;;; Building Notes:
;; far too early [and pretentious] to call the 'the long lost logging api' ^^


;; §IMP: DETERMINE EXTERIOR API YOU WANNA, and INDIVIDUAL COMPONENTS!!
;; §todo: see terminology
;; find name to differentiate make/create [¤so rename this and create function]
;; §TODO: make or create: create idea, conceptual. make: realise

;;; Code:

(require 'ht) ;§maybe start with alist not to have dependancy
(require 'omni-log-buffer)

(defun l-message-no-log (message) ; ¤maybe: rest version (would have to splat it)
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

(defvar l-log-index (ht) ; §maybe create Message equivalent? ¤maybe: alist
  "Logger hash containing associating between name and logger.")

;;; ¤idea: keyword to signal intensity-> l-log-/name/ :info "blable"
(defun l-create-log (name &optional filename)
  "Create and return a log with given NAME.

The log is both registered and returned to be eventually asigned to a variable.
An optional FILENAME to which log will be outputed can be provided too.
Warning will be issued if a logger with same NAME already exists."
  ;; §otherParam: filename, saving frequenci, etc.
  ;; §keywordp
  ;; §maybe: create holding var and functions? [maybe at a higher level?]
  (interactive "sName of the log: ")
  ;;§todo: sanitzename?
  ;; §todo: then check no name conflict
  (if (equal nil (l-get-log name)) ;¤hack
      (let ((log (l--make-log name `(filename ,filename))))
        (ht-set! l-log-index name log)
        log)
    (message "A log named %s already exists: %s" name (l-get-log name))))

(defun l-get-log (name)
  "Send back the eventual buffer with specified NAME."
  (ht-get l-log-index name))

(defun l-create-logger (log)
  "Create a function to directly append to LOG the given message.
This function would be named log- followed by logger name"
  ;; §for now unique
  (message "type: %s" (type-of log))
  (if (l-log-p log)
       ;;§todo: check not set!
      (let ((name (concat "log-" (l-log-name log))))
        (if (fboundp (intern name))
            (warn "%s logging function has already been made!" name)
          (l--make-logger log name)))
    (warn "%s is not a log!" log)))

(defmacro l--make-logger (-log fname)
  "Macro to create the logging function attached to a -LOG.

This is not inteded for users."
  ;; §maybe: swap name
  ;; ¤note: beware macro name conflict: var name must be different from the one used in log.
  `(defun ,(intern (eval fname))
     (message) ;§todo:doc
     (interactive)
     (l--append-to-log ',(symbol-value -log) message)
     (l-message-no-log message))) ; ¤note: maybe subst?
;;; ¤note: inlined, without check

;; §maybe? l-log to current. -> set current or latest register?


(defun log (log-or-name message)
  "Log to specified LOG-OR-NAME given MESSAGE .
LOG-OR-NAME is either a log or the name of the existing log"
  ; §maybe: optional log-or-name. would use default if not set
  (if (l-log-p log-or-name)
      (l-message-to-log log-or-name message)
    (let ((log (l-get-log log-or-name)))
      (if log
          (l-message-to-log log message)
        (warn "There is no log of name %s." log-or-name)))))

(defun l-message-to-log (log message)
  "Display MESSAGE to the Echo area and append it the given LOG."
  ;; §later: evaluate message content now. and enable multi format (format style)
  (l--append-to-log (l-check-log log) message)
  (l-message-no-log message) ; ¤note: maybe subst?
  ;;  message ; ¤see if giving message as return value? [latter when evaluation occur inside? &rest]
  )

(defun l--append-to-log (log message)
  "Add MESSAGE to specified LOG."
  ;; ¤note: type checking supposed to be done at a higher level
  (with-current-buffer (l-log-buffer log)
    ;; §maybe: create a with-current-log
    (goto-char (point-max)) ;; ¤note: maybe use some mark if the bottom of the buffer us some text or so
    (let ((inhibit-read-only t))
      (insert message)
      ;; §todo: call to special formater on message: add timestamp (maybe calling function? (if can be retrieved from namespace))
      ;; ¤note: message is supposed to be already formated. (-> color empahsize inside should be already done)
    (insert "\n"))))


;; §todo: mayeb wmessage + qmessage (or t transient)

;; ¤test:
(let ((test-log (l-create-log "ansible")))
  (l-message-to-log test-log (propertize "42" 'face 'font-lock-warning-face))
  (l-create-logger test-log)
  (log-ansible (propertize "Working!" 'face 'font-lock-type-face)))

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
