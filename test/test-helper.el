;; test-helper - for omni-log

;;; Code:

(require 'f)

(defvar omni-log-path
  (f-parent (f-this-file)))

(defvar omni-log-test-path
  (f-dirname (f-this-file)))

(defvar omni-log-root-path
  (f-parent omni-log-test-path))

(defvar omni-log-sandbox-path
  (f-expand "sandbox" omni-log-test-path))

(when (f-exists? omni-log-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" omni-log-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory omni-log-sandbox-path))
     (when (f-exists? omni-log-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir omni-log-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(defmacro with-logger (name &rest body)
  "Evaluate BODY with a log named NAME at disposition."
  `(let ((test-logger (omni-log-create ,(eval name))))
     ,@body
     (omni-log-kill-logger test-logger)))

(defmacro with-logger-opt (name options &rest body)
  "Evaluate BODY with a log named NAME at disposition."
  `(let ((test-logger (omni-log-create ,(eval name) ,options)))
     ,@body
     (omni-log-kill-logger test-logger)))

(defun should-log (logger message)
  (with-current-buffer (omni-log-logger-buffer logger)
    (should (equal (buffer-string) message))))

(require 'undercover)
(undercover "*.el" "omni-log/*.el"
            (:exclude "*-test.el")
            (:send-report nil)
            (:report-file "/tmp/undercover-report.json"))
(require 'ert)
(require 'ert-async)
(require 'omni-log-logger)
(require 'omni-log)

;;; test-helper.el ends here
