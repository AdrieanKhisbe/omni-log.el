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

(defmacro with-log (name &rest body)
  "Evaluate BODY with a log named NAME at disposition."
  `(let ((test-log (l-create-log ,(eval name))))
     ,@body
     (l-kill-log test-log)))

(require 'ert)
(require 's)
(require 'dash)
(require 'omni-log-buffer (f-expand "omni-log-buffer" omni-log-root-path))
(require 'omni-log (f-expand "omni-log" omni-log-root-path))

(provide 'test-helper)
;;; test-helper.el ends here
