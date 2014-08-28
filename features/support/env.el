(require 'f)

(defvar omni-log-support-path
  (f-dirname load-file-name))

(defvar omni-log-features-path
  (f-parent omni-log-support-path))

(defvar omni-log-root-path
  (f-parent omni-log-features-path))

(add-to-list 'load-path omni-log-root-path)

(require 'omni-log)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
