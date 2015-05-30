(ert-deftest l-logger/from-logger()
  (let ((logger '(logger "dummy-logger")))
    (should (eq logger (l-logger logger)))))
;;

(ert-deftest can-create-logger()
  (let ((test-logger (l-create-logger "ansible")))
    (should (l-logger-p test-logger))
    (should (equal "ansible" (l-logger-name test-logger)))
    (l-kill-logger test-logger)))

;; §todo: log with properties

(ert-deftest can-log-to-created-logger()
  (with-logger "ansible"
    (l-message-to-logger test-logger "42")
    (with-current-buffer (l-logger-buffer test-logger)
      (should (equal (buffer-string) "42\n")))))

(ert-deftest can-create-logger-method()
  (with-logger "ansible"
    (l-create-log-function test-logger)
    (log-ansible "Working!")
    (with-current-buffer (l-logger-buffer test-logger)
      (should (equal (buffer-string) "Working!\n")))))
