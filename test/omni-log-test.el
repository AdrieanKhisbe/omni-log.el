(ert-deftest omni-log-logger/from-logger()
  (let ((logger '(logger "dummy-logger")))
    (should (eq logger (omni-log-logger logger)))))
;;

(ert-deftest can-create-logger()
  (let ((test-logger (omni-log-create-logger "ansible")))
    (should (omni-log-logger-p test-logger))
    (should (equal "ansible" (omni-log-logger-name test-logger)))
    (omni-log-kill-logger test-logger)))

(ert-deftest can-create-logger-with-properties()
  (let ((test-logger (omni-log-create-logger "ansible" '((a . "1") (b . 2)))))
    (should (omni-log-logger-p test-logger))
    (should (equal "ansible" (omni-log-logger-name test-logger)))
    (should (equal '((a . "1") (b . 2)) (omni-log-logger-properties test-logger)))
    (omni-log-kill-logger test-logger)))


(ert-deftest can-access-logger-with-properties()
  (let ((test-logger (omni-log-create-logger "ansible" '((a . "1") (b . 2)))))
    (should (equal '((a . "1") (b . 2)) (omni-log-logger-properties test-logger)))
    (should (equal (omni-log-logger-property test-logger 'a) "1"))
    (omni-log-kill-logger test-logger)))

(ert-deftest can-log-to-created-logger()
  (with-logger "ansible"
    (omni-log-message-to-logger test-logger "42")
    (with-current-buffer (omni-log-logger-buffer test-logger)
      (should (equal (buffer-string) "42\n")))))

(ert-deftest can-create-logger-method()
  (with-logger "ansible"
    (omni-log-create-log-function test-logger)
    (log-ansible "Working!")
    (with-current-buffer (omni-log-logger-buffer test-logger)
      (should (equal (buffer-string) "Working!\n")))))

(ert-deftest can-create-logger-method-with-variable-name()
  (let ((name "yo"))
    (with-logger name
      (omni-log-create-log-function test-logger)
      (log-yo "Working!")
      (with-current-buffer (omni-log-logger-buffer test-logger)
        (should (equal (buffer-string) "Working!\n"))))))
