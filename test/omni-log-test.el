
;; ¤> Logger Creation
(ert-deftest omni-log-logger/from-logger ()
  (let ((logger '(logger "dummy-logger")))
    (should (eq logger (omni-log-logger logger)))))


(ert-deftest can-create-logger ()
  (let ((test-logger (omni-log-create-logger "ansible")))
    (should (omni-log-logger-p test-logger))
    (should (equal "ansible" (omni-log-logger-name test-logger)))
    (omni-log-kill-logger test-logger)))

(ert-deftest cannot-recreate-logger ()
  (let ((test-logger (omni-log-create-logger "ansible")))
    (should (equal (omni-log-create-logger "ansible") "A logger named ansible already exists"))
    (omni-log-kill-logger test-logger)))

(ert-deftest can-retrieve-logger-by-name ()
  (let ((test-logger (omni-log-create-logger "ansible")))
    (should (equal (omni-log-logger "ansible") test-logger))
    (omni-log-kill-logger test-logger)))


(ert-deftest can-create-logger-with-properties ()
  (let ((test-logger (omni-log-create-logger "ansible" '((a . "1") (b . 2)))))
    (should (omni-log-logger-p test-logger))
    (should (equal "ansible" (omni-log-logger-name test-logger)))
    (should (equal '((a . "1") (b . 2)) (omni-log-logger-properties test-logger)))
    (omni-log-kill-logger test-logger)))


(ert-deftest can-access-logger-with-properties ()
  (let ((test-logger (omni-log-create-logger "ansible" '((a . "1") (b . 2)))))
    (should (equal '((a . "1") (b . 2)) (omni-log-logger-properties test-logger)))
    (should (equal (omni-log-logger-property test-logger 'a) "1"))
    (omni-log-kill-logger test-logger)))

(ert-deftest can-access-logger-with-properties-with-default ()
  (let ((test-logger (omni-log-create-logger "ansible" '((a . "1") (b . 2)))))
    (should (equal '((a . "1") (b . 2)) (omni-log-logger-properties test-logger)))
    (should (equal (omni-log-logger-property test-logger 'a 2) "1"))
    (should (equal (omni-log-logger-property test-logger 'c 2) 2))
    (omni-log-kill-logger test-logger)))

(ert-deftest can-create-logger-and-function-in-one-call ()
  (let ((test-logger (omni-log-create "ansible" '((a . "1") (b . 2)))))
    (should (equal '((a . "1") (b . 2)) (omni-log-logger-properties test-logger)))
    (should (fboundp 'log-ansible))
    (omni-log-kill-logger test-logger)))

(ert-deftest modify-and-set-logger-property ()
  (let ((test-logger (omni-log-create-logger "ansible" '((a . "1") (b . 2)))))
    (omni-log-logger-set-property test-logger 'a 1)
    (omni-log-logger-set-property test-logger 'c 3)
    (should (equal '((a . 1) (b . 2) (c . 3)) (omni-log-logger-properties test-logger)))
    (omni-log-kill-logger test-logger)))

;; ¤> Kill buffer

;; §todo: kill buffer
;; §todo: kill buffer with archive

(ert-deftest kill-no-logger ()
  (should-error
   (omni-log-kill-logger "toto")
   :type 'wrong-type-argument))

;; ¤> Logging
(ert-deftest log-with-name ()
  (with-logger "ansible"
    (log "ansible" "Yo")
    (should-log test-logger "Yo\n")))

(ert-deftest log-with-logger ()
  (with-logger "ansible"
   (log test-logger "Yolo!")
   (should-log test-logger "Yolo!\n")))

(ert-deftest log-no-log ()
  (should (equal (log "unknown" "Lost Bottle")
                 "Warning (emacs): There is no logger of name unknown.")))

(ert-deftest can-log-to-created-logger ()
  (let ((test-logger (omni-log-create-logger "ansible")))
    (omni-log-message-to-logger test-logger "42")
    (should-log test-logger "42\n")
    (omni-log-kill-logger test-logger)))

(ert-deftest can-create-logger-method ()
  (with-logger "ansible"
    (log-ansible "Working!")
    (should-log test-logger "Working!\n")))

(ert-deftest cannot-recreate-logger-method ()
  (with-logger "ansible"
    (should (equal (omni-log-create-log-function test-logger)
                   "Warning (emacs): log-ansible logging function has already been made!"))))

(ert-deftest can-create-logger-method-with-variable-name ()
  (let ((name "yo"))
    (with-logger name
      (log-yo "Working!")
      (should-log test-logger "Working!\n"))))

(ert-deftest can-log-with-rest-message ()
  (with-logger "ansible"
    (log-ansible "Working %s %d!" "on" 42)
    (should-log test-logger "Working on 42!\n")))

(ert-deftest can-log-with-prompt ()
  (with-logger-opt "prompt" '((prompt . ">>"))
                   (log-prompt "Toto")
                   (should-log test-logger ">> Toto\n")))

(ert-deftest can-log-with-fading ()
  (with-logger-opt "fading" '((fading . t))
    (modify-face 'omni-log-face "red" "blue") ; ¤hack
    (log-fading "Toto")
    (should-log test-logger "Toto\n")))

;; ¤> color utils and fading
(ert-deftest color-gradient-name ()
  (should (equal (omni-log-color-gradient-name "black" "white" 4)
                 '("black" "#333333" "#666666" "#999999" "#cccccc" "white"))))

(ert-deftest-async fading-function (done)
 (modify-face 'omni-log-face "red" "blue")
 (omni-log-quiet-fading-message "Fading" 0.1 0.5)
 (should (equal (face-attribute 'omni-log-fading-face :foreground) "red"))
 (run-at-time 1 nil
   (lambda (callback)
     ;; (should (equal (face-attribute 'omni-log-fading-face :foreground nil t) "green"))
     ;; not working cause current-message don't work
     (funcall callback)) done))
