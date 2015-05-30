(ert-deftest l-log/from-log()
  (let ((log '(log "dummy-log")))
    (should (eq log (l-log log)))))
;;

(ert-deftest can-create-log()
  (let ((test-log (l-create-log "ansible")))
    (should (l-log-p test-log))
    (should (equal "ansible" (l-log-name test-log)))
    (l-kill-log test-log)))

;; §todo: log with properties

(ert-deftest can-log-to-created-log()
  (let ((test-log (l-create-log "ansible")))
    (l-message-to-log test-log "42")
    (with-current-buffer (l-log-buffer test-log)
      (should (equal (buffer-string) "42\n"))
      (l-kill-log test-log))))

;; (ert-deftest can-create-logger-method()
;;   (let ((test-log (l-create-log "ansible")))
;;     (l-message-to-log test-log (propertize "42" 'face 'font-lock-warning-face))
;;     (l-create-logger test-log)
;;     (log-ansible (propertize "Working!" 'face 'font-lock-type-face))))

;; (ert-deftest can-create-log()
;;   (let ((test-log (l-create-log "ansible")))
;;     (l-message-to-log test-log (propertize "42" 'face 'font-lock-warning-face))
;;     (l-create-logger test-log)
;;     (log-ansible (propertize "Working!" 'face 'font-lock-type-face))))
