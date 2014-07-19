(Then "^current column should be \\([[:digit:]]+\\)"
      (lambda (col)
        (let ((actual (current-column))
              (expected (string-to-number col)))
          (should (= actual expected)))))

(When "^I call jsonconfig-create-imenu-index"
      (lambda ()
        (setq jsonconfig-test-imenu-output (jsonconfig-create-imenu-index))))

(Then "^imenu entries are\\(?: \"\\(.*\\)\"\\|:\\)$"
      (lambda (expected)
        (should (equal jsonconfig-test-imenu-output (read expected)))))

(Then "^imenu entry none"
      (lambda ()
        (should (not jsonconfig-test-imenu-output))))
