(Then "^current column should be \\([[:digit:]]+\\)"
      (lambda (col)
        (let ((actual (current-column))
              (expected (string-to-number col)))
          (should (= actual expected)))))
