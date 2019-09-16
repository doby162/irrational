(defvar *test-number* 999999999)

(defun next-digit (seed)
  (let ((chopped (floor seed 100))(i 0)); reduce the numbers digit count by two
    (format t "~a~%" chopped)
    (loop while (<= (* i i) chopped) ; we want to find the largest real number with a square that is less than the seed
          do (incf i))
    (- i 1))) ; we increment after we check, so we added one two many times
