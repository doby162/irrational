;(let ((n 871) (x 1))
;(dotimes (k 90)
;(setf x (* 0.5 (+ x (/ n x)))))
;(format t "~100$" x))
;(base64:BASE64-STRING-TO-INTEGER "hello")
;(ql:quickload :cl-base64)
;(setf *a* (string-downcase (base64:integer-to-base64-string (arbitrary-root 2 5000000))))
;(setf *a* (base32:encode-word (arbitrary-root 2 5000000)))
;(setf *a* (arbitrary-base (arbitrary-root 2 5000000) 27))
;(search "quack" *a*)

(defvar *a*)
(defun run (digits) (setf *a* (arbitrary-base (arbitrary-root 2 digits) 26)))
(defvar *huge* 1)
(defun find-huge (test-word)
  (setf *huge* 1)
  (loop
    (format t "Searching for your word in ~a digits~%" *huge*)
    (time (run *huge*))
    (when (search test-word *a*) (return *huge*))
    (setf *huge* (* *huge* 2))))

(defun digit-length (digit)
"A slightly cludgy way to tell the length of a number on the screen"
  (let ((len 0))
    (loop
      (setf len (+ len 1))
      (unless (>= (abs digit) (expt 10 len)) (return len)))))

(defun arbitrary-root ( &optional (n 2) (prec 100))
"Returns the square root of any natural number to arbitrary precision and without the decimal point"
  (isqrt (* n (expt 10 (* 2 (- prec (digit-length (isqrt n))))))))

(defun arbitrary-base (n b &optional (char-table
(list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" " " "0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))
  (let ((ret ""))
    (loop
      (when (= n 0) (return ret))
      (setf ret (concatenate 'string ret (nth (mod n b) char-table)))
      (setf n (floor n b)))))

(defun test ()
"Unit test suit"
  (and
    (= (arbitrary-root) 1414213562373095048801688724209698078569671875376948073176679737990732478462107038850387534327641572);make sure the default behavior is consistent
    (= (arbitrary-root 2 9) 141421356);check accuracy against preexisting numbers
    (= (arbitrary-root 3 8) 17320508))
    (= (* (sqrt 5) 1000000) (round (/ (arbitrary-root 5 8) 10)));check accuracy against sqrt
    (= (* (sqrt 8) 1000000) (round (/ (arbitrary-root 8 8) 10)))
    (= (* (sqrt 80) 1000000) (round (/ (arbitrary-root 80 8) 10)))
    (= (* (sqrt 54321) 1000000) (round (/ (arbitrary-root 54321 8) 10)))
    (= (digit-length 123456789) 9);check digit length for accuracy
    (= (digit-length (arbitrary-root 2 123)) 123);check that arbitrary-root correctly usses the value of digit length
    (= (digit-length (arbitrary-root 123 123)) 123);regardless of the size of n
    (equalp (arbitrary-base 8 2) "aaab")) 
