(defpackage #:irrational
  (:use :cl)
  (:export :find-huge);
  (:export :multi-find);
  (:export :irrational-search);
  (:export :test));
(in-package :irrational);
(defvar *corenum* 0)
(defvar *a* "")
;(time (setf *a* (arbitrary-base (newt-find 2 100) 26)))
;todo: add the notion of arbitrary base to all function, make the arbitrary base function accept 
;a vector of octets, implement caching
(defun newt-find (n precision base &optional (guess 1))
  (let ((x guess) (past 1))
    (setf precision (- precision (digit-length n)))
    (dotimes (i precision)
      (setf n (* n (* base base)))
      (loop
	    (setf x (floor (+ x (floor n x)) 2)) (when (= past x) (return "x")) (setf past x)))
    (return-from newt-find x)))

(defun digit-engine (n precision base) (arbitrary-base (newt-find n precision base) base))

(defun irrational-search (word) (search word *a*))

(defun spawn-thread (digits root)
  (setf *corenum* (+ *corenum* 1))
  (bordeaux-threads:make-thread (lambda() (run digits root)   (setf *corenum* (- *corenum* 1))    )))

(defun multi-find (test-word &optional (root 2)) 
  (setf *a* "")
  (let ((digits 1))
  (loop
    (when (search test-word *a*) (return digits))
    (bordeaux-threads:thread-yield)
    (when (< *corenum* 2) (spawn-thread digits root) (setf digits (* digits 2))))))

(defun run (digits root) (setf *a* (arbitrary-base (arbitrary-root root digits) 26)))

(defun find-huge (test-word &optional (root 2))
  (let ((digits 1))
  (loop
    (run digits root)
    (when (search test-word *a*) (return digits))
    (setf digits (* digits 2)))))

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
    (reverse 
      (loop
      (when (= n 0) (return ret))
      (setf ret (concatenate 'string ret (nth (mod n b) char-table)))
      (setf n (floor n b))))))

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
    (equalp (arbitrary-base 8 2) "baaa")) 
