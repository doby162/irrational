;(let ((n 871) (x 1))
;(dotimes (k 90)
;(setf x (* 0.5 (+ x (/ n x)))))
;(format t "~100$" x))
;(base64:BASE64-STRING-TO-INTEGER "hello")
;(ql:quickload :cl-base64)

(defun code-2 (digits)
  (isqrt (* 2 (expt 10 (* 2 (- digits 2))))))


(defun arbitrary-root ( &optional (n 2) (prec 100))
  (isqrt (* n (expt 10 (* 2 (- prec 2))))))


(defun test ()
  (and
    (= (arbitrary-root) 141421356237309504880168872420969807856967187537694807317667973799073247846210703885038753432764157)
    (= (arbitrary-root 2 10) 141421356)
    (= (arbitrary-root 3 8) 17320508)))
