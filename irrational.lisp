(let ((n 871) (x 1))
(dotimes (k 100)
(setf x (* 0.5 (+ x (/ n x)))))
(format t "~A" x))




;(let ((digits 10000) (result 1) (remainder 1))
;  (dotimes (i digits)
;    (* remainder 100)
;    (let ((j 0))
;      (loop
;        (when (and (<= (* j (+ j (* 20 result))) remainder) (> (* (+ 20 result j 1)  (+ j 1)) remainder))
;          (break))
;        (setf j (+ j 1))
;        (setf remainder (- remainder (
;((20 * result + (j + 1)) * (j + 1)) > remainder:

;(20 * result + (j + 1))
