(ql:quickload :cl-ppcre)


(defparameter *input*
  (with-open-file (stream "/home/Programming/AoC2017/CL/02/input")
    (loop for line = (read-line stream nil)
       while line collect line)))

(defun calc-line (line)
  (let ((nums (mapcar #'parse-integer (cl-ppcre:split "	" line))))
    (- (apply #'max nums) (apply #'min nums))))

(defun calc1 (in)
  (apply #'+ (mapcar #'calc-line in)))

(defun check-div (x l)
  (flet ((checker (n)
           (if (and (eql 0 (mod n x))
                    (not (eql x n)))
               (floor n x)
               0)))
   (mapcar #'checker l)))

(defun calc-line2 (line)
  (let ((nums (mapcar #'parse-integer (cl-ppcre:split "	" line))))
    (apply #'+ (mapcar (lambda (x) (apply #'+ (check-div x nums))) nums))))

(defun calc2 (in)
  (apply #'+ (mapcar #'calc-line2 in)))
