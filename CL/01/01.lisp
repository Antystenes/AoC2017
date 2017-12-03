#!/usr/bin/sbcl --noinform

(defun checker (x y)
  (let* ((sum  (car x))
         (last (cadr x))
         (newsum (if (eql last y)
                     (+ sum last)
                     sum)))
    (list newsum y)))

(defun calc (line)
  (let* ((inp (map 'list #'digit-char-p line))
         (out (reduce #'checker (cdr inp) :initial-value (list 0 (car inp)))))
    (if (eql (cadr out) (car inp))
        (+ (car out) (car inp))
        (car out))))

(defparameter *input* (read-line (open "/home/Programming/AoC2017/CL/01/input")))

(defun pair-elements (ls)
  (let* ((mid (floor (length ls) 2))
         (ls2 (concatenate 'list (subseq ls mid nil) (subseq ls 0 mid))))
    (mapcar (lambda (x y) (if (eql x y) x 0)) ls ls2)))

(defun calc2 (line)
  (apply #'+ (pair-elements (map 'list #'digit-char-p line))))
