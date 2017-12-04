(import (srfi :1))
(use-modules (ice-9 rdelim))

(define (checker y x)
  (let* ((sum  (car x))
         (last (cadr x))
         (newsum (if (= last y)
                     (+ sum last)
                     sum)))
    (list newsum y)))

(define (char-convert chr) (string->number (string chr)))

(define (calc line)
  (let* ((inp (map char-convert (string->list line)))
         (out (fold checker (list 0 (car inp)) (cdr inp))))
    (if (= (cadr out) (car inp))
        (+ (car out) (car inp))
        (car out))))

(define file-contents
  (call-with-input-file "/home/Programming/AoC2017/CL/01/input"
    read-line))

(define (pair-elements inputlist)
  (let* ((mid (floor (/ (length inputlist) 2)))
         (li2 (call-with-values (lambda () (split-at inputlist mid))
                (lambda (x y) (concatenate (list y x))))))
    (map (lambda (x y) (if (= x y) x 0)) inputlist li2)))

(define (calc2 line)
  (reduce + 0 (pair-elements (map char-convert (string->list line)))))
