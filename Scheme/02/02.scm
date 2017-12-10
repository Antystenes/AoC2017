(import (srfi :1))
(use-modules (ice-9 rdelim))

(define file-contents
  (call-with-input-file "/home/Programming/AoC2017/CL/02/input"
    read-lines))

(define (read-lines p)
  (let ((line (read-line p)))
    (if (eof-object? line)
        '()
        (cons line (read-lines p)))))

(define (split sep line)
  (define (splitter ch acc)
    (if (char=? ch sep)
        (cons "" acc)
        (let ((str  (car acc))
              (ls   (cdr acc))
              (head (make-string 1 ch)))
          (cons (string-append head str) ls))))
  (let ((char-list (string->list line)))
    (fold-right tab-splitter '("") char-list)))

(define data
  (map (lambda (x) (map string->number (split #\tab x))) file-contents))

(define (task1 data)
  (define (process-line l)
    (let ((maximum (apply max l))
          (minimum (apply min l)))
      (- maximum minimum)))
  (reduce + 0 (map process-line data)))

(define (task2 data)
  (define (process-number x l)
    (reduce + 0 (map (lambda (y) (if (and (= (modulo x y) 0) (not (= x y))) (/ x y) 0)) l)))
  (define (process-line l)
    (reduce + 0 (map (lambda (x) (process-number x l)) l)))
  (reduce + 0 (map process-line data)))
