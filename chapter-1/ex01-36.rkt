#lang eopl

(require test-engine/racket-tests)

(check-expect (number-elements '(a b c))
              '((0 a) (1 b) (2 c)))

(define number-elements
  (lambda (lst)
    (if (null? lst) '()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))


(define g
  (lambda (first remain)
    (if (null? remain)
        (list first)
        (cons first
              (map (lambda (x)
                     (list (+ 1 (car x))
                           (cadr x))) remain)))))


(test)