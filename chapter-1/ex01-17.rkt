#lang eopl

(require test-engine/racket-tests)

(check-expect (down '()) '())
(check-expect (down '(1 2 3)) '((1) (2) (3)))
(check-expect (down '(a (more (complicate)) object))
              '((a) ((more (complicate))) (object)))

(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (car lst))
              (down (cdr lst))))))


(test)